{-# LANGUAGE OverloadedStrings #-}
module Ed.Memory where

import           Control.Applicative    (liftA2)
import           Control.Lens           (ix, lengthOf, snoc, (%~), (.~), (^.),
                                         (^?))
import           Control.Monad.IO.Class (MonadIO)

import           System.Timeout         (timeout)

import           Data.Bifoldable        (bifold)
import           Data.Bifunctor         (first, second)
import           Data.Foldable          (traverse_)
import           Data.Function          ((&))

import           Text.Read              (readMaybe)

import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T

import           Hedgehog

import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range

import           Ed.Types

import           Text.Printf            (printf)

formatCmd :: String -> Word -> Text
formatCmd str = T.pack . printf str

genSafeLineNum :: (HasEdModel s v, MonadGen m) => s -> m Word
genSafeLineNum = Gen.word . Range.linear 1 . fromIntegral . lengthOf edBuffer

genTextInp :: MonadGen m => m Text
genTextInp = Gen.text (Range.linear 1 100) Gen.unicode

cAppendText
  :: ( MonadGen n
     , MonadIO m
     , MonadTest m
     )
  => EdProc
  -> Command n m EdModel
cAppendText edProc =
  let
    gen _ = Just $ Cmd_Append <$> genTextInp

    execute (Cmd_Append t) = evalIO $
      traverse_ (edCmd edProc)
        [ "a"
        , t
        , "."
        ]
      *> getBlackBoxState edProc
  in
    Command gen execute
    [ Update $ \ed (Cmd_Append i) _ -> ed
      & edBuffer %~ flip snoc i
      & edAddress .~ if emptyBuffer ed then 1 else bufferLength ed + 1

    , Ensure $ \edOld edNew (Cmd_Append i) bbEd -> do
        let newB = edNew ^. edBuffer
            oldB = edOld ^. edBuffer

        edNew ^? edAddress === bbEd ^. bbEdAddress

        newB === snoc oldB i
        T.unlines newB === bbEd ^. bbEdBuffer
    ]

cPrintAll
  :: ( MonadIO m
     , MonadTest m
     , MonadGen n
     )
  => EdProc
  -> Command n m EdModel
cPrintAll edProc =
  let
    gen _ = Just $ Gen.constant Cmd_PrintAll

    execute _ = evalIO $
      edCmd edProc ",p" *> getBlackBoxState edProc
  in
    Command gen execute
    [ Require $ \ed _ ->
        not $ emptyBuffer ed

    , Update $ \ed _ _ ->
        ed & edAddress .~ bufferLength ed

    , Ensure $ \_ edNew _ bbEd -> do
        bbEd ^. bbEdAddress === edNew ^? edAddress
        bbEd ^. bbEdBuffer === T.unlines (edNew ^. edBuffer)
    ]

cDeleteLine
  :: ( MonadIO m
     , MonadTest m
     , MonadGen n
     )
  => EdProc
  -> Command n m EdModel
cDeleteLine edProc =
  let
    gen = Just . fmap Cmd_DeleteLine . genSafeLineNum

    execute (Cmd_DeleteLine n) = evalIO $
      edCmd edProc (formatCmd "%ud" n) *> getBlackBoxState edProc
  in
    Command gen execute
    [ Require $ \ed (Cmd_DeleteLine ln) ->
        addressInBuffer ed ln

    , Update $ \ed (Cmd_DeleteLine n) _ -> ed
        & edBuffer %~ bifold . first (take (fromIntegral n - 1)) . splitAt (fromIntegral n)
        & edAddress .~ n

    , Ensure $ \oldEd newEd (Cmd_DeleteLine n) bbEd -> do
        let
          newB = newEd ^. edBuffer
          oldB = oldEd ^. edBuffer

          bbEdB = bbEd ^. bbEdBuffer

        T.unlines newB === bbEdB
        newEd ^? edAddress === bbEd ^. bbEdAddress

        let n' = fromIntegral n

        newB ^? ix n'       === oldB ^? ix (n' + 1)
        newB ^? ix (n' - 1) === oldB ^? ix n'
    ]

cAppendAt
  :: ( MonadGen n
     , MonadIO m
     , MonadTest m
     )
  => EdProc
  -> Command n m EdModel
cAppendAt edProc =
  let
    gen b = Just $ Cmd_AppendAt
      <$> genSafeLineNum b
      <*> genTextInp

    execute (Cmd_AppendAt ln i) = evalIO $
      traverse_ (edCmd edProc)
        [ formatCmd "%ua" ln
        , i
        , "."
        ]
      *> getBlackBoxState edProc
  in
    Command gen execute
    [ Require $ \ed (Cmd_AppendAt ln _) ->
        addressInBuffer ed ln

    , Update $ \ed (Cmd_AppendAt ln i) _ -> ed
      & edBuffer %~ bifold . second (i:) . splitAt (fromIntegral ln)
      -- We end on the line following the append
      & edAddress .~ ln + 1

    , Ensure $ \_ edNew (Cmd_AppendAt ln i) bbEd -> do
        let newB = edNew ^. edBuffer
            newA = edNew ^. edAddress

            bbEdB = bbEd ^. bbEdBuffer
            bbEdA = bbEd ^. bbEdAddress

        T.unlines newB === bbEdB

        Just newA === bbEdA
        newB ^? ix (fromIntegral ln) === Just i
    ]

edCmd :: EdProc -> Text -> IO ()
edCmd p c = T.hPutStrLn (_edIn p) c

getCurrentLine :: EdProc -> IO (Maybe Word)
getCurrentLine ed = fmap (readMaybe . T.unpack)
  $ edCmd ed ".=" *> T.hGetLine (_edOut ed)

readEntireBuffer :: EdProc -> IO Text
readEntireBuffer ed = edCmd ed ",p" *> go []
  where
    go acc = (timeout 2500 $ T.hGetLine (_edOut ed))
      >>= maybe (pure . T.unlines . reverse $ acc) (go . (:acc))

-- Must get address before reading the entire buffer as reading the buffer will
-- change the address.
getBlackBoxState :: EdProc -> IO BBEd
getBlackBoxState ed = liftA2 BBEd (getCurrentLine ed) (readEntireBuffer ed)

prop_ed_blackbox_memory :: EdProc -> Property
prop_ed_blackbox_memory edProc = property $ do
  let
    cmds = ($ edProc) <$>
      [ cAppendText
      , cAppendAt
      , cDeleteLine
      ]

    initialState = EdModel mempty 0

  actions <- forAll $ Gen.sequential (Range.linear 1 10) initialState cmds

  -- Reset the ed buffer
  evalIO $ do
    edCmd edProc "a"
    edCmd edProc "avoiding invalid address error"
    edCmd edProc "."
    edCmd edProc ",d"

  executeSequential initialState actions
