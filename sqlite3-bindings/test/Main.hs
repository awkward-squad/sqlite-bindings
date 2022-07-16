module Main where

import Control.Exception (bracket)
import Data.Functor
import Data.Text (Text)
import qualified Data.Text as Text
import Foreign.C.Types (CInt)
import Sqlite3.Bindings
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain do
    testCase "sqlite3_open" do
      bracket (check (open ":memory:")) (check . close) \_ -> pure ()
      bracket (check (open "")) (check . close) \_ -> pure ()

--

close :: Sqlite3 -> IO (Either Text ())
close conn = do
  code <- sqlite3_close conn
  pure (inspect code ())

open :: Text -> IO (Either Text Sqlite3)
open name = do
  (maybeConn, code) <- sqlite3_open name
  case maybeConn of
    Nothing -> pure (inspect code undefined)
    Just conn -> do
      let result = inspect code conn
      case result of
        Left _ -> void (close conn)
        Right _ -> pure ()
      pure result

--

check :: IO (Either Text a) -> IO a
check action =
  action >>= \case
    Left msg -> assertFailure (Text.unpack msg)
    Right result -> pure result

inspect :: CInt -> a -> Either Text a
inspect code result =
  if code == _SQLITE_OK
    then Right result
    else Left (sqlite3_errstr code)
