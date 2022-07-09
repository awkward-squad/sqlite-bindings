{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (bracket)
import qualified Data.ByteString as ByteString
import Data.Functor
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Foreign.C.String (CString)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import Sqlite.Bindings
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain do
    testCase "sqlite3_open" do
      bracket (check (open ":memory:")) (check . close) \_ -> pure ()
      bracket (check (open "")) (check . close) \_ -> pure ()

--

close :: Ptr Sqlite3 -> IO (Either Text ())
close conn = do
  code <- sqlite3_close conn
  inspect conn code ()

errmsg :: Ptr Sqlite3 -> IO Text
errmsg conn = do
  c_msg <- sqlite3_errmsg conn
  cstringToText c_msg

open :: Text -> IO (Either Text (Ptr Sqlite3))
open name = do
  (code, conn) <-
    ByteString.useAsCString (Text.encodeUtf8 name) \c_name ->
      alloca \connPtr -> do
        code <- sqlite3_open c_name connPtr
        conn <- peek connPtr
        pure (code, conn)
  result <- inspect conn code conn
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

inspect :: Ptr Sqlite3 -> CInt -> a -> IO (Either Text a)
inspect conn code result =
  if code == _SQLITE_OK
    then pure (Right result)
    else do
      msg <- errmsg conn
      pure (Left msg)

--

cstringToText :: CString -> IO Text
cstringToText string =
  Text.decodeUtf8 <$> ByteString.packCString string

textToCString :: Text -> (CString -> IO a) -> IO a
textToCString =
  ByteString.useAsCString . Text.encodeUtf8
