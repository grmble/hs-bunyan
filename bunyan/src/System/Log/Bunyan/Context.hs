{- | Standard context manipulations.

https://github.com/trentm/node-bunyan#recommendedbest-practice-fields
-}
module System.Log.Bunyan.Context where

import UnliftIO.Exception (SomeException(..))
import Type.Reflection (typeOf)
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as M
import qualified Data.List as L

-- | Decorate the context with a request ID
requestID :: A.ToJSON a => a -> A.Object -> A.Object
requestID = M.insert "req_id" . A.toJSON

-- | Decorate the context with exception information
--
-- The stack/msg guessing works with UserException, lets
-- see some other examples
someException :: SomeException -> A.Object -> A.Object
someException (SomeException e) =
  M.insert "name" (showValue (typeOf e)) .
  M.insert "message" (A.toJSON msg) .
  M.insert "stack" (A.toJSON stack)
  where
    _lines = lines (show e)
    msg = L.intercalate ":" $ take 3 _lines
    stack = L.intercalate "\n" $ drop 3 _lines

-- | Helper to create an Aeson value via show
showValue :: Show x => x -> A.Value
showValue = A.toJSON . show
