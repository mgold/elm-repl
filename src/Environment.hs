{-# LANGUAGE OverloadedStrings #-}
module Environment where

import Control.Monad.RWS (RWST, runRWST)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.List (isSubsequenceOf, words, unwords)
import Data.Monoid ((<>))
import Data.Trie (Trie) -- TODO: Switch to a Char-based trie.
import qualified Data.Trie as Trie


-- TASKS

type Task =
    RWST () () Env IO


run :: Env -> Task a -> IO a
run env task =
    do  (x,_,_) <- runRWST task () env
        return x


-- USER INPUT

data Input
    = Meta Config
    | Code (Maybe DefName, String)
    | Skip
    deriving (Show, Eq)


data Config
    = AddFlag String
    | RemoveFlag String
    | ListFlags
    | ClearFlags
      -- Just if this was triggered by an error
    | InfoFlags (Maybe String)
    | Help (Maybe String)
    | Exit
    | Reset
    deriving (Show, Eq)


data DefName
    = VarDef  String
    | DataDef String
    | Import  String
    deriving (Show, Eq)


-- ENVIRONMENT

data Env = Env
    { compilerPath  :: FilePath
    , interpreterPath :: FilePath
    , flags :: [String]
    , imports :: Trie String
    , adts :: Trie String
    , defs :: Trie String
    , ansN :: Integer
    }
    deriving Show


empty :: FilePath -> FilePath -> Env
empty compiler interpreter =
    Env compiler
        interpreter
        []
        Trie.empty
        Trie.empty
        (Trie.singleton firstVar (BS.unpack firstVar <> " = ()"))
        0


firstVar :: ByteString
firstVar =
    "tsol"


lastVar :: ByteString
lastVar =
    "ans"


lastVarString :: String
lastVarString =
    BS.unpack lastVar


toElmCode :: Env -> String
toElmCode env =
    unlines $ "module Repl where" : decls
  where
    decls =
        concatMap Trie.elems [ imports env, adts env, defs env ]


insert :: (Maybe DefName, String) -> Env -> Env
insert (maybeName, body) env =
    case maybeName of
      Nothing ->
          display body env

      Just (Import name) ->
          noDisplay $ env
              { imports = Trie.insert (BS.pack name) body (imports env)
              }

      Just (DataDef name) ->
          noDisplay $ env
              { adts = Trie.insert (BS.pack name) body (adts env)
              }

      Just (VarDef name) ->
          define (BS.pack name) (replaceAns env body) (display name env)


defineAns :: String -> Env -> Env
defineAns body env =
    env { defs = Trie.insert lastVar (formatDefinition lastVarString oldAnsString) $
                 Trie.insert oldAns  (formatDefinition oldAnsString body) $
                 defs env
        , ansN = succ (ansN env)
        }
  where
    oldAnsString = mkOldAnsString (ansN env)
    oldAns = BS.pack oldAnsString


define :: ByteString -> String -> Env -> Env
define name body env =
    env { defs = Trie.insert name body (defs env) }
  where
    formattedBody = formatDefinition (BS.unpack name) body


mkOldAnsString n =
  "replOldAns" ++ show n


display :: String -> Env -> Env
display body env =
  if lastVarString == body then
    env
  else if lastVarString `isSubsequenceOf` body then
    defineAns (replaceAns env body) env
  else
    defineAns (body) env


replaceAns env s =
    unwords $ map (\w -> if w == lastVarString then oldAnsString else w) $ words s
  where
    oldAnsString =
      mkOldAnsString $ pred (ansN env)


formatDefinition :: String -> String -> String
formatDefinition name body =
    name ++ " =" ++ concatMap ("\n  "++) (lines body)


noDisplay :: Env -> Env
noDisplay env =
    env { defs = Trie.delete lastVar (defs env) }
