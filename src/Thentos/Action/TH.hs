{-# LANGUAGE TemplateHaskell #-}

module Thentos.Action.TH (deriveHasDB)
where

import Control.Applicative ((<$>))
import Control.Monad (replicateM)
import Data.Acid (Update, Query, makeAcidic)
import Language.Haskell.TH
import Text.Show.Pretty (ppShow)

import Thentos.Transaction.Core (ThentosUpdate, ThentosQuery)
import Thentos.Types (DB, ThentosError)

import Data.Char (toLower)

-- two things to generate:
    -- 1. the typeclass definition itself
    -- 2. instance definitions

-- returns a ClassD
-- queries and updates are the names of the transaction constructors, e.g.
    -- LookupUser, which are then converted to e.g. lookupUser
makeHasDBTypeClass :: [Name] -> [Name] -> Q [Dec]
makeHasDBTypeClass queries updates = do
    updateDecls <- mapM makeUpdateDecl updates
    queryDecls  <- mapM makeQueryDecl queries
    return [ClassD ctx className [typeVar] [] (updateDecls ++ queryDecls)]
  where
    ctx = ClassP ''AsDB typeVar
    typeVar = PlainTV $ mkName "a"
    className = mkName "HasDB"


-- FIXME: probably need the number of arguments as well
-- returns a FunD
makeUpdateDecl :: Name -> Q Dec
makeUpdateDecl conName = FunD 
  where
    methodName = upperCamelToLowerCamel conName

makeQueryDecl :: Name -> Q Dec
makeQueryDecl = undefined

-- returns an InstanceD
deriveHasDB :: Name -> [Name] -> [Name] -> Q [Dec]
deriveHasDB dbName queries updates = do
    undefined
    -- first generate instance declaration, i.e. 'instance HasDB `dbName` where'


makeQueryMethodDef

upperCamelToLowerCamel :: Name -> Name
upperCamelToLowerCamel = mkName . lowerFirst . baseName
  where
    lowerFirst :: String -> String
    lowerFirst (c:cs) = toLower c : cs
    lowerFirst [] = [] -- should not be necessary, but meh
