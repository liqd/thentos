{-# LANGUAGE TemplateHaskell #-}

module Thentos.Action.TH (makeHasDBTypeClass, makeHasDBInstance, lookupUserType)
where

import Control.Applicative ((<$>))
import Control.Monad (replicateM)
import Data.Acid (Update, Query, makeAcidic)
import Language.Haskell.TH
import Text.Show.Pretty (ppShow)

import Thentos.Transaction.Core (ThentosUpdate, ThentosQuery)
import Thentos.Types (DB, AsDB, ThentosError)

import Data.Char (toLower)

import Thentos.Action.Core (query'P, update'P, Action)
import Thentos.Types

-- FIXME: just for testing, remove
lookupUserType :: Type
lookupUserType = AppT (AppT ArrowT (ConT ''UserId))
                      (AppT (ConT ''Action)
                            (VarT $ mkName "db")  `AppT` (TupleT 2 `AppT` ConT ''UserId `AppT` ConT ''User))

--import Thentos.Action.TransactionWrappers (HasDB)

-- two things to generate:
    -- 1. the typeclass definition itself
    -- 2. instance definitions

-- * typeclass definition

-- returns a ClassD
-- queries and updates are the names of the transaction constructors, e.g.
    -- LookupUser, which are then converted to e.g. lookupUser
makeHasDBTypeClass :: [(Name, Type)] -> [(Name, Type)] -> Q [Dec]
makeHasDBTypeClass queries updates = do
    updateDecls <- mapM makeUpdateDecl updates
    queryDecls  <- mapM makeQueryDecl queries
    return [ClassD ctx className [PlainTV typeVarName] [] (updateDecls ++ queryDecls)]
  where
    ctx :: Cxt
    ctx = [ClassP ''AsDB [VarT typeVarName]]

    typeVarName :: Name
    typeVarName = mkName "db"

    className = mkName "HasDB"

-- returns a SigD
makeUpdateDecl :: (Name, Type) -> Q Dec 
makeUpdateDecl (conName, typ) = return $ SigD methodName typ
  where 
    methodName = upperCamelToLowerCamel conName

-- FIXME: this is identical to makeUpdateDecl
makeQueryDecl :: (Name, Type) -> Q Dec
makeQueryDecl (conName, typ) = return $ SigD methodName typ
  where 
    methodName = upperCamelToLowerCamel conName




-- * instance definitions

makeHasDBInstance :: Name -> [(Name, Type)] -> [(Name, Type)] -> Q [Dec]
makeHasDBInstance dbType queries updates = do
    queryDefs <- mapM (makeQueryDef . fst) queries
    updateDefs <- mapM (makeUpdateDef . fst) updates
    --return $ [InstanceD [] (ConT $ mkName "HasDB") (queryDefs ++ updateDefs)]
    return $ [InstanceD [] (AppT (ConT $ mkName "HasDB") (ConT dbType)) (queryDefs ++ updateDefs)]


makeUpdateDef :: Name -> Q Dec
makeUpdateDef conName = do
    argCount <- transactionArgCount conName
    args <- replicateM argCount (newName "arg")
    let methodName = upperCamelToLowerCamel conName
        methodBody = NormalB $ AppE (VarE 'update'P) $ applyConstructorToAll (ConE conName) (map VarE args)
        clause = Clause (map VarP args) methodBody []
    return $ FunD methodName [clause]

makeQueryDef :: Name -> Q Dec
makeQueryDef conName = do
    argCount <- transactionArgCount conName
    args <- replicateM argCount (newName "arg")
    let methodName = upperCamelToLowerCamel conName
        --methodBody = NormalB $ AppE (ConE conName) (map VarE args)
        methodBody = NormalB $ AppE (VarE 'query'P) $ applyConstructorToAll (ConE conName) (map VarE args)
        clause = Clause (map VarP args) methodBody []
    return $ FunD methodName [clause]

-- TODO deduplicate makeUpdateDecl and makeQueryDecl


-- * util

upperCamelToLowerCamel :: Name -> Name
upperCamelToLowerCamel = mkName . lowerFirst . nameBase
  where
    lowerFirst :: String -> String
    lowerFirst (c:cs) = toLower c : cs
    lowerFirst [] = [] -- should not be necessary, but meh


transactionArgCount :: Name -> Q Int
transactionArgCount transName = do
    info <- reify transName
    case info of
        DataConI _ t _ _ -> return $ countArgs t
        other -> error $ "oh noes: " ++ show other


-- FIXME: copied from Transaction.TH
-- | Count the number of arguments in a function type
countArgs :: Type -> Int
countArgs (ForallT _ _ app) = countArgs app
countArgs (AppT (AppT ArrowT _arg) returnType) = 1 + countArgs returnType
countArgs _ = 0


applyConstructorToAll :: Exp -> [Exp] -> Exp
applyConstructorToAll c@(ConE _) args = foldl AppE c args
applyConstructorToAll _ _ = error "applyConstructorToAll: first arg has to be a constructor expression"


{-
makeActionType :: Name -> Type -> (Type, ThentosTransactionType)
makeActionType dbType (AppT (AppT ArrowT arg) returnType) =
    let (rightOfArrow, transType) = makeThentosType dbType returnType
    in (AppT (AppT ArrowT arg) rightOfArrow, transType)
makeThentosType dbType (AppT (AppT t (VarT _)) returnType)
    | t == ConT (''ThentosUpdate) = (updateType, ThentosU)
    | t == ConT (''ThentosQuery) = (queryType, ThentosQ)
    | otherwise = error $ "not a thentos transaction type:" ++ ppprint t
  where
    updateType :: Type
    updateType = makeAcidStateType ''Update

    queryType :: Type
    queryType = makeAcidStateType ''Query

    makeAcidStateType :: Name -> Type
    makeAcidStateType acidStateTypeConstructor =
        AppT (AppT (ConT acidStateTypeConstructor) (ConT dbType)) (AppT (AppT (ConT ''Either) (ConT ''ThentosError)) returnType)
makeThentosType dbType (ForallT _ _ app) = makeThentosType dbType app
makeThentosType _ t = error $ "not a thentos transaction type: " ++ ppprint t
-}
