{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}
{-# LANGUAGE MultiWayIf  #-}

module Thentos.Transaction.TH
     ( makeThentosAcidicPhase1
     , makeThentosAcidicPhase2
     )
where

import Control.Applicative (pure, (<$>), (<*>))
import Control.Monad (replicateM)
import Data.Acid (Update, Query)
import Language.Haskell.TH
import Text.Show.Pretty (ppShow)

import Thentos.Transaction.Core (ThentosUpdate, ThentosQuery)
import Thentos.Types (DB, ThentosError, Extends)


import Data.Char (toUpper)
import Data.Data (Typeable)
import Data.Acid (QueryEvent, UpdateEvent)
import Data.Acid.Advanced (Event(QueryEvent, UpdateEvent), Method, MethodResult, MethodState)
import Data.SafeCopy (deriveSafeCopy, base, SafeCopy(putCopy, getCopy))


data ThentosTransactionType = ThentosQ | ThentosU

makeThentosAcidicPhase1 :: [Name] -> Q [Dec]
makeThentosAcidicPhase1 names = concat <$> mapM processTransaction names
{-
makeThentosAcidicPhase2 :: Name -> [Name] -> Q [Dec]
makeThentosAcidicPhase2 stateName eventNames =
    makeAcidic stateName $ map dropPrefix eventNames
-}

{-
-- for every transaction, we need the following:

data AgentRoles t = AgentRoles Agent
    deriving Typeable

$(deriveSafeCopy 0 'base ''AgentRoles)

instance (db `Extends` DB) => Method (AgentRoles db) where
    type MethodResult (AgentRoles db) = Either (ThentosError db) (Set.Set Role)
    type MethodState (AgentRoles db) = db

instance (db `Extends` DB) => QueryEvent (AgentRoles db)
    -- or UpdateEvent, as the case may be

-- and then a list of all events/transactions:
dbEvents :: (db `Extends` DB) => [Event db]
dbEvents =
    [ QueryEvent (\(AgentRoles a) -> agentRoles a)
    ..
    ]

instance IsAcidic DB where
    acidEvents = dbEvents

-- the TH should probably take the name of the base db type as an argument and
-- substitute that in all the places that say "DB" above
-}
makeThentosAcidicPhase2 :: Name -> [Name] -> Q [Dec]
makeThentosAcidicPhase2 stateName transNames = do
    --error "i seem to get here (0)"
    decs <- concat <$> mapM (acidifyTrans stateName) eventNames
    eventList <- mkEventList stateName eventNames
    isAcidic <- makeIsAcidic stateName
    return $ decs ++ eventList ++ [isAcidic]
  where
    eventNames = map dropPrefix transNames

makeIsAcidic :: Name -> Q Dec
makeIsAcidic dbName = do
    return $ InstanceD [] (AppT (ConT $ mkName "IsAcidic") (ConT dbName)) [ValD (VarP $ mkName "acidEvents") (NormalB $ VarE $ mkName "dbEvents") []]

mkEventList :: Name -> [Name] -> Q [Dec]
mkEventList dbName eventNames = do
    sig <- mkSig
    def <- mkDef
    return [sig, def]
  where
    mkSig = do
        dbTypeVar <- newName "db"
        let ctx = [ClassP ''Extends [VarT dbTypeVar, ConT dbName]]
            typ = SigD (mkName "dbEvents") $ ForallT [PlainTV dbTypeVar] ctx (AppT ListT (AppT (ConT ''Event) (VarT dbTypeVar)))
        return typ

    mkDef = do
        l <- ListE <$> mapM mkEntryForEvent eventNames
        return $ ValD (VarP $ mkName "dbEvents") (NormalB l) []

    mkEntryForEvent :: Name -> Q Exp
    mkEntryForEvent eventName = do
        (argTypes, evType, _) <- analyseEventFunc eventName
        argNames <- mapM (\_ -> newName "arg") argTypes
        let evConst = ConE $ case evType of
                TransQuery -> 'QueryEvent
                TransUpdate -> 'UpdateEvent
            constName = toTypeName eventName

            lambdaBody = foldl AppE (VarE eventName) (map VarE argNames)
            lambda = LamE [ConP constName (map VarP argNames)] lambdaBody
        return $ AppE evConst lambda

acidifyTrans :: Name -> Name -> Q [Dec]
acidifyTrans dbName eventName = do -- eventName e.g. agentRoles (NOT AgentRoles)
    -- data AgentRoles t = AgentRoles Agent deriving Typeable
    (argTypes, transType, returnType) <- analyseEventFunc eventName
    typDecl <- mkTypeDecl argTypes
    -- TODO: can this be in the same splice as the type declaration?
    -- sc <- deriveSafeCopy 0 'base acidTypeName
    sc <- mkSafeCopyInstance (length argTypes)

    methodInstance <- mkMethodInstance returnType
    eventInstance <- mkEventInstance transType
    -- return $ [typDecl] ++ sc ++ [methodInstance, eventInstance]
    return [typDecl, sc, methodInstance, eventInstance]
  where
    acidTypeName = toTypeName eventName -- e.g. AgentRoles

    mkTypeDecl :: [Type] -> Q Dec
    mkTypeDecl args = do
        let conArgs = map (\a -> (NotStrict, a)) args
            constructor = NormalC acidTypeName conArgs
            derive = [''Typeable]
        -- TODO: should probably make new name instead of mkName "t"
        return $ DataD [] acidTypeName [PlainTV $ mkName "t"] [constructor] derive

    mkMethodInstance :: Type -> Q Dec
    mkMethodInstance returnType = do
        dbTypeVar <- VarT <$> newName "db"
        let ctx = [ClassP ''Extends [dbTypeVar, ConT dbName]]
            fullType = AppT (ConT ''Method) (AppT (ConT acidTypeName) dbTypeVar)
        rDecl <- mkResultDecl dbTypeVar returnType
        sDecl <- mkStateDecl dbTypeVar
        return $ InstanceD ctx fullType [rDecl, sDecl]
        
    -- type MethodResult (AgentRoles db) = Either (ThentosError db) (Set.Set Role)
    mkResultDecl :: Type -> Type -> Q Dec
    mkResultDecl dbTypeVar eventReturnType = do
        let lhs_type = AppT (ConT acidTypeName) dbTypeVar
            errorType = AppT (ConT ''ThentosError) dbTypeVar
            returnType = AppT (AppT (ConT ''Either) errorType) eventReturnType
        return $ TySynInstD ''MethodResult (TySynEqn [lhs_type] returnType)

    -- type MethodState (AgentRoles db) = db
    mkStateDecl :: Type -> Q Dec
    mkStateDecl dbTypeVar = do
        let lhs_type = AppT (ConT acidTypeName) dbTypeVar
        return $ TySynInstD ''MethodState (TySynEqn [lhs_type] dbTypeVar)

    -- instance (db `Extends` DB) => QueryEvent (AgentRoles db)
        -- (or UpdateEvent, as the case may be)
    mkEventInstance :: TransType -> Q Dec
    mkEventInstance transType = do
        dbTypeVar <- VarT <$> newName "db"
        let evName = case transType of
                TransQuery -> ''QueryEvent
                TransUpdate -> ''UpdateEvent
            ctx = [ClassP ''Extends [dbTypeVar, ConT dbName]]
        return $ InstanceD ctx (AppT (ConT evName) (AppT (ConT acidTypeName) dbTypeVar)) []

    -- instance SafeCopy (MyUpdateEvent db) where
    --    putCopy (MyUpdateEvent a b) = do putCopy a; putCopy b; return ();
    --    getCopy = (pure MyUpdateEvent) <*> getCopy <*> getCopy
    mkSafeCopyInstance :: Int -> Q Dec
    mkSafeCopyInstance argCount = do
        argNames <- replicateM argCount (newName "arg")
        dbTypeVarName <- newName "db"
        let eventTypeName = toTypeName eventName
            putDecl = FunD 'putCopy [Clause [ConP eventTypeName (map VarP argNames)] putBody []]
            putBody = NormalB . DoE $ map (\a -> NoBindS $ AppE (VarE 'putCopy) (VarE a)) argNames ++ [NoBindS $ AppE (VarE 'return) (ConE '())]
            
            getDecl = ValD (VarP 'getCopy) (NormalB getBody) []
            getBody = foldl (\a b -> UInfixE a (VarE '(<*>)) b) (AppE (VarE 'pure) (ConE eventTypeName)) (replicate argCount (VarE 'getCopy))

        --return $ InstanceD [] (AppT (ConT ''SafeCopy) (ConT eventTypeName)) [getDecl, putDecl]
        return $ InstanceD [] (AppT (ConT ''SafeCopy) (AppT (ConT eventTypeName) (VarT dbTypeVarName))) [getDecl, putDecl]
        
toTypeName :: Name -> Name
toTypeName (nameBase -> s) =
    mkName $ case s of
        c : cs -> toUpper c : cs
        []     -> []

-- | Extract a list of arguments from a function type
extractArgs :: Type -> ([Type], Type)
extractArgs (ForallT _ _ app) = extractArgs app
extractArgs (AppT (AppT ArrowT arg) returnType) =
    let rest = extractArgs returnType
    in (arg : fst rest, snd rest)
extractArgs t =  ([]  , t)

-- extracts arguments, return type and Transaction type (i.e. Query | Update)
-- from a given function: example:
-- agentRoles ::
---      forall db. Extends db DB =>
---      Agent -> Query db (Either (ThentosError db) (Set.Set Role))
-- becomes
-- ([Agent], QueryT, (Set.Set Role))
analyseEventFunc :: Name -> Q ([Type], TransType, Type)
analyseEventFunc func = do
    info <- reify func
    let typ = case info of
            VarI _ t _ _ -> t
            _ -> error $ nameBase func ++ " is not a function"
        (args, returnType) = extractArgs typ
        (transType, rType) = analyseReturnType returnType
    return (args, transType, rType)

data TransType = TransQuery | TransUpdate

-- Query db (Either (ThentosError db) (Set.Set Role)) --> (TransQuery, (Set.Set Role))
analyseReturnType :: Type -> (TransType, Type)
analyseReturnType (AppT (AppT transType _dbVar) (AppT (_either_err) returnType)) =
    let ttyp = if | transType == ConT ''Query -> TransQuery
                  | transType == ConT ''Update -> TransUpdate
                  | otherwise -> error "analyseReturnType: bad transaction type"
    in (ttyp, returnType)
analyseReturnType _ = error "analyseReturnType: unexpected type"


processTransaction :: Name -> Q [Dec]
processTransaction functionName = do
    info <- reify functionName
    let typ = case info of
            VarI _ t _ _ -> t
            _ -> error $ nameBase functionName ++ " is not a function"
    processTransaction' functionName typ

processTransaction' :: Name -> Type -> Q [Dec]
processTransaction' functionName typ = do
    let name_suffix = dropPrefix functionName
        (sig, transType) = makeThentosType' typ
        signature = SigD name_suffix sig
    fun <- makeFinalFun functionName name_suffix (countArgs typ) transType
    return [signature, fun]

dropPrefix :: Name -> Name
dropPrefix (nameBase -> s)
    | take 6 s == "trans_" = mkName $ drop 6 s
    | otherwise            = error $ "expected function name starting with 'trans_', got '" ++ s ++ "'"

-- | Count the number of arguments in a function type
countArgs :: Type -> Int
countArgs (ForallT _ _ app) = countArgs app
countArgs (AppT (AppT ArrowT _arg) returnType) = 1 + countArgs returnType
countArgs _ = 0

-- | Convert e.g. @a -> b -> ThentosUpdate Foo@ to
-- @a -> b -> Update DB (Either (ThentosError DB) Foo)@ and check whether it
-- is an Update or a Query.
makeThentosType :: Type -> (Type, ThentosTransactionType)
makeThentosType (AppT (AppT ArrowT arg) returnType) =
    let (rightOfArrow, transType) = makeThentosType returnType
    in (AppT (AppT ArrowT arg) rightOfArrow, transType)
makeThentosType (AppT (AppT t (VarT _)) returnType)
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
        AppT (AppT (ConT acidStateTypeConstructor) (VarT dbName))
            (AppT (AppT (ConT ''Either) (AppT (ConT ''ThentosError) (VarT dbName))) returnType)
    dbName = mkName "db"
makeThentosType (ForallT _ _ app) = makeThentosType app
makeThentosType t = error $ "not a thentos transaction type: " ++ ppprint t

makeThentosType' :: Type -> (Type, ThentosTransactionType)
makeThentosType' t =
    let (typ, transTyp) = makeThentosType t
    in (ForallT [PlainTV dbVarName] [ClassP ''Extends [VarT dbVarName, ConT ''DB]] typ, transTyp)
  where
    dbVarName = mkName "db"


-- | Generate a function definition.
makeFinalFun :: Name -> Name -> Int -> ThentosTransactionType -> Q Dec
makeFinalFun nameWithPrefix functionName argCount transType = do
    args <- replicateM argCount (newName "arg")
    let funToApply =
            case transType of
                ThentosU -> "runThentosUpdate"
                ThentosQ -> "runThentosQuery"
        body = NormalB $ makeFunApp (mkName funToApply) nameWithPrefix args
    return $ FunD functionName [Clause (map VarP args) body []]

-- | Generate a function application expression like
-- @runThentosQuery (trans_do_something x y)@.
makeFunApp :: Name -> Name -> [Name] -> Exp
makeFunApp updateOrQuery funName argNames =
    AppE (VarE updateOrQuery) $ foldl AppE (VarE funName) (map VarE argNames)

ppprint :: (Ppr a, Show a) => a -> String
ppprint t = "\n\n" ++ pprint t ++ "\n\n" ++ ppShow t ++ "\n"
