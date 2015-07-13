{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE ViewPatterns              #-}

module Thentos.Transaction.TH
    ( makeThentosAcidicPhase1
    , makeThentosAcidicPhase2
    )
where

import Control.Applicative (pure, (<$>), (<*>))
import Control.Monad (replicateM)
import Data.Acid (Update, Query, UpdateEvent, QueryEvent)
import Data.Acid.Advanced (IsAcidic(..), Event(QueryEvent, UpdateEvent), Method, MethodResult, MethodState)
import Data.Char (toUpper)
import Data.Data (Typeable)
import Data.SafeCopy (SafeCopy(putCopy, getCopy), safeGet, safePut, contain)
import Language.Haskell.TH
import Text.Show.Pretty (ppShow)

import Thentos.Transaction.Core (ThentosUpdate, ThentosQuery)
import Thentos.Types (ThentosError, Extends)


data TransactionType = TransQuery | TransUpdate

makeThentosAcidicPhase1 :: Name -> [Name] -> Q [Dec]
makeThentosAcidicPhase1 dbTypeName names =
    concat <$> mapM (processTransaction $ ConT dbTypeName) names

{-
-- for every transaction, we need the following:

data AgentRoles db = AgentRoles Agent
    deriving Typeable

instance SafeCopy (AgentRoles db) where
    putCopy (AgentRoles a) = contain (do putCopy a; return ();)
    getCopy = contain (pure AgentRoles <*> getCopy)

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

-- for db types that extend an existing db type, dbEvents will look like this:
dbEvents :: (db `Extends` DB, db `Extends` CustomDB => [Event db]
dbEvents = [...new events...] ++ ParentDb.dbEvents

instance IsAcidic DB where
    acidEvents = dbEvents
-}

makeThentosAcidicPhase2 :: Name -> [Name] -> [Name] -> [Name] -> Q [Dec]
makeThentosAcidicPhase2 stateName transNames inheritedDbTypes inheritedEvents = do
    decs <- concat <$> mapM (acidifyTrans stateName) eventNames
    eventList <- mkEventList inheritedDbTypes inheritedEvents stateName eventNames
    isAcidic <- makeIsAcidic stateName
    return $ decs ++ eventList ++ isAcidic
  where
    eventNames = map dropPrefix transNames

makeIsAcidic :: Name -> Q [Dec]
makeIsAcidic dbType =
    [d| instance IsAcidic $(conT dbType) where acidEvents = $(varE $ mkName "dbEvents") |]

mkEventList :: [Name] -> [Name] -> Name -> [Name] -> Q [Dec]
mkEventList inheritedDbTypes inheritedEvents dbName eventNames = do
     sig <- mkSig
     def <- mkDef
     return [sig, def]
  where
    mkSig = do
        dbTypeVar <- newName "db"
        let ctx = mapM (\db -> classP ''Extends [varT dbTypeVar, db]) allDbTypes
        sigD (mkName "dbEvents") $
            forallT [PlainTV dbTypeVar]
            ctx
            [t| [Event $(varT dbTypeVar)] |]

    mkDef = do
        newEvents <- listE $ map mkEntryForEvent eventNames
        let l = foldr (\a b -> UInfixE a (VarE '(++)) b)
                      (ListE [])
                      (newEvents : map VarE inheritedEvents)
        valD (varP $ mkName "dbEvents") (pure $ NormalB l) []

    allDbTypes :: [Q Type]
    allDbTypes = map conT (dbName : inheritedDbTypes)

    mkEntryForEvent :: Name -> Q Exp
    mkEntryForEvent eventName = do
        (argTypes, evType, _) <- analyseEventFunc eventName
        argNames <- mapM (\_ -> newName "arg") argTypes
        let evConst = conE $ case evType of
                TransQuery -> 'QueryEvent
                TransUpdate -> 'UpdateEvent
            constName = toTypeName eventName

            lambdaBody = foldl appE (varE eventName) (map varE argNames)
            lambda = lamE [conP constName (map varP argNames)] lambdaBody
            -- the following should also work, but crashes ghc-7.8.4 on mac:
            -- lambda = [e| \ $(conP constName (map varP argNames)) -> $lambdaBody |]
        appE evConst lambda

acidifyTrans :: Name -> Name -> Q [Dec]
acidifyTrans dbName eventName = do
    (argTypes, transType, returnType) <- analyseEventFunc eventName
    typDecl <- mkTypeDecl (map pure argTypes)
    sc <- mkSafeCopyInstance (length argTypes)
    methodInstance <- mkMethodInstance returnType
    eventInstance <- mkEventInstance transType
    return $ [typDecl] ++ sc ++ [methodInstance, eventInstance]
  where
    acidTypeName = toTypeName eventName

    -- data AgentRoles t = AgentRoles Agent deriving Typeable
    mkTypeDecl :: [Q Type] -> Q Dec
    mkTypeDecl args = do
        typeVar <- newName "t"
        let conArgs = map (\a -> strictType notStrict a) args
            constructor = normalC acidTypeName conArgs
            derive = [''Typeable]
        dataD (pure []) acidTypeName [PlainTV typeVar] [constructor] derive

    -- instance (db `Extends` DB) => Method (AgentRoles db) where
    --     type MethodResult (AgentRoles db) = Either (ThentosError db) (Set Role)
    --     type MethodState (AgentRoles db) = db
    mkMethodInstance :: Type -> Q Dec
    mkMethodInstance returnType = do
        dbTypeVar <- VarT <$> newName "db"
        let ctx = [classPCompat ''Extends [dbTypeVar, ConT dbName]]
            fullType = AppT (ConT ''Method) (AppT (ConT acidTypeName) dbTypeVar)
        rDecl <- mkResultDecl (pure dbTypeVar) (pure returnType)
        sDecl <- mkStateDecl dbTypeVar
        return $ InstanceD ctx fullType [rDecl, sDecl]

    -- type MethodResult (AgentRoles db) = Either (ThentosError db) (Set Role)
    mkResultDecl :: Q Type -> Q Type -> Q Dec
    mkResultDecl dbTypeVar eventReturnType = do
        let lhs_type = [t| $(conT acidTypeName) $dbTypeVar |]
            returnType = [t| Either (ThentosError $dbTypeVar) $eventReturnType|]
        tySynInstD ''MethodResult (tySynEqn [lhs_type] returnType)

    -- type MethodState (AgentRoles db) = db
    mkStateDecl :: Type -> Q Dec
    mkStateDecl dbTypeVar = do
        let lhs_type = AppT (ConT acidTypeName) dbTypeVar
        return $ TySynInstD ''MethodState (TySynEqn [lhs_type] dbTypeVar)

    -- instance (db `Extends` DB) => QueryEvent (AgentRoles db)
        -- (or UpdateEvent, as the case may be)
    mkEventInstance :: TransactionType -> Q Dec
    mkEventInstance transType = do
        dbTypeVar <- VarT <$> newName "db"
        let evName = case transType of
                TransQuery -> ''QueryEvent
                TransUpdate -> ''UpdateEvent
            ctx = [classPCompat ''Extends [dbTypeVar, ConT dbName]]
        return $ InstanceD ctx
                           (AppT (ConT evName) (AppT (ConT acidTypeName) dbTypeVar))
                           []

    -- instance SafeCopy (AgentRoles db) where
    --    putCopy (AgentRoles a) = contain (do putCopy a; return ();)
    --    getCopy = contain (pure AgentRoles <*> getCopy)
    mkSafeCopyInstance :: Int -> Q [Dec]
    mkSafeCopyInstance argCount = do
        argNames <- replicateM argCount (newName "arg")
        let eventTypeName = toTypeName eventName
            putBody = appE (varE 'contain) doPut
            doPut = doE $ map (\a -> noBindS [e| safePut $(varE a) |])
                              argNames
                        ++ [noBindS [e| return () |] ]
            getBody = appE (varE 'contain) $
                foldl (\a b -> uInfixE a (varE '(<*>)) b)
                      [e| pure $(conE eventTypeName) |]
                      (replicate argCount (varE 'safeGet))

        [d| instance SafeCopy ($(conT acidTypeName) db) where
                putCopy $(conP acidTypeName (map varP argNames)) = $putBody
                getCopy = $getBody
          |]

-- | lookupUser -> LookupUser
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
analyseEventFunc :: Name -> Q ([Type], TransactionType, Type)
analyseEventFunc func = do
    info <- reify func
    let typ = case info of
            VarI _ t _ _ -> t
            _ -> error $ nameBase func ++ " is not a function"
        (args, returnType) = extractArgs typ
        (transType, rType) = analyseReturnType returnType
    return (args, transType, rType)

-- Query db (Either (ThentosError db) (Set Role)) --> (TransQuery, (Set Role))
analyseReturnType :: Type -> (TransactionType, Type)
analyseReturnType (AppT (AppT transType _dbVar) (AppT (_either_err) returnType)) =
    let ttyp = if | transType == ConT ''Query -> TransQuery
                  | transType == ConT ''Update -> TransUpdate
                  | otherwise -> error "analyseReturnType: bad transaction type"
    in (ttyp, returnType)
analyseReturnType _ = error "analyseReturnType: unexpected type"


processTransaction :: Type -> Name -> Q [Dec]
processTransaction dbType functionName = do
    info <- reify functionName
    let typ = case info of
            VarI _ t _ _ -> t
            _ -> error $ nameBase functionName ++ " is not a function"
    processTransaction' functionName typ dbType

processTransaction' :: Name -> Type -> Type -> Q [Dec]
processTransaction' functionName typ dbType = do
    let name_suffix = dropPrefix functionName
        (sig, transType) = makeThentosType' typ dbType
        signature = SigD name_suffix sig
    fun <- makeFinalFun functionName name_suffix (countArgs typ) transType
    return [signature, fun]

dropPrefix :: Name -> Name
dropPrefix (nameBase -> s)
    | take 6 s == "trans_" = mkName $ drop 6 s
    | otherwise            = error $ "expected function name starting with 'trans_', got '" ++ s ++ "'"

-- | Count the number of arguments in a function type
countArgs :: Type -> Int
countArgs = length . fst . extractArgs

-- | Convert e.g. @a -> b -> ThentosUpdate Foo@ to
-- @a -> b -> Update DB (Either (ThentosError DB) Foo)@ and check whether it
-- is an Update or a Query.
makeThentosType :: Type -> (Type, TransactionType)
makeThentosType (AppT (AppT ArrowT arg) returnType) =
    let (rightOfArrow, transType) = makeThentosType returnType
    in (AppT (AppT ArrowT arg) rightOfArrow, transType)
makeThentosType (AppT (AppT t (VarT _)) returnType)
    | t == ConT (''ThentosUpdate) = (updateType, TransUpdate)
    | t == ConT (''ThentosQuery) = (queryType, TransQuery)
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

makeThentosType' :: Type -> Type -> (Type, TransactionType)
makeThentosType' t dbType =
    let (typ, transTyp) = makeThentosType t
    in (ForallT [PlainTV dbVarName] [classPCompat ''Extends [VarT dbVarName, dbType]] typ, transTyp)
  where
    dbVarName = mkName "db"


-- | Generate a function definition.
makeFinalFun :: Name -> Name -> Int -> TransactionType -> Q Dec
makeFinalFun nameWithPrefix functionName argCount transType = do
    args <- replicateM argCount (newName "arg")
    let funToApply =
            case transType of
                TransUpdate -> "runThentosUpdate"
                TransQuery  -> "runThentosQuery"
        body = NormalB $ makeFunApp (mkName funToApply) nameWithPrefix args
    return $ FunD functionName [Clause (map VarP args) body []]

-- | Generate a function application expression like
-- @runThentosQuery (trans_do_something x y)@.
makeFunApp :: Name -> Name -> [Name] -> Exp
makeFunApp updateOrQuery funName argNames =
    AppE (VarE updateOrQuery) $ foldl AppE (VarE funName) (map VarE argNames)

ppprint :: (Ppr a, Show a) => a -> String
ppprint t = "\n\n" ++ pprint t ++ "\n\n" ++ ppShow t ++ "\n"


classPCompat :: Name -> [Type] -> Pred
#if MIN_VERSION_template_haskell(2,10,0)
classPCompat cla tys = foldl AppT (ConT cla) tys
#else
classPCompat = ClassP
#endif
