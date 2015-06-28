> {-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies, NoMonomorphismRestriction, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, ScopedTypeVariables, TypeOperators #-}
> module AcidPoly where
>
> import Control.Category
> import Control.Lens
> import Control.Lens.Internal.Zoom
> import Control.Monad.Reader
> import Control.Monad.State
> import Data.Acid
> import Data.Acid.Advanced
> import Data.SafeCopy
> import Data.Typeable
> import Language.Haskell.TH
> import Prelude hiding ((.), id)
> import Unsafe.Coerce

Using a user-extensible database with acid-state
================================================

The problem
-----------

We have some domain-specific state that we'd like to log and make
persistent using acid-state.

To keep things simple, that our state / database contains just a single
integer.

> data DBM = DBM { _dbCounterM :: Int }
>   deriving (Show, Typeable)

For acid-state, we need to make this an instance of `SafeCopy`. For
general purposes, we want to derive lenses for it, too:

> deriveSafeCopy 0 'base ''DBM
> makeLenses ''DBM

Now let's assume we have three operations on the database: one to read
the current counter value, one to increment it, and one to set it to
an arbitrary value. With acid-state, we first declare each action as
follows:

> getCounterM :: Query DBM Int
> getCounterM = view dbCounterM

> setCounterM :: Int -> Update DBM ()
> setCounterM val = dbCounterM .= val

> incCounterM :: Update DBM ()
> incCounterM = dbCounterM += 1

Now each of these actions is monomorphic in type `DBM`. Under this
assumption, it's easy to make these actions acidic as follows:

> makeAcidic ''DBM ['getCounterM, 'setCounterM, 'incCounterM]

This magically gives us datatypes and data constructors as follows

< data GetCounterM = GetCounterM
< data SetCounterM = SetCounterM Int
< data IncCounterM = IncCounterM

together with suitable instances for certain type families and
classes that we can use in an actual application.

Here's an example:

> testM :: IO ()
> testM = do
>   db <- openLocalState (DBM 0)
>   s0 <- query db GetCounterM
>   print s0
>   update db IncCounterM
>   s1 <- query db GetCounterM
>   print s1
>   update db (SetCounterM (2 * s1))
>   s2 <- query db GetCounterM
>   print s2
>   closeAcidState db

The problem arises because we'd like `DBM` to be *extensible*.
Another application might need to extend `DBM` with new pieces of
data and additional actions. However, the old actions defined by
the original library should continue to work in the new context.

Extending the database is simple enough:

> data UDBM = UDBM { _udbCoreM :: DBM, _udbExtraM :: Bool }
>
> deriveSafeCopy 0 'base ''UDBM
> makeLenses ''UDBM
>
> toggleExtraM :: Update UDBM Bool
> toggleExtraM = do
>   udbExtraM %= not
>   use udbExtraM

However, what do we do now? We'd like to call `makeAcidic` on
'UDBM` as follows:

< makeAcidic ''UDBM
<   ['toggleExtraM, 'getCounterM, 'setCounterM, 'incCounterM]

But we can't! Name clashes with the previous invokation aside,
we get errors because `DBM` and `UDBM` are incompatible types.

Going into more detail
----------------------

The first call to `makeAcidic` above generates the following
code (we omit certain parts such as the `SafeCopy` instances
being generated that aren't directly relevant here), and we
also just focus on the `getCounter` action, as the others are
similar.

< data GetCounterM = GetCounterM
<   deriving (Typeable)
<
< instance Method GetCounterM where
<   type MethodResult GetCounterM = Int
<   type MethodState  GetCounterM = DBM
<
< instance QueryEvent GetCounterM
<
< instance IsAcidic DBM where
<   acidEvents =
<     [ QueryEvent  (\ GetCounterM -> getCounterM)
<     , UpdateEvent (\ (SetCounterM arg) -> setCounterM arg)
<     , UpdateEvent (\ IncCounterM -> incCounterM)
<     ]

The class `IsAcidic` is defined as follows:

< class SafeCopy a => IsAcidic a where
<   acidEvents :: [Event a]

And the type of `QueryEvent` is as follows:

< QueryEvent :: QueryEvent e
<            => (e -> Query (MethodState e) (MethodResult e))
<            -> Event (MethodState e)

So via `MethodState`, the `GetCounterM` action is tied very
specifically to `DBM`. It isn't type-correct if it occurs in
an `IsAcidic` instance for `UDBM`. Also,

< query :: QueryEvent e
<       => AcidState (MethodState e) -> e -> IO (MethodResult e)

ensures that we can only run this action in an `AcidState`
context that is specific to `DBM`, and not one that is specific
to `UDBM`.

Towards a solution
------------------

Intuitively, there is hope: we have a lens between `UDBM` and `DBM`
given by `udbCoreM`. In general, if `DBM` is contained via a lens
in a larger database, we should be able to run actions based on the
smaller database in the larger context.

If we try to make things more polymorphic in order to achieve this, we
have two basic options. First, we can try to make `AcidState` more
flexible. I.e., if we could turn an `AcidState UDBM` into an
`AcidState DBM`, we might be able to just use the original
`GetCounterM`, derived for `DBM`, in the context of the `UDBM`
database.

Unfortunately, this turns out to be not particularly realistic.
The type `AcidState` is abstract, not an instance of any suitable
type classes, and even if we look inside, it contains several functions
relating to the state type, and has several internal constructors for
different acid-state models (in-memory, local disk, remote), so that
adapting this would be a very invasive operation.

The other, more promising, option seems to be to make the actions
themselves more polymorphic.

In fact, it is near-trivial to make the original method definitions
more polymorphic. (We make new definitions of `DBM` and `UDBM` in
order to have both the monomorphic and polymorphic developments
side-by-side in the same module.)

> data DBP = DBP { _dbCounterP :: Int }
>   deriving (Show, Typeable)
>
> deriveSafeCopy 0 'base ''DBP
> makeLenses ''DBP
>
> data UDBP = UDBP { _udbCoreP :: DBP, _udbExtraP :: Bool }
>   deriving (Show, Typeable)
>
> deriveSafeCopy 0 'base ''UDBP
> makeLenses ''UDBP

We now make the relation between the two databases precise using
a type class that gives us the lens when desired:

> class (Typeable db1, Typeable db2, SafeCopy db1, SafeCopy db2) =>
>       db1 `Extends` db2 where
>   focus :: Lens' db1 db2
>
> instance DBP `Extends` DBP where
>   focus = id
>
> instance UDBP `Extends` UDBP where
>   focus = id
>
> instance UDBP `Extends` DBP where
>   focus = udbCoreP

When defining the actions, we can now use the `Extends` class in
order to keep them polymorphic. This requires ever so slightly
rephrasing them, but is all in all very easy:

> getCounterP :: (db `Extends` DBP) => Query db Int
> getCounterP = view (focus . dbCounterP)
>
> setCounterP :: (db `Extends` DBP) => Int -> Update db ()
> setCounterP val = focus . dbCounterP .= val
>
> incCounterP :: (db `Extends` DBP) => Update db ()
> incCounterP = focus . dbCounterP += 1
>
> toggleExtraP :: (db `Extends` UDBP) => Update db Bool
> toggleExtraP = do
>   focus . udbExtraP %= not
>   use (focus . udbExtraP)

Unfortunately, this promising approach seems to fall short as
soon as we try to call `makeAcidic`. Already

< makeAcidic ''DBP ['getCounterP, 'setCounterP, 'incCounterP]

fails because `makeAcidic` actually complains about the class
context. A call for `UDBP` fails for the same reason.

But is this all just a shortcoming of the Template Haskell code
in `makeAcidic`? Not quite. The question is what code we'd want
to generate for each action, such as `getCounterP`. Let's
look once again at the code we had for `getCounterM`:

< data GetCounterM = GetCounterM
<   deriving (Typeable)
<
< instance Method GetCounterM where
<   type MethodResult GetCounterM = Int
<   type MethodState  GetCounterM = DBM
<
< instance QueryEvent GetCounterM

As long as we have to tie the `MethodState` to a specific type
here, we're in trouble. The only option then is to generate two
different wrapper datatypes, but that defeats the purpose of
being able to reuse code coming from the core library flexibly.

The actual solution
-------------------

Ok, so let's try to define `GetCounterP` such that it is
also polymorphic. This means we have to avoid setting
`MethodState GetCounterP` to `DBP` or any specific datatype.

Instead, we're going to parameterize the `GetCounterP` type
over its state type:

> data GetCounterP db = GetCounterP
>   deriving Typeable
>
> deriveSafeCopy 0 'base ''GetCounterP
>
> instance (db `Extends` DBP) => Method (GetCounterP db) where
>   type MethodResult (GetCounterP db) = Int
>   type MethodState  (GetCounterP db) = db
>
> instance (db `Extends` DBP) => QueryEvent (GetCounterP db)

We can proceed similarly for the other actions:

> data SetCounterP db = SetCounterP Int
>   deriving Typeable
>
> deriveSafeCopy 0 'base ''SetCounterP
>
> instance (db `Extends` DBP) => Method (SetCounterP db) where
>   type MethodResult (SetCounterP db) = ()
>   type MethodState  (SetCounterP db) = db
>
> instance (db `Extends` DBP) => UpdateEvent (SetCounterP db)
>
> data IncCounterP db = IncCounterP
>   deriving Typeable
>
> deriveSafeCopy 0 'base ''IncCounterP
>
> instance (db `Extends` DBP) => Method (IncCounterP db) where
>   type MethodResult (IncCounterP db) = ()
>   type MethodState  (IncCounterP db) = db
>
> instance (db `Extends` DBP) => UpdateEvent (IncCounterP db)
>
> data ToggleExtraP db = ToggleExtraP
>   deriving Typeable
>
> deriveSafeCopy 0 'base ''ToggleExtraP
>
> instance (db `Extends` UDBP) => Method (ToggleExtraP db) where
>   type MethodResult (ToggleExtraP db) = Bool
>   type MethodState  (ToggleExtraP db) = db
>
> instance (db `Extends` UDBP) => UpdateEvent (ToggleExtraP db)

We can now even define the core item of the `IsAcidic` class,
the list of events, in a polymorphic way:

> dbpEvents :: (db `Extends` DBP) => [Event db]
> dbpEvents =
>   [ QueryEvent  (\ GetCounterP -> getCounterP)
>   , UpdateEvent (\ (SetCounterP arg) -> setCounterP arg)
>   , UpdateEvent (\ IncCounterP -> incCounterP)
>   ]

> udbpEvents :: (db `Extends` UDBP, db `Extends` DBP) => [Event db]
> udbpEvents =
>     UpdateEvent (\ ToggleExtraP -> toggleExtraP)
>   : dbpEvents

Once this work is done, making two separate `IsAcidic` instances
is trivial:

> instance IsAcidic DBP  where acidEvents = dbpEvents
> instance IsAcidic UDBP where acidEvents = udbpEvents

Having separate, monomorphic, instances of `IsAcidic` makes sense.
After all, in one application, we're probably going to work with
either the one or the other database, but not with both. The
achievement is that we can use actions such as `IncCounterP` in
both contexts, without having to redefine or cast them in any way.

Here's the original program in the new polymorphic context:

> testP :: IO ()
> testP = do
>   db <- openLocalState (DBP 0)
>   s0 <- query db GetCounterP
>   print s0
>   update db IncCounterP
>   s1 <- query db GetCounterP
>   print s1
>   update db (SetCounterP (2 * s1))
>   s2 <- query db GetCounterP
>   print s2
>   closeAcidState db

And here's a slightly different program using `UDBP`:

> testUP :: IO ()
> testUP = do
>   db <- openLocalState (UDBP (DBP 0) False)
>   s0 <- query db GetCounterP
>   print s0
>   update db IncCounterP
>   s1 <- query db GetCounterP
>   print s1
>   update db (SetCounterP (2 * s1))
>   s2 <- query db GetCounterP
>   print s2
>   s3 <- update db ToggleExtraP
>   print s3
>   closeAcidState db

Final remarks
-------------

To make this design convenient, there should be a function
`makePolyAcidic` that does essentially the same job as `makeAcidic`,
but in a polymorphic way.

In concrete terms, I imagine that the above code could be written
more succinctly by saying

< makePolyAcidic ''DBP  []      ['getCounterP, 'setCounterP, 'incCounterP]
< makePolyAcidic ''UDBP [''DBP] ['toggleExtraP]

The first and final argument would be like that for `makeAcidic`, but
only contain the actions specific to the current database type. The new
middle argument would indicate which other operations to include.


With a little bit of hackery, it may also be possible to make the
definition of the polymorphic actions themselves slightly more systematic.
Unfortunately, this will again either mean putting code directly into
acid-state, or unsafely coercing the `Update` and `Query` datatypes
to their internal representations, because acid-state doesn't expose anything
of them.

> type instance Zoomed (Update s) = Zoomed (State s)
>
> instance Zoom (Update s) (Update t) s t where
>   zoom = unsafeCoerce (zoom :: LensLike' (Zoomed (Update s) c) t s -> State s c -> State t c)
>
> type instance Magnified (Query s) = Magnified (Reader s)
>
> instance Magnify (Query s) (Query t) s t where
>   magnify = unsafeCoerce (magnify :: LensLike' (Magnified (Query  s) c) t s -> Reader s c -> Reader t c)

With this, we could define our actions as follows:

> getCounterP' :: (db `Extends` DBP) => Query db Int
> getCounterP' = magnify focus $ view dbCounterP
>
> setCounterP' :: (db `Extends` DBP) => Int -> Update db ()
> setCounterP' val = zoom focus $ dbCounterP .= val
>
> incCounterP' :: (db `Extends` DBP) => Update db ()
> incCounterP' = zoom focus $ dbCounterP += 1
>
> toggleExtraP' :: (db `Extends` UDBP) => Update db Bool
> toggleExtraP' = zoom focus $ do
>   udbExtraP %= not
>   use (udbExtraP)

This is systematic enough that we could even allow the actions to be
defined monomorphically, and have the `makePolyAcidic` code apply `zoom`
in order to turn them into polymorphic actions.
