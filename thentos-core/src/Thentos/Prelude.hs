module Thentos.Prelude (module X) where

import Control.Applicative         as X
import Control.Conditional         as X hiding ((<|), (??), (|>))
import Control.Exception           as X (ErrorCall (..), Exception, IOException)
import Control.Exception.Lifted    as X (catch, throwIO, try)
import Control.Lens                as X hiding (Bifunctor, Context, bimap, contexts)
import Control.Monad               as X hiding (guard, unless, when)
import Data.Char                   as X (isAlpha, ord, toUpper)
{- FIXME: using monad-base might help working with MonadIO and MonadLIO
import Control.Monad.Base          as X (MonadBase (liftBase), liftBaseDefault)
-}
import Control.Monad.Error.Class   as X (MonadError, catchError, throwError)
import Control.Monad.IO.Class      as X (MonadIO (liftIO))
import Control.Monad.Random        as X (evalRand, mkStdGen)
import Control.Monad.Random.Class  as X (MonadRandom, getRandomR)
import Control.Monad.Reader.Class  as X (MonadReader (ask, local, reader), asks)
import Control.Monad.State.Class   as X (MonadState (put, get, state), gets, modify, modify')
import Control.Monad.Trans.Control as X (MonadBaseControl)
import Data.Bifunctor              as X (Bifunctor, bimap, first, second)
import Data.Either                 as X (isLeft, isRight)
import Data.Foldable               as X
import Data.Function               as X (on)
import Data.Functor                as X (($>))
import Data.Functor.Infix          as X ((<$$>))
import Data.Int                    as X (Int64)
import Data.List                   as X (groupBy, intercalate, isPrefixOf, nub, nubBy, sort,
                                         unfoldr, (\\))
import Data.Maybe                  as X (catMaybes, fromJust, fromMaybe, isJust, isNothing,
                                         listToMaybe, maybeToList)
import Data.Monoid                 as X
import Data.Proxy                  as X (Proxy (Proxy))
import Data.String                 as X (IsString (fromString))
import Data.String.Conversions     as X (ConvertibleStrings, LBS, LT, SBS, ST, cs)
import Data.Traversable            as X
import Data.Typeable               as X (Typeable)
import Data.Void                   as X (Void, absurd)
import GHC.Generics                as X (Generic)
import LIO.Core                    as X (LIOState (LIOState), MonadLIO (liftLIO), evalLIO,
                                         getClearance, guardWrite, setClearance, taint)
import LIO.DCLabel                 as X (CNF, DCLabel (DCLabel), ToCNF (toCNF), cFalse, (%%), (/\),
                                         (\/))
import LIO.Error                   as X (AnyLabelError (AnyLabelError))
import LIO.Label                   as X (Label, lub)
import LIO.Missing                 as X (dcBottom, dcTop, guardWriteOk, tryGuardWrite, tryTaint)
import Safe                        as X (fromJustNote, readMay)
import System.Log                  as X (Priority (..))
import System.Log.Logger           as X (removeAllHandlers, setHandlers, setLevel,
                                         updateGlobalLogger)
import System.Log.Missing          as X (Prio (fromPrio), announceAction, logger, loggerName)
import System.Random               as X (Random)
import Text.Show.Pretty            as X (ppShow)
