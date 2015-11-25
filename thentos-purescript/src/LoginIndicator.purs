module LoginIndicator where

import Control.Monad.Aff (Aff(), runAff, forkAff, later')
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)
import Data.Generic
import Data.List
import Data.Void
import Halogen (Component(), ComponentHTML(), ComponentDSL(), HalogenEffects(), Natural(), runUI, component, modify)
import Halogen.Util (appendTo)
import Prelude

import qualified Data.Array as Array
import qualified Halogen.HTML.Core as H
import qualified Halogen.HTML.Events.Handler as EH
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P

import Error
import Mula


data State = LoggedIn RegisterOption String | LoggedOut RegisterOption
data RegisterOption = RegisterOption | NoRegisterOption

derive instance genericState :: Generic State
instance showState :: Show State where show = gShow

derive instance genericRegisterOption :: Generic RegisterOption
instance showRegisterOption :: Show RegisterOption where show = gShow

initialState :: State
initialState = LoggedOut RegisterOption

data Query a = Query a Event
data Event = Login String String | Logout | SetRegisterOption RegisterOption

hrefClickHandler :: forall e. Event -> P.IProp (onClick :: P.I | e) (Query Unit)
hrefClickHandler queryEv = E.onClick $ \domEv -> do
    EH.preventDefault :: EH.EventHandler Unit
    EH.stopPropagation :: EH.EventHandler Unit
    EH.stopImmediatePropagation :: EH.EventHandler Unit
    E.input_ (\a -> Query a queryEv) domEv

ui :: forall g. (Functor g) => Component State Query g
ui = component render eval
  where
    render :: State -> ComponentHTML Query
    render st = H.div [cl "user-indicator"] [body st]
      where
        cl :: forall r i. String -> P.IProp (class :: P.I | r) i
        cl = P.class_ <<< H.className

        body (LoggedOut regopt) = H.div [cl "user-indicator-primary"] $
                                      Array.concat [[i, a], r regopt]
          where
            i :: H.HTML Void (Query Unit)
            i = H.i (cl <$> ["icon-user", "user-indicator-login-icon"]) []

            a :: H.HTML Void (Query Unit)
            a = H.a [ cl "user-indicator-login", P.href ""
                    , hrefClickHandler $ Login "wef" "pass"
                    ]
                    [trh "login"]

            r :: RegisterOption -> Array (H.HTML Void (Query Unit))
            r NoRegisterOption = []
            r RegisterOption =
                [ trh "or"
                , H.a [ cl "user-indicator-register", P.href ""
                      , hrefClickHandler $ SetRegisterOption NoRegisterOption
                      ]
                      [trh "register"]
                ]

        body (LoggedIn _ name) = H.div_ [n, l]
          where
            n = if noLink
                  then H.a [cl "user-indicator-name", P.href userUrl] [H.text name]
                  else H.span [cl "user-indicator-name"] [H.text name]
            l = H.a [ cl "user-indicator-logout", P.href ""
                    , hrefClickHandler Logout
                    ]
                    [trh "logout"]

            noLink :: Boolean
            noLink = false  -- FIXME: data-ng-if="!noLink"

            userUrl :: String
            userUrl = "/user/url"  -- FIXME: data-ng-href="{{ credentials.userPath | adhResourceUrl }}"

    eval :: Natural Query (ComponentDSL State Query g)
    eval (Query next ev) = do
          modify (f ev)
          pure next
      where
        f (SetRegisterOption ro) (LoggedIn _ n) = LoggedIn ro n
        f (SetRegisterOption ro) (LoggedOut _)  = LoggedOut ro

        f (Login name pass) (LoggedOut ro)  = LoggedIn ro name
        f Logout            (LoggedIn ro _) = LoggedOut ro

        f (Login name pass) st@(LoggedIn _ n) = warnJS ("already logged in as " ++ n ++ "!") st
        f Logout            st@(LoggedOut _)  = warnJS "already logged out!" st


main :: forall eff. String -> Eff (HalogenEffects eff) Unit
main selector = runAff throwException (const (pure unit)) <<< forkAff $ do
    { node: node, driver: driver } <- runUI ui initialState
    appendTo selector node

{-
  setInterval 900 $ toList ((\ q -> driver (action (\next -> Query next q))) <$>
      [ Logout
      , Login "wef" "pass"
      , Logout
      , Login "wof" "pass"
      , Login "wef" "pass"
      , SetRegisterOption NoRegisterOption
      , Logout
      , Login "phoo" "pass"
      ])
-}

setInterval :: forall e a. Int -> List (Aff e a) -> Aff e Unit
setInterval ms Nil = pure unit
setInterval ms (Cons action as) = later' ms $ do
    action
    setInterval ms as
