module Register where

import Control.Monad.Aff (Aff(), Canceler(), runAff, forkAff, later')
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)
import Data.Array (concat, intersect)
import Data.Foldable
import Data.Functor (($>))
import Data.Generic
import Data.String (null, length, contains)
import Data.Tuple
import Data.Void
import Halogen (Component(), ComponentHTML(), ComponentDSL(), HalogenEffects(), Action(), Natural(), runUI, component, modify)
import Halogen.Util (appendTo)
import Prelude

import qualified Data.Array as Array
import qualified Halogen.HTML.Core as H
import qualified Halogen.HTML.Events.Handler as EH
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.HTML.Events.Types as ET
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P

import Error
import Mula

foreign import onChangeValue :: forall a. a -> String


-- * types

type State =
    { stErrors        :: Array FormError
    , stOfInterestNow :: Array FormError
    , stLoggedIn      :: Boolean
    , stRegSuccess    :: Boolean
    , stName          :: String
    , stEmail         :: String
    , stPass1         :: String
    , stPass2         :: String
    , stTermsAndConds :: Boolean

    -- TODO: also keep in the state: a trigger function for update loop of surrounding framework.

    , stSupportEmail  :: String
    }

initialState :: State
initialState =
    { stErrors: []
    , stOfInterestNow: []
    , stLoggedIn: false
    , stRegSuccess: false
    , stName: ""
    , stEmail: ""
    , stPass1: ""
    , stPass2: ""
    , stTermsAndConds: false
    , stSupportEmail: ""
    }

data Query a =
    KeyPressed (State -> State) a
  | UpdateTermsAndConds Boolean a
  | AddInterestingErrors (Array FormError) a


-- * render

render :: State -> ComponentHTML Query
render st = H.div [cl "login"]
    [ mydebug st
    , body st
    , H.a [cl "login-cancel", P.href ""]  -- FIXME: link!
        [trh "TR__CANCEL"]
    , H.div [cl "login-info"]
        [ trh "TR__REGISTRATION_SUPPORT"
        , H.br_
        , H.a [P.href $ "mailto:" ++ st.stSupportEmail ++ "?subject=Trouble%20with%20registration"]
                                   -- FIXME: call urlEncode function on proper string ^^.
            [H.text st.stSupportEmail]
        ]
    ]

body :: State -> ComponentHTML Query
body st = case Tuple st.stLoggedIn st.stRegSuccess of

    -- present empty or incomplete registration form
    Tuple false false -> H.form [cl "login-form", P.name "registerForm"] $
        [ inputField st P.InputText (trh "TR__USERNAME") "username"
            (\i s -> s { stName = i })
            [ErrorRequiredUsername]

        , inputField st P.InputEmail (trh "TR__EMAIL") "email"
            (\i s -> s { stEmail = i })
            [ErrorRequiredEmail, ErrorFormatEmail]

        , inputField st P.InputPassword (trh "TR__PASSWORD") "password"
            (\i s -> s { stPass1 = i })
            [ErrorForwardPassword, ErrorTooShortPassword]

        , inputField st P.InputPassword (trh "TR__PASSWORD_REPEAT") "password_repeat"
            (\i s -> s { stPass2 = i })
            [ErrorMatchPassword]

        , H.label [cl "login-check"]
            [ H.div [cl "login-check-input"]
                [ H.input [ P.inputType P.InputCheckbox, P.name "registerCheck", P.required true
                          , E.onChecked $ E.input $ UpdateTermsAndConds
                          ]
                , H.span_ [trh "TR__I_ACCEPT_THE_TERMS_AND_CONDITIONS"]  -- FIXME: link!
                , renderErrors st [ErrorRequiredTermsAndConditions]
                ]
            ]

        , H.input
            [ P.inputType P.InputSubmit, P.name "register", P.value (tr "TR__REGISTER")
            , P.disabled $ not $ Data.Array.null st.stErrors
            , E.onChecked $ E.input $ UpdateTermsAndConds
            ]
        , H.div [cl "login-info"] [H.p_ [trh "TR__REGISTRATION_LOGIN_INSTEAD"]]  -- FIXME: link!
        ]

    -- can not register: already logged in
    Tuple false true -> H.div [cl "login-success"] [H.p_ [trh "TR__REGISTRATION_ALREADY_LOGGED_IN"]]

    -- registered: waiting for processing of activation email
    Tuple true false -> H.div [cl "login-success"]
        [ H.h2_ [trh "TR__REGISTER_SUCCESS"]
        , H.p_ [trh "TR__REGISTRATION_CALL_FOR_ACTIVATION"]

        -- FIXME: the a3 code says this.  what does it mean?:
        -- 'Show option in case the user is not automatically logged in (e.g. 3rd party cookies blocked.)'
        ]

    -- FIXME: a3 code says this.  is that relevant for us?
    -- <!-- FIXME: Technically this should only display if you logged in as the user you just registered as, but
    -- this will display if you log in as any user -->

    -- registered and registration link clicked
    Tuple true true -> H.div [cl "login-success"]
        [ H.h2_ [trh "TR__REGISTRATION_THANKS_FOR_REGISTERING"]
        , H.p_ [trh "TR__REGISTRATION_PROCEED"]  -- FIXME: link
        ]


mydebug :: State -> ComponentHTML Query
mydebug st = H.pre_ [H.p_ [H.text q]]
  where
    q :: String
    q = intercalate ", "
        [ show st.stErrors
        , show st.stOfInterestNow
        , show st.stLoggedIn
        , show st.stRegSuccess
        , show st.stName
        , show st.stEmail
        , show st.stPass1
        , show st.stPass2
        , show st.stTermsAndConds
        ]

-- | The last argument 'ofInterestHere' contains all errors that are reportable near the current
-- field (e.g., all errors related to email address).  The 'State' field 'ofInterestNow' contains a
-- list of all errors that should be reported at this point in time (e.g., excluding email errors
-- because the email field has not been filled out yet).
inputField :: State -> P.InputType -> ComponentHTML Query -> String
           -> (String -> State -> State)
           -> Array FormError
           -> ComponentHTML Query
inputField st inputType msg key updateState ofInterestHere = H.label_
    [ H.span [cl "label-text"] [msg]
    , H.input [ P.inputType inputType, P.name key, P.required true
              , E.onInput $ E.input $ KeyPressed <<< updateState <<< onChangeValue
              , E.onFocusOut $ E.input_ $ AddInterestingErrors ofInterestHere
              , E.onFocusIn $ E.input_ $ AddInterestingErrors ofInterestHere
              ]
    , renderErrors st ofInterestHere
    ]

-- | there is something about this very similar to `trh`: we want to be able to collect all
-- classnames occurring in a piece of code, and construct a list from them with documentation
-- (source file locations?).
cl :: forall r i. String -> P.IProp (class :: P.I | r) i
cl = P.class_ <<< H.className


-- * eval

eval :: forall g. Natural Query (ComponentDSL State Query g)
eval (KeyPressed updateState next) = do
    modify updateState
    modify checkState
    pure next
eval (UpdateTermsAndConds newVal next) = do
    modify (\st -> st { stTermsAndConds = newVal })
    modify checkState
    pure next
eval (AddInterestingErrors es next) = do
    modify (\st -> st { stOfInterestNow = union es st.stOfInterestNow })
    modify checkState
    pure next


-- * form errors

data FormError =
      ErrorRequiredUsername
    | ErrorRequiredEmail
    | ErrorFormatEmail
    | ErrorForwardPassword
    | ErrorTooShortPassword
    | ErrorMatchPassword
    | ErrorRequiredTermsAndConditions

derive instance genericFormError :: Generic FormError
instance eqFormError :: Eq FormError where eq = gEq

-- | ('show' for 'FormError' returns the translation keys.)
instance showFormError :: Show FormError where
    show ErrorRequiredUsername = "TR__ERROR_REQUIRED_USERNAME"
    show ErrorRequiredEmail = "TR__ERROR_REQUIRED_EMAIL"
    show ErrorFormatEmail = "TR__ERROR_FORMAT_EMAIL"
    show ErrorForwardPassword = "TR__ERROR_REQUIRED_PASSWORD"
    show ErrorTooShortPassword = "TR__ERROR_TOO_SHORT_PASSWORD"
    show ErrorMatchPassword = "TR__ERROR_MATCH_PASSWORD"
    show ErrorRequiredTermsAndConditions = "TR__ERROR_REQUIRED_TERMS_AND_CONDITIONS"

checkState :: State -> State
checkState st = st { stErrors = intersect st.stOfInterestNow $ concat
    [ if null st.stName           then [ErrorRequiredUsername] else []
    , if null st.stEmail          then [ErrorRequiredEmail] else []
    , if contains "@" st.stEmail  then [] else [ErrorFormatEmail]
    , if null st.stPass1          then [ErrorForwardPassword] else []
    , if length st.stPass1 >= 6   then [] else [ErrorTooShortPassword]
    , if st.stPass1 == st.stPass2 then [] else [ErrorMatchPassword]
    , if st.stTermsAndConds       then [] else [ErrorRequiredTermsAndConditions]
    ]
  }

renderErrors :: State -> Array FormError -> ComponentHTML Query
renderErrors st ofInterstHere = H.span [cl "input-error"] $ (trh <<< show) <$> forReporting
  where
    forReporting :: Array FormError
    forReporting = intersect st.stErrors ofInterstHere


-- * main

ui :: forall g. (Functor g) => Component State Query g
ui = component render eval

main :: forall eff. String -> Eff (HalogenEffects eff) Unit
main selector = runAff throwException (const (pure unit)) <<< forkAff $ do
    { node: node, driver: driver } <- runUI ui initialState
    appendTo selector node


-- FIXME: widget destruction?
