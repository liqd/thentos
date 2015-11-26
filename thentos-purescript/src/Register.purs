module Register where

import Prelude

import Control.Monad.Aff.Class (MonadAff)
import Control.Monad.Aff.Console (print)
import Control.Monad.Aff (runAff, forkAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE())
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)
import Data.Array (concat, intersect, union)
import Data.Foldable
import Data.Generic
import Data.String (length)
import Data.Tuple
import DOM.HTML.Types (HTMLElement(), htmlElementToNode)
import DOM.Node.Node (appendChild)
import Halogen (Component(), ComponentHTML(), ComponentDSL(), HalogenEffects(), Natural(), runUI, component, modify, liftAff')
import Halogen.Util (appendTo)

import qualified Halogen.HTML.Core as H
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.HTML.Events.Types as E
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P

import Mula

foreign import eventInputValue :: forall fields. E.Event fields -> InputValue


-- * types

type State =
    { stErrors        :: Array FormError
    , stOfInterestNow :: Array FormError
    , stLoggedIn      :: Boolean
    , stRegSuccess    :: Boolean
    , stName          :: InputValue
    , stEmail         :: InputValue
    , stPass1         :: InputValue
    , stPass2         :: InputValue
    , stTermsAndConds :: Boolean

    -- TODO: also keep in the state: a trigger function for update loop of surrounding framework.

    , stSupportEmail  :: String
    }

type InputValue =
    { value :: String
    , validity :: Validity
    }

type Validity =
    { valueMissing :: Boolean
    , typeMismatch :: Boolean
    , patternMismatch :: Boolean
    , tooLong :: Boolean
    , rangeUnderflow :: Boolean
    , rangeOverflow :: Boolean
    , stepMismatch :: Boolean
    , badInput :: Boolean
    , customError :: Boolean
    , valid :: Boolean
    }

data Query a =
    KeyPressed (State -> State) a
  | UpdateField String (Array FormError) InputValue a
  | UpdateTermsAndConds Boolean a


-- * row show hacks

showState :: State -> String
showState st =intercalate ", "
    [ show st.stErrors
    , show st.stOfInterestNow
    , show st.stLoggedIn
    , show st.stRegSuccess
    , showInputValue st.stName
    , showInputValue st.stEmail
    , showInputValue st.stPass1
    , showInputValue st.stPass2
    , show st.stTermsAndConds
    ]

showInputValue :: InputValue -> String
showInputValue iv = intercalate ":" [iv.value, showValidity iv.validity]

showValidity :: Validity -> String
showValidity v = mconcat
    [ if v.valueMissing    then "*" else "_"
    , if v.typeMismatch    then "*" else "_"
    , if v.patternMismatch then "*" else "_"
    , if v.tooLong         then "*" else "_"
    , if v.rangeUnderflow  then "*" else "_"
    , if v.rangeOverflow   then "*" else "_"
    , if v.stepMismatch    then "*" else "_"
    , if v.badInput        then "*" else "_"
    , if v.customError     then "*" else "_"
    , if v.valid           then "*" else "_"
    ]


-- * initial values

initialState :: State
initialState =
    { stErrors: []
    , stOfInterestNow: []
    , stLoggedIn: false
    , stRegSuccess: false
    , stName: emptyInputValue
    , stEmail: emptyInputValue
    , stPass1: emptyInputValue
    , stPass2: emptyInputValue
    , stTermsAndConds: false
    , stSupportEmail: ""
    }

emptyInputValue :: InputValue
emptyInputValue =
    { value: ""
    , validity: validityOk
    }

validityOk :: Validity
validityOk = {
      valueMissing:    false
    , typeMismatch:    false
    , patternMismatch: false
    , tooLong:         false
    , rangeUnderflow:  false
    , rangeOverflow:   false
    , stepMismatch:    false
    , badInput:        false
    , customError:     false
    , valid:           true
    }


-- * render

render :: State -> ComponentHTML Query
render st = H.div [cl "login"]
    [ H.pre_ [H.p_ [H.text $ showState st]]
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
        [ inputField st P.InputText "TR__USERNAME" "username"
            (\i s -> s { stName = i })
            [ErrorRequiredUsername]

        , inputField st P.InputEmail "TR__EMAIL" "email"
            (\i s -> s { stEmail = i })
            [ErrorRequiredEmail, ErrorFormatEmail]

        , inputField st P.InputPassword "TR__PASSWORD" "password"
            (\i s -> s { stPass1 = i })
            [ErrorForwardPassword, ErrorTooShortPassword]

        , inputField st P.InputPassword "TR__PASSWORD_REPEAT" "password_repeat"
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

-- | The last argument 'ofInterestHere' contains all errors that are reportable near the current
-- field (e.g., all errors related to email address).  The 'State' field 'ofInterestNow' contains a
-- list of all errors that should be reported at this point in time (e.g., excluding email errors
-- because the email field has not been filled out yet).
--
-- FIXME: use recycle translation key lbl as form field key, so we only have to pass one argument
-- instead of two.
inputField :: State -> P.InputType -> String -> String
           -> (InputValue -> State -> State)
           -> Array FormError
           -> ComponentHTML Query
inputField st inputType lbl key updateState ofInterestHere = H.label_
    [ H.span [cl "label-text"] [trh lbl]
    , H.input [ P.inputType inputType, P.name key, P.required true
              , P.required true
              , E.onInput  $ E.input $ KeyPressed <<< updateState <<< eventInputValue
              , E.onChange $ E.input $ UpdateField lbl ofInterestHere <<< eventInputValue
              ]
    , renderErrors st ofInterestHere
    ]

renderErrors :: State -> Array FormError -> ComponentHTML Query
renderErrors st ofInterstHere = H.span [cl "input-error"] $ (trh <<< show) <$> forReporting
  where
    forReporting :: Array FormError
    forReporting = intersect st.stErrors ofInterstHere

-- | ('show' for 'FormError' returns the translation keys.)
instance showFormError :: Show FormError where
    show ErrorRequiredUsername = "TR__ERROR_REQUIRED_USERNAME"
    show ErrorRequiredEmail = "TR__ERROR_REQUIRED_EMAIL"
    show ErrorFormatEmail = "TR__ERROR_FORMAT_EMAIL"
    show ErrorForwardPassword = "TR__ERROR_REQUIRED_PASSWORD"
    show ErrorTooShortPassword = "TR__ERROR_TOO_SHORT_PASSWORD"
    show ErrorMatchPassword = "TR__ERROR_MATCH_PASSWORD"
    show ErrorRequiredTermsAndConditions = "TR__ERROR_REQUIRED_TERMS_AND_CONDITIONS"

-- | there is something about this very similar to `trh`: we want to be able to collect all
-- classnames occurring in a piece of code, and construct a list from them with documentation
-- (source file locations?).
cl :: forall r i. String -> P.IProp (class :: P.I | r) i
cl = P.class_ <<< H.className


-- * eval

eval :: forall eff g. (MonadAff (console :: CONSOLE | eff) g, Functor g)
    => Natural Query (ComponentDSL State Query g)
eval e@(KeyPressed updateState next) = do
    liftAff' $ print "KeyPressed"
    modify updateState
    modify checkState
    pure next
eval e@(UpdateField label es iv next) = do
    liftAff' $ print
        ["UpdateField", label, show es, showInputValue iv]
    modify (\st -> st { stOfInterestNow = union es st.stOfInterestNow })
    modify checkState
    pure next
eval e@(UpdateTermsAndConds newVal next) = do
    liftAff' $ print "UpdateTermsAndConds"
    modify (\st -> st { stTermsAndConds = newVal })
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

checkState :: State -> State
checkState st = st { stErrors = intersect st.stOfInterestNow $ concat
    [ if st.stName.validity.valueMissing      then [ErrorRequiredUsername] else []
    , if st.stEmail.validity.valueMissing     then [ErrorRequiredEmail] else []
    , if st.stEmail.validity.typeMismatch     then [] else [ErrorFormatEmail]
    , if st.stPass1.validity.valueMissing     then [ErrorForwardPassword] else []
    , if length st.stPass1.value >= 6         then [] else [ErrorTooShortPassword]
    , if st.stPass1.value == st.stPass2.value then [] else [ErrorMatchPassword]
    , if st.stTermsAndConds                   then [] else [ErrorRequiredTermsAndConditions]
    ]
  }


-- * main

ui :: forall eff g. (MonadAff (console :: CONSOLE | eff) g, Functor g) => Component State Query g
ui = component render eval

main :: forall eff. String -> Eff (HalogenEffects (console :: CONSOLE | eff)) Unit
main selector = runAff throwException (const (pure unit)) <<< forkAff $ do
    { node: node, driver: driver } <- runUI ui initialState
    appendTo selector node

mainEl :: forall eff. HTMLElement -> Eff (HalogenEffects (console :: CONSOLE | eff)) Unit
mainEl element = runAff throwException (const (pure unit)) <<< forkAff $ do
    { node: node, driver: driver } <- runUI ui initialState
    liftEff $ appendChild (htmlElementToNode element) (htmlElementToNode node)


-- FIXME: widget destruction?
