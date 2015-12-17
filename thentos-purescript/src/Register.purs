module Register where

import Prelude

import Control.Monad.Aff.Class (MonadAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Aff (runAff, forkAff, Aff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE())
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)
import Control.Monad.Free
import Control.Monad (unless)
import Data.Array (filter, concat, intersect, union)
import Data.Generic
import Data.Maybe
import Data.String (length, trim, null)
import Data.Tuple
import DOM.HTML.Types (HTMLElement(), htmlElementToNode)
import DOM.Node.Node (appendChild)
import Global (encodeURIComponent)
import Halogen (Component(), ComponentHTML(), ComponentDSL(), HalogenEffects(), Natural(),
                runUI, component, modify, get, liftAff')
import Halogen.Util (appendTo)
import Network.HTTP.Affjax (AJAX(), AffjaxResponse(), affjax, defaultRequest)
import Network.HTTP.Method (Method(POST))
import Network.HTTP.RequestHeader (RequestHeader(RequestHeader))
import Network.HTTP.ResponseHeader (responseHeader, responseHeaderName, responseHeaderValue)
import Network.HTTP.StatusCode (StatusCode(StatusCode))

import qualified Data.ArrayBuffer.Types as AB
import qualified Data.StrMap as StrMap
import qualified Data.URI.Query as URI
import qualified Data.URI.Scheme as URI
import qualified Data.URI.Types as URI
import qualified Halogen.HTML.Core as H
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.HTML.Events.Types as E
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.Query as Q

import Error

import qualified I18n as I18n
import qualified I18n.Lang as I18n


foreign import eventInputValue :: forall fields. E.Event fields -> InputValue
foreign import arrayBufferToBase64 :: AB.ArrayBuffer -> String


-- * types

type Effs eff = HalogenEffects (console :: CONSOLE, ajax :: AJAX | eff)

type State eff =
    { stErrors        :: Array FormError
    , stServerErrors  :: Array String  -- can't be translated (yet) and need different app logic
    , stOfInterestNow :: Array FormError
    , stLang          :: I18n.Lang
    , stName          :: InputValue
    , stEmail         :: InputValue
    , stPass1         :: InputValue
    , stPass2         :: InputValue
    , stTermsAndConds :: Boolean
    , stCaptchaQ      :: Maybe (AffjaxResponse AB.ArrayBuffer)
    , stCaptchaA      :: InputValue
    , stConfig        :: StateConfig eff
    }

type StateConfig eff =
    { cfgBackendUrl      :: String
    , cfgLoggedIn        :: Boolean
    , cfgRegSuccess      :: Boolean
    , cfgSupportEmail    :: String
    , cfgOnRefresh       :: Aff eff Unit
        -- ^ trigger function for update loop of surrounding framework
    , cfgOnCancel        :: Aff eff Unit
        -- ^ usually: leave register page/state and return to referrer page/state
    , cfgOnGoLogin       :: Aff eff Unit
        -- ^ allow user to login instead of register
    , cfgOnTermsAndConds :: Aff eff Unit
        -- ^ show terms and conditions page
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

data Query eff a =
    KeyPressed (State eff -> State eff) a
  | UpdateField String (Array FormError) InputValue a
  | UpdateTermsAndConds Boolean a
  | ClickSubmit a
  | ClickOther String (Aff eff Unit) a
  | LoadNewCaptcha a
  | CaptchaKeyPressed InputValue a
  | ChangeLanguage I18n.Lang a


-- * initial values

initialState :: forall eff. StateConfig eff -> State eff
initialState cfg =
    { stErrors: []
    , stServerErrors: []
    , stOfInterestNow: []
    , stLang: I18n.EN
    , stName: emptyInputValue
    , stEmail: emptyInputValue
    , stPass1: emptyInputValue
    , stPass2: emptyInputValue
    , stTermsAndConds: false
    , stCaptchaQ: Nothing
    , stCaptchaA: emptyInputValue
    , stConfig: cfg
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

render :: forall eff. State eff -> ComponentHTML (Query eff)
render st = I18n.trH st.stLang $ render' st

render' :: forall eff. State eff -> ComponentHTML (Query eff)
render' st = H.div [cl "login"]
    [ H.pre [cl "thentos-pre"] [H.text $ stringify st]
        -- FIXME: remove state dump if not in debug mode.
    , H.pre [cl "thentos-pre"] [H.text $ "server errors: " <> stringify st.stServerErrors]
        -- FIXME: render server errors more user-friendly.

    , case Tuple st.stConfig.cfgLoggedIn st.stConfig.cfgRegSuccess of
        Tuple false false -> bodyIncompleteForm st
        Tuple false true  -> bodyRegisterSuccess
        Tuple true  _     -> bodyLogin

    , H.div [cl "login-info"]
        [ H.text "TR__REGISTRATION_SUPPORT"
        , H.br_
        , let address = st.stConfig.cfgSupportEmail
              subject = "Trouble with registration"
          in H.a [P.href $ renderEmailUrl address subject] [H.text address]
        ]
    ]

renderEmailUrl :: String -> String -> String
renderEmailUrl "" _ = throwJS "renderEmailUrl: no address."
renderEmailUrl address subject =
    URI.printScheme (URI.URIScheme "mailto") <> address <>
        URI.printQuery (URI.Query (StrMap.singleton "subject" (Just (encodeURIComponent subject))))

-- | registration form empty or incomplete.
bodyIncompleteForm :: forall eff. State eff -> ComponentHTML (Query eff)
bodyIncompleteForm st = H.form [cl "login-form", P.name "registerForm"] $
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
            [ H.input [ P.inputType P.InputCheckbox
                      , P.name "registerCheck"
                      , P.required true
                      , E.onChecked $ E.input $ UpdateTermsAndConds
                      ]
            , H.span_ [H.a [onHrefClick "terms-and-conditions" st.stConfig.cfgOnTermsAndConds]
                [H.text "TR__I_ACCEPT_THE_TERMS_AND_CONDITIONS"]]
            , renderErrors st [ErrorRequiredTermsAndConditions]
            ]
        ]

    , H.div [cl "thentos-captcha"]
        [ case st.stCaptchaQ of
            Just resp | resp.status == StatusCode 201
                -> H.img [P.src ("data:image/png;base64," <> arrayBufferToBase64 resp.response)]
            Just resp
                -> H.text $ "[captcha image: " <> show resp.status <> "]"
            Nothing
                -> H.text "[captcha image: nothing]"
        , H.input
            [ P.inputType P.InputText
            , P.name "thentos-captcha-guess"
            , P.required true
            , E.onInput  $ E.input $ CaptchaKeyPressed <<< eventInputValue
            , E.onChange $ E.input $
                UpdateField "captcha-guess" [ErrorRequiredCaptchaSolution] <<< eventInputValue
            ]
        , renderErrors st [ErrorRequiredCaptchaSolution]
        ]

    , H.input
        [ P.inputType P.InputSubmit, P.name "register", P.value (I18n.trS st.stLang "TR__REGISTER")
        , P.disabled $ not $ Data.Array.null st.stErrors
        , onClickExclusive $ E.input_ $ ClickSubmit
        ]
    , H.div [cl "login-info"]
        [H.p_
            [H.a [onHrefClick "login" st.stConfig.cfgOnGoLogin]
                [H.text "TR__REGISTRATION_LOGIN_INSTEAD"]]]

    , H.a [cl "login-cancel", onHrefClick "cancel" st.stConfig.cfgOnCancel]
        [H.text "TR__CANCEL"]
    ]

-- | registered successfully, waiting for processing of activation email.
bodyRegisterSuccess :: forall eff. ComponentHTML (Query eff)
bodyRegisterSuccess = H.form [cl "login-form", P.name "registerForm"]
    [ H.div [cl "login-success"]
        [ H.h2_ [H.text "TR__REGISTER_SUCCESS"]
        , H.p_ [H.text "TR__REGISTRATION_CALL_FOR_ACTIVATION"]  -- FIXME: link
        ]
    ]

-- | registered and confirmation; proceed to login.
bodyLogin :: forall eff. ComponentHTML (Query eff)
bodyLogin = H.div [cl "login-success"]
    [ H.h2_ [H.text "TR__REGISTRATION_THANKS_FOR_REGISTERING"]
    , H.p_ [H.text "TR__REGISTRATION_PROCEED"]  -- FIXME: link
    ]

-- | The last argument 'ofInterestHere' contains all errors that are reportable near the current
-- field (e.g., all errors related to email address).  The 'State' field 'ofInterestNow' contains a
-- list of all errors that should be reported at this point in time (e.g., excluding email errors
-- because the email field has not been filled out yet).
inputField :: forall eff.
              State eff -> P.InputType -> String -> String
           -> (InputValue -> State eff -> State eff)
           -> Array FormError
           -> ComponentHTML (Query eff)
inputField st inputType lbl key updateState ofInterestHere = H.label_
    [ H.span [cl "label-text"] [H.text lbl]
    , H.input [ P.inputType inputType
              , P.name key
              , P.required true
              , E.onInput  $ E.input $ KeyPressed <<< updateState <<< eventInputValue
              , E.onChange $ E.input $ UpdateField lbl ofInterestHere <<< eventInputValue
              ]
    , renderErrors st ofInterestHere
    ]

onHrefClick :: forall eff e.
      String -> Aff eff Unit -> P.IProp (onClick :: P.I | e) (Query eff Unit)
onHrefClick lbl handler = onClickExclusive (E.input_ (ClickOther lbl handler))

onClickExclusive :: forall eff e.
      (E.Event E.MouseEvent -> E.EventHandler (Query eff Unit))
    -> P.IProp (onClick :: P.I | e) (Query eff Unit)
onClickExclusive handler = E.onClick $ \domEv -> do
    E.preventDefault
    E.stopPropagation
    E.stopImmediatePropagation
    handler domEv

renderErrors :: forall eff. State eff -> Array FormError -> ComponentHTML (Query eff)
renderErrors st ofInterstHere = H.span [cl "input-error"] $ (H.text <<< show) <$> forReporting
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
    show ErrorRequiredCaptchaSolution = "TR__ERROR_REQUIRED_CAPTCHA_SOLUTION"

-- | there is something about this very similar to `trh`: we want to be able to collect all
-- classnames occurring in a piece of code, and construct a list from them with documentation
-- (source file locations?).
cl :: forall r i. String -> P.IProp (class :: P.I | r) i
cl = P.class_ <<< H.className


-- * eval

logEvent :: forall eff g a. (MonadAff (Effs eff) g)
    => Query (Effs eff) a -> ComponentDSL (State (Effs eff)) (Query (Effs eff)) g String
logEvent = liftAff' <<< log <<< stringify

eval :: forall eff g. (MonadAff (Effs eff) g)
    => Natural (Query (Effs eff)) (ComponentDSL (State (Effs eff)) (Query (Effs eff)) g)

eval e@(KeyPressed updateState next) = do
    logEvent e
    modify updateState
    modify checkState
    pure next

eval e@(UpdateField _ es iv next) = do
    logEvent e
    modify (\st -> st { stOfInterestNow = union es st.stOfInterestNow })
    modify checkState
    pure next

eval e@(UpdateTermsAndConds newVal next) = do
    logEvent e
    modify (\st -> st
        { stTermsAndConds = newVal
        , stOfInterestNow = union [ErrorRequiredTermsAndConditions] st.stOfInterestNow
        })
    modify checkState
    pure next

eval e@(ClickSubmit next) = do
    logEvent e
    modify (\st -> st { stOfInterestNow = allFormErrors })
    modify checkState
    st <- get

    unless (st.stErrors /= [] || st.stServerErrors /= []) $ do
        resp <- liftAff' $ doSubmit st
        case resp.status of
            StatusCode i | i >= 200 && i <= 204 -> do  -- success.
                modifyConfig $
                    \cfg -> cfg { cfgRegSuccess = true }
            StatusCode code -> do  -- server error.
                modify $
                    \st -> st { stServerErrors = [show code ++ " " ++ show resp.response]
                                        <> st.stServerErrors }
            -- FIXME: server errors must be translated into widget errors so they can be
            -- displayed where they live.
            -- FIXME: throw LoadNewCaptcha event if resp. server error suggests it.
    pure next

eval e@(ClickOther lbl handler next) = do
    logEvent e
    liftAff' handler
    pure next

eval e@(LoadNewCaptcha next) = do
    logEvent e
    st <- get
    response <- liftAff' $ affjax $ defaultRequest
        { method = POST
        , url = st.stConfig.cfgBackendUrl ++ "/user/captcha"
        , headers = []
        }
    modify (_ { stCaptchaQ = Just (fixResponse response) })
    pure next

eval e@(CaptchaKeyPressed ival next) = do
    logEvent e
    modify (\st -> st
        { stOfInterestNow = union [ErrorRequiredCaptchaSolution] st.stOfInterestNow
        , stCaptchaA = ival
        })
    pure next

eval e@(ChangeLanguage lang next) = do
    logEvent e
    modify (\st -> st { stLang = lang })
    modify checkState
    pure next

modifyConfig :: forall eff f g.
    (StateConfig eff -> StateConfig eff) -> Free (Q.HalogenF (State eff) f g) Unit
modifyConfig f = modify (\st -> st { stConfig = f st.stConfig })


-- * submit

type SubmitFormBody =
    { ucCaptcha :: { csId :: String, csSolution :: String }
    , ucUser :: { udName :: String, udEmail :: String, udPassword :: String }
    }

doSubmit :: forall eff. State (Effs eff) -> Aff (Effs eff) (AffjaxResponse String)
doSubmit st = affjax $ defaultRequest
    { method = POST
    , url = st.stConfig.cfgBackendUrl ++ "/user/register"
    , headers = [RequestHeader "content-type" "application/json"]
    , content = Just (stringify submitBody)
    }
  where
    submitBody :: SubmitFormBody
    submitBody =
        { ucCaptcha:
            { csId: csId
            , csSolution: st.stCaptchaA.value
            }
        , ucUser:
            { udName: st.stName.value
            , udEmail: st.stEmail.value
            , udPassword: st.stPass1.value
            }
        }

    csId :: String
    csId = case st.stCaptchaQ of
        (Just q) -> case filter ((== "Thentos-Captcha-Id") <<< responseHeaderName) q.headers of
            [x] -> responseHeaderValue x
            _ -> ""
        Nothing -> ""


-- * form errors

data FormError =
      ErrorRequiredUsername
    | ErrorRequiredEmail
    | ErrorFormatEmail
    | ErrorForwardPassword
    | ErrorTooShortPassword
    | ErrorMatchPassword
    | ErrorRequiredTermsAndConditions
    | ErrorRequiredCaptchaSolution

derive instance genericFormError :: Generic FormError
instance eqFormError :: Eq FormError where eq = gEq

-- | (There is Data.Enum, but no Data.Bounded.  It looks all less useful than the Haskell stuff.)
allFormErrors :: Array FormError
allFormErrors =
    [ ErrorRequiredUsername
    , ErrorRequiredEmail
    , ErrorFormatEmail
    , ErrorForwardPassword
    , ErrorTooShortPassword
    , ErrorMatchPassword
    , ErrorRequiredTermsAndConditions
    , ErrorRequiredCaptchaSolution
    ]

checkState :: forall eff. State eff -> State eff
checkState st = st { stErrors = intersect st.stOfInterestNow $ concat
    [ if null st.stName.value                 then [ErrorRequiredUsername]           else []
    , if null st.stEmail.value                then [ErrorRequiredEmail]              else []
    , if st.stEmail.validity.typeMismatch     then [ErrorFormatEmail]                else []
    , if null st.stPass1.value                then [ErrorForwardPassword]            else []
    , if length st.stPass1.value < 6          then [ErrorTooShortPassword]           else []
    , if st.stPass1.value /= st.stPass2.value then [ErrorMatchPassword]              else []
    , if not st.stTermsAndConds               then [ErrorRequiredTermsAndConditions] else []
    , if null st.stCaptchaA.value             then [ErrorRequiredCaptchaSolution]    else []
    ]
  }


-- * main

ui :: forall eff g. (MonadAff (Effs eff) g) => Component (State (Effs eff)) (Query (Effs eff)) g
ui = component render eval

main :: forall eff. Maybe (StateConfig (Effs eff)) -> String -> Eff (Effs eff) Unit
main mCfg selector = main' mCfg $ appendTo selector

mainEl :: forall eff. Maybe (StateConfig (Effs eff)) -> HTMLElement -> Eff (Effs eff) Unit
mainEl mCfg parent = main' mCfg $ \child -> do
    liftEff $ appendChild (htmlElementToNode child) (htmlElementToNode parent)

main' :: forall eff a.
    Maybe (StateConfig (Effs eff)) -> (HTMLElement -> Aff (Effs eff) a) -> Eff (Effs eff) Unit
main' mCfg addToDOM = runAff throwException (const (pure unit)) <<< forkAff $ do
    { node: node, driver: driver } <- runUI ui (initialState cfg)
    addToDOM node
    driver $ Q.action LoadNewCaptcha
  where
    cfg = fromMaybe defaultStateConfig mCfg

-- | FIXME: affjax returns header values with trailing `\r`.  As a work-around, this function trims
-- all header values.
fixResponse :: forall a. AffjaxResponse a -> AffjaxResponse a
fixResponse resp = resp { headers = f <$> resp.headers }
  where
    f h = responseHeader (responseHeaderName h) (trim (responseHeaderValue h))

defaultStateConfig :: forall eff. StateConfig eff
defaultStateConfig =
    { cfgBackendUrl      : ""
    , cfgLoggedIn        : false
    , cfgRegSuccess      : false
    , cfgSupportEmail    : "nobody@example.com"
    , cfgOnRefresh       : warnJS "triggered cfgOnRefresh"       $ pure unit  -- FIXME: where do we need to call this?  explain!
    , cfgOnCancel        : warnJS "triggered cfgOnCancel"        $ pure unit
    , cfgOnGoLogin       : warnJS "triggered cfgOnLogin"         $ pure unit
    , cfgOnTermsAndConds : warnJS "triggered cfgOnTermsAndConds" $ pure unit
    }
