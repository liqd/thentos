/* global exports */
"use strict";

// module I18n
exports.tr = function(key) {
    return table[lang][key];
};

var lang = "en";

var table = {};

table.en = {
    "TR__CANCEL": "cancel",
    "TR__EMAIL": "email",
    "TR__ERROR_FORMAT_EMAIL": "Email format is incorrect.",
    "TR__ERROR_MATCH_PASSWORD": "Passwords do not match.",
    "TR__ERROR_REQUIRED_EMAIL": "Email is required.",
    "TR__ERROR_REQUIRED_PASSWORD": "Password is required.",
    "TR__ERROR_REQUIRED_TERMS_AND_CONDITIONS": "You must agree to terms and conditions",
    "TR__ERROR_REQUIRED_CAPTCHA_SOLUTION": "Please enter the keywords displayed above",
    "TR__ERROR_REQUIRED_USERNAME": "Username is required",
    "TR__ERROR_TOO_SHORT_PASSWORD": "Password is too short.",
    "TR__I_ACCEPT_THE_TERMS_AND_CONDITIONS": "I accept the [[link:terms and conditions]].",
    "TR__PASSWORD": "password",
    "TR__PASSWORD_REPEAT": "password repeat",
    "TR__REGISTER": "register",
    "TR__REGISTER_SUCCESS": "Thank you for your registration. We sent you an email with an activation link. In case you do not recieve that email, please also check your spam folder.",
    "TR__REGISTRATION_CALL_FOR_ACTIVATION": "Once you have clicked on the activation link you can proceed using {{siteName}}.",
    "TR__REGISTRATION_LOGIN_INSTEAD": "I already have an account. Log in [[link:here]].",
    "TR__REGISTRATION_PROCEED": "Proceed back to [[link:{{siteName}}]].",
    "TR__REGISTRATION_SUPPORT": "Having trouble with the registration?",
    "TR__REGISTRATION_THANKS_FOR_REGISTERING": "Hi, {{userName}}! Thanks for registering with {{siteName}}.",
    "TR__USERNAME": "username",
};

table.de = {
    "TR__CANCEL": "abbrechen",
    "TR__EMAIL": "E-Mail",
    "TR__ERROR_FORMAT_EMAIL": "E-Mail-Format ist ungültig.",
    "TR__ERROR_MATCH_PASSWORD": "Passwörter stimmen nicht überein",
    "TR__ERROR_REQUIRED_EMAIL": "E-Mail erforderlich",
    "TR__ERROR_REQUIRED_PASSWORD": "Passwort erforderlich",
    "TR__ERROR_REQUIRED_TERMS_AND_CONDITIONS": "Bitte stimme den Nutzungsbedingungen zu.",
    "TR__ERROR_REQUIRED_CAPTCHA_SOLUTION": "Bitte die Schlüsselwörter eingeben",
    "TR__ERROR_REQUIRED_USERNAME": "Nutzername erforderlich",
    "TR__ERROR_TOO_SHORT_PASSWORD": "Das Passwort ist zu kurz.",
    "TR__I_ACCEPT_THE_TERMS_AND_CONDITIONS": "Ich akzeptiere die [[link:Nutzungsbedingungen]].",
    "TR__PASSWORD": "Passwort",
    "TR__PASSWORD_REPEAT": "Passwort Wiederholung",
    "TR__REGISTER": "registrieren",
    "TR__REGISTER_SUCCESS": "Danke für Deine Registrierung. Wir haben Dir eine E-Mail mit einem Aktivierungslink gesendet. Falls Du diese E-Mail nicht erhältst, überprüfe bitte auch Deinen Spamordner.",
    "TR__REGISTRATION_CALL_FOR_ACTIVATION": "Sobald Du auf den Aktivierungslink geklickt hast, kannst Du {{siteName}} benutzen.",
    "TR__REGISTRATION_LOGIN_INSTEAD": "Ich habe bereits einen Account. [[link:Hier]] anmelden.",
    "TR__REGISTRATION_PROCEED": "Gehe zurück zu [[link:{{siteName}}]].",
    "TR__REGISTRATION_SUPPORT": "Probleme mit der Registrierung?",
    "TR__REGISTRATION_THANKS_FOR_REGISTERING": "Hi, {{userName}}! Danke für Deine Anmeldung bei {{siteName}}.",
    "TR__USERNAME": "Nutzername",
};
