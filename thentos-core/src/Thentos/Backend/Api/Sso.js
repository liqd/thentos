// FIXME: don't just hope that jquery is available!
// FIXME: somehow make the two names defined here less global!  (ask somebody who speaks js.)

/*
 * bind this to the click event of your "log in with github" button.
 */
var requestSso = function(returnUri) {
    var response = $.post("/sso/github/request", { data: { return_uri: returnUri } });
    window.location = response['return_uri'];
};

/*
 * bind this to the return uri.  arguments: uri should be the actual
 * current uri the browser is aimed at when this function is called
 * (this is where the sso credentials provided by github and need to
 * be passed on to thentos are extracted); cb is a function that is
 * called with the session token as an argument.  the application has
 * to make sure that this callback will activate the session token and
 * change the application state to "logged in".
 */
var confirmSso = function(uri, cb) {
    var readStateParam = function(uri) { return ""; };                  // FIXME
    var readCodeParam = function(uri) { return ""; };                   // FIXME

    var d = {
        state: readStateParam(uri),
        code: readCodeParam(uri),
    };

    var tok = $.post("/sso/github/confirm", { data: d });
    cb(tok);
};
