
var requestSso = function() {
    var request_url = post(backend + "/sso/github/request", { data: {} });
    redirect(request_url);
};

// this function is registered under the route of the auth callback url.  that is, when the browser
// is redirected back from github, it will call this function first.
var confirmSso = function() {
    // extract authentication token from route (or from the function arguments)
    // pass to thentos via rest api
    // extract target route and session token from login response from thentos
    // activate session token
    // re-route to target route (something along the lines of `/logged_in/dashboard`)
};
