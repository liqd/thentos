//
// DO NOT EDIT!  THIS IS GENERATED REST API CLIENT CODE!
//
// source package: thentos-adhocracy
// source package version: Version {versionBranch = [0,0,1,1], versionTags = []}
//

var post200PrincipalsUsers = function($http, body)
{
  return $http(
    { url: '/principals/users'
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , method: 'POST'
    });
}



var post200Activate_account = function($http, body)
{
  return $http(
    { url: '/activate_account'
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , method: 'POST'
    });
}



var post200Login_username = function($http, body)
{
  return $http(
    { url: '/login_username'
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , method: 'POST'
    });
}



var post200Login_email = function($http, body)
{
  return $http(
    { url: '/login_email'
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , method: 'POST'
    });
}



var post200Password_reset = function($http, body)
{
  return $http(
    { url: '/password_reset'
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , method: 'POST'
    });
}



var ServiceProxy = function($http)
{
  return $http(
    { url: '/'
    , method: 'GET'
    });
}
