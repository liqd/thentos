//
// DO NOT EDIT!  THIS IS GENERATED REST API CLIENT CODE!
//
// source package: thentos-core
// source package version: Version {versionBranch = [0,0,1,1], versionTags = []}
//

var postUser = function($http, body, headerxthentossession)
{
  return $http(
    { url: '/user'
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , headers: { "x-thentos-session": headerxthentossession }
    , method: 'POST'
    });
}



var postUserLogin = function($http, body, headerxthentossession)
{
  return $http(
    { url: '/user/login'
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , headers: { "x-thentos-session": headerxthentossession }
    , method: 'POST'
    });
}



var deleteUserByUid = function($http, uid, headerxthentossession)
{
  return $http(
    { url: '/user/' + encodeURIComponent(uid) + ''
    , headers: { "x-thentos-session": headerxthentossession }
    , method: 'DELETE'
    });
}



var getUserByUidName = function($http, uid, headerxthentossession)
{
  return $http(
    { url: '/user/' + encodeURIComponent(uid) + '/name'
    , headers: { "x-thentos-session": headerxthentossession }
    , method: 'GET'
    });
}



var getUserByUidEmail = function($http, uid, headerxthentossession)
{
  return $http(
    { url: '/user/' + encodeURIComponent(uid) + '/email'
    , headers: { "x-thentos-session": headerxthentossession }
    , method: 'GET'
    });
}



var postService = function($http, body, headerxthentossession)
{
  return $http(
    { url: '/service'
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , headers: { "x-thentos-session": headerxthentossession }
    , method: 'POST'
    });
}



var deleteServiceBySid = function($http, sid, headerxthentossession)
{
  return $http(
    { url: '/service/' + encodeURIComponent(sid) + ''
    , headers: { "x-thentos-session": headerxthentossession }
    , method: 'DELETE'
    });
}



var getService = function($http, headerxthentossession)
{
  return $http(
    { url: '/service'
    , headers: { "x-thentos-session": headerxthentossession }
    , method: 'GET'
    });
}



var postThentos_session = function($http, body, headerxthentossession)
{
  return $http(
    { url: '/thentos_session'
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , headers: { "x-thentos-session": headerxthentossession }
    , method: 'POST'
    });
}



var getThentos_session = function($http, body, headerxthentossession)
{
  return $http(
    { url: '/thentos_session'
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , headers: { "x-thentos-session": headerxthentossession }
    , method: 'GET'
    });
}



var deleteThentos_session = function($http, body, headerxthentossession)
{
  return $http(
    { url: '/thentos_session'
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , headers: { "x-thentos-session": headerxthentossession }
    , method: 'DELETE'
    });
}



var getService_session = function($http, body, headerxthentossession)
{
  return $http(
    { url: '/service_session'
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , headers: { "x-thentos-session": headerxthentossession }
    , method: 'GET'
    });
}



var getService_sessionMeta = function($http, body, headerxthentossession)
{
  return $http(
    { url: '/service_session/meta'
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , headers: { "x-thentos-session": headerxthentossession }
    , method: 'GET'
    });
}



var deleteService_session = function($http, body, headerxthentossession)
{
  return $http(
    { url: '/service_session'
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , headers: { "x-thentos-session": headerxthentossession }
    , method: 'DELETE'
    });
}
