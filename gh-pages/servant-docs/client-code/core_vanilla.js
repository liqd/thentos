//
// DO NOT EDIT!  THIS IS GENERATED REST API CLIENT CODE!
//
// source package: thentos-core
// source package version: Version {versionBranch = [0,0,1,1], versionTags = []}
//

var postUser = function(body, headerxthentossession, onSuccess, onError)
{
  var xhr = new XMLHttpRequest();
  xhr.open('POST', '/user', true);
  xhr.setRequestHeader("x-thentos-session", headerxthentossession);
  xhr.onreadystatechange = function (e) {
    if (xhr.readyState == 4) {
        var value = JSON.parse(xhr.responseText);
      if (xhr.status == 200 || xhr.status == 201) {
        onSuccess(value);
      } else {
        onError(value);
      }
    }
  }
  xhr.send(JSON.stringify(body)
);
}

var postUserLogin = function(body, headerxthentossession, onSuccess, onError)
{
  var xhr = new XMLHttpRequest();
  xhr.open('POST', '/user/login', true);
  xhr.setRequestHeader("x-thentos-session", headerxthentossession);
  xhr.onreadystatechange = function (e) {
    if (xhr.readyState == 4) {
        var value = JSON.parse(xhr.responseText);
      if (xhr.status == 200 || xhr.status == 201) {
        onSuccess(value);
      } else {
        onError(value);
      }
    }
  }
  xhr.send(JSON.stringify(body)
);
}

var deleteUserByUid = function(uid, headerxthentossession, onSuccess, onError)
{
  var xhr = new XMLHttpRequest();
  xhr.open('DELETE', '/user/' + encodeURIComponent(uid) + '', true);
  xhr.setRequestHeader("x-thentos-session", headerxthentossession);
  xhr.onreadystatechange = function (e) {
    if (xhr.readyState == 4) {
        var value = JSON.parse(xhr.responseText);
      if (xhr.status == 200 || xhr.status == 201) {
        onSuccess(value);
      } else {
        onError(value);
      }
    }
  }
  xhr.send(null);
}

var getUserByUidName = function(uid, headerxthentossession, onSuccess, onError)
{
  var xhr = new XMLHttpRequest();
  xhr.open('GET', '/user/' + encodeURIComponent(uid) + '/name', true);
  xhr.setRequestHeader("x-thentos-session", headerxthentossession);
  xhr.onreadystatechange = function (e) {
    if (xhr.readyState == 4) {
        var value = JSON.parse(xhr.responseText);
      if (xhr.status == 200 || xhr.status == 201) {
        onSuccess(value);
      } else {
        onError(value);
      }
    }
  }
  xhr.send(null);
}

var getUserByUidEmail = function(uid, headerxthentossession, onSuccess, onError)
{
  var xhr = new XMLHttpRequest();
  xhr.open('GET', '/user/' + encodeURIComponent(uid) + '/email', true);
  xhr.setRequestHeader("x-thentos-session", headerxthentossession);
  xhr.onreadystatechange = function (e) {
    if (xhr.readyState == 4) {
        var value = JSON.parse(xhr.responseText);
      if (xhr.status == 200 || xhr.status == 201) {
        onSuccess(value);
      } else {
        onError(value);
      }
    }
  }
  xhr.send(null);
}

var postService = function(body, headerxthentossession, onSuccess, onError)
{
  var xhr = new XMLHttpRequest();
  xhr.open('POST', '/service', true);
  xhr.setRequestHeader("x-thentos-session", headerxthentossession);
  xhr.onreadystatechange = function (e) {
    if (xhr.readyState == 4) {
        var value = JSON.parse(xhr.responseText);
      if (xhr.status == 200 || xhr.status == 201) {
        onSuccess(value);
      } else {
        onError(value);
      }
    }
  }
  xhr.send(JSON.stringify(body)
);
}

var deleteServiceBySid = function(sid, headerxthentossession, onSuccess, onError)
{
  var xhr = new XMLHttpRequest();
  xhr.open('DELETE', '/service/' + encodeURIComponent(sid) + '', true);
  xhr.setRequestHeader("x-thentos-session", headerxthentossession);
  xhr.onreadystatechange = function (e) {
    if (xhr.readyState == 4) {
        var value = JSON.parse(xhr.responseText);
      if (xhr.status == 200 || xhr.status == 201) {
        onSuccess(value);
      } else {
        onError(value);
      }
    }
  }
  xhr.send(null);
}

var getService = function(headerxthentossession, onSuccess, onError)
{
  var xhr = new XMLHttpRequest();
  xhr.open('GET', '/service', true);
  xhr.setRequestHeader("x-thentos-session", headerxthentossession);
  xhr.onreadystatechange = function (e) {
    if (xhr.readyState == 4) {
        var value = JSON.parse(xhr.responseText);
      if (xhr.status == 200 || xhr.status == 201) {
        onSuccess(value);
      } else {
        onError(value);
      }
    }
  }
  xhr.send(null);
}

var postThentos_session = function(body, headerxthentossession, onSuccess, onError)
{
  var xhr = new XMLHttpRequest();
  xhr.open('POST', '/thentos_session', true);
  xhr.setRequestHeader("x-thentos-session", headerxthentossession);
  xhr.onreadystatechange = function (e) {
    if (xhr.readyState == 4) {
        var value = JSON.parse(xhr.responseText);
      if (xhr.status == 200 || xhr.status == 201) {
        onSuccess(value);
      } else {
        onError(value);
      }
    }
  }
  xhr.send(JSON.stringify(body)
);
}

var getThentos_session = function(body, headerxthentossession, onSuccess, onError)
{
  var xhr = new XMLHttpRequest();
  xhr.open('GET', '/thentos_session', true);
  xhr.setRequestHeader("x-thentos-session", headerxthentossession);
  xhr.onreadystatechange = function (e) {
    if (xhr.readyState == 4) {
        var value = JSON.parse(xhr.responseText);
      if (xhr.status == 200 || xhr.status == 201) {
        onSuccess(value);
      } else {
        onError(value);
      }
    }
  }
  xhr.send(JSON.stringify(body)
);
}

var deleteThentos_session = function(body, headerxthentossession, onSuccess, onError)
{
  var xhr = new XMLHttpRequest();
  xhr.open('DELETE', '/thentos_session', true);
  xhr.setRequestHeader("x-thentos-session", headerxthentossession);
  xhr.onreadystatechange = function (e) {
    if (xhr.readyState == 4) {
        var value = JSON.parse(xhr.responseText);
      if (xhr.status == 200 || xhr.status == 201) {
        onSuccess(value);
      } else {
        onError(value);
      }
    }
  }
  xhr.send(JSON.stringify(body)
);
}

var getService_session = function(body, headerxthentossession, onSuccess, onError)
{
  var xhr = new XMLHttpRequest();
  xhr.open('GET', '/service_session', true);
  xhr.setRequestHeader("x-thentos-session", headerxthentossession);
  xhr.onreadystatechange = function (e) {
    if (xhr.readyState == 4) {
        var value = JSON.parse(xhr.responseText);
      if (xhr.status == 200 || xhr.status == 201) {
        onSuccess(value);
      } else {
        onError(value);
      }
    }
  }
  xhr.send(JSON.stringify(body)
);
}

var getService_sessionMeta = function(body, headerxthentossession, onSuccess, onError)
{
  var xhr = new XMLHttpRequest();
  xhr.open('GET', '/service_session/meta', true);
  xhr.setRequestHeader("x-thentos-session", headerxthentossession);
  xhr.onreadystatechange = function (e) {
    if (xhr.readyState == 4) {
        var value = JSON.parse(xhr.responseText);
      if (xhr.status == 200 || xhr.status == 201) {
        onSuccess(value);
      } else {
        onError(value);
      }
    }
  }
  xhr.send(JSON.stringify(body)
);
}

var deleteService_session = function(body, headerxthentossession, onSuccess, onError)
{
  var xhr = new XMLHttpRequest();
  xhr.open('DELETE', '/service_session', true);
  xhr.setRequestHeader("x-thentos-session", headerxthentossession);
  xhr.onreadystatechange = function (e) {
    if (xhr.readyState == 4) {
        var value = JSON.parse(xhr.responseText);
      if (xhr.status == 200 || xhr.status == 201) {
        onSuccess(value);
      } else {
        onError(value);
      }
    }
  }
  xhr.send(JSON.stringify(body)
);
}
