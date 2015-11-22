//
// DO NOT EDIT!  THIS IS GENERATED REST API CLIENT CODE!
//
// source package: thentos-adhocracy
// source package version: Version {versionBranch = [0,0,1,1], versionTags = []}
//

var post200PrincipalsUsers = function(body, onSuccess, onError)
{
  var xhr = new XMLHttpRequest();
  xhr.open('POST', '/principals/users', true);
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

var post200Activate_account = function(body, onSuccess, onError)
{
  var xhr = new XMLHttpRequest();
  xhr.open('POST', '/activate_account', true);
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

var post200Login_username = function(body, onSuccess, onError)
{
  var xhr = new XMLHttpRequest();
  xhr.open('POST', '/login_username', true);
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

var post200Login_email = function(body, onSuccess, onError)
{
  var xhr = new XMLHttpRequest();
  xhr.open('POST', '/login_email', true);
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

var post200Password_reset = function(body, onSuccess, onError)
{
  var xhr = new XMLHttpRequest();
  xhr.open('POST', '/password_reset', true);
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

var ServiceProxy = function(onSuccess, onError)
{
  var xhr = new XMLHttpRequest();
  xhr.open('GET', '/', true);
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
