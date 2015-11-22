## The thentos API family: Adhocracy3 Proxy

Version {versionBranch = [0,0,1,1], versionTags = []}

## Overview

Adhocracy3 has a basic user management built-in.  In order for thentos
to have minimal impact on the existing code base, it can be deployed
as a reverse proxy and mimic the built-in user management rest api.
This way, the frontend does not need to change at all to use the old
features of the new user management system.  The impact of new
features to the frontend can be kept at a minimum.

What follows is the fully compatible adhocracy3 user management rest
api.  Any deviation should be considered an error and reported in a
later version of this document.


## Authenticating Proxy

All requests that are not handled by the endpoints listed
below are handled as follows:
We extract the Thentos Session Token (X-Thentos-Session) from
the request headers and forward the request to the service, adding
X-Thentos-User and X-Thentos-Groups with the appropriate
data to the request headers. If the request does not include
a valid session token, it is rejected. Responses from the
service are returned unmodified.


## POST /activate_account

#### email-click account activation

The confirmation email contains a link to this end-point.
The path contains a token can only be learned from receiving
(or intercepting) the email.


#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{
    "path": "/activate/MXe13lTh8Tr_UaqL6hcRIeGo"
}
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Success

```javascript
{
    "status": "success",
    "user_path": "somepath",
    "user_token": "sometoken"
}
```

## GET /docs/js

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- empty

```

```

## GET /docs/md

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- empty

```

```

## GET /docs/ng

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- empty

```

```

## GET /docs/purs/:ModuleName

#### Captures:

- *string*: purescript module name

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- empty

```

```

## GET /docs/purs/Util.js

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- empty

```

```

## GET /docs/purs/Util.purs

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- empty

```

```

## GET /js

#### Response:

- Status code 200
- Headers: []

- No response body

## POST /login_email

#### login with user email


#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{
    "name": "alice",
    "password": "secret"
}
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Success

```javascript
{
    "status": "success",
    "user_path": "somepath",
    "user_token": "sometoken"
}
```

## POST /login_username

#### login with user name


#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{
    "name": "alice",
    "password": "secret"
}
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Success

```javascript
{
    "status": "success",
    "user_path": "somepath",
    "user_token": "sometoken"
}
```

## POST /password_reset

#### reset password


#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{
    "password": "secret",
    "path": "/proposals/environment"
}
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Success

```javascript
{
    "status": "success",
    "user_path": "somepath",
    "user_token": "sometoken"
}
```

## POST /principals/users

#### request creation of a new account

When the user-creation form is filled out with login name, email, and
password, this end-point is used to post the form content and trigger
the email confirmation procedure.


#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{
    "content_type": "adhocracy_core.resources.principal.IUser",
    "data": {
        "adhocracy_core.sheets.principal.IPasswordAuthentication": {
            "password": "secret"
        },
        "adhocracy_core.sheets.principal.IUserBasic": {
            "name": "alice"
        },
        "adhocracy_core.sheets.principal.IUserExtended": {
            "email": "alice@example.com"
        }
    }
}
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
{
    "content_type": "adhocracy_core.resources.principal.IUser",
    "path": "/proposals/environment",
    "updated_resources": {
        "changed_descendants": [],
        "created": [],
        "modified": [],
        "removed": []
    }
}
```

- 

```javascript
{
    "content_type": "adhocracy_core.resources.principal.IUser",
    "path": "/proposals/environment",
    "updated_resources": {
        "changed_descendants": [],
        "created": [],
        "modified": [],
        "removed": [
            "/proposals/environment"
        ]
    }
}
```

