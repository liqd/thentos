## The thentos API family: Core

Version {versionBranch = [0,0,1,1], versionTags = []}

## Overview

`Core` is a simple, general-purpose user management protocol
that supports using one identity for multiple services.  It has
all the expected basic features like email confirmation, password
reset, change of user data.  Furthermore, it allows to create services,
register users with services, and manage the user's service login
sessions.


## Request Headers

If a request has an unknown header with prefix "X-Thentos-".


## Authentication

To call any of this API's endpoints as a User or Service,
your request has to contain an HTTP header with the name
'X-Thentos-Session' and with the value set to a valid session
token.


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

## GET /service

#### delete a service and unregister all its users


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
[]
```

- 

```javascript
[
    {
        "fromServiceId": "S6F4dLfwPiR6NcsrKjgHJBYh"
    }
]
```

## POST /service

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
[
    12,
    {
        "fromServiceName": "Evil Corp."
    },
    {
        "fromServiceDescription": "Making the worse a little better every day."
    }
]
```

#### Response:

- Status code 201
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
[
    {
        "fromServiceId": "S6F4dLfwPiR6NcsrKjgHJBYh"
    },
    {
        "fromServiceKey": "HeU3QCNovwDDgbAqbb+tW1e2"
    }
]
```

## DELETE /service/:sid

#### Captures:

- *sid*: service ID

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
[]
```

## DELETE /service_session

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{
    "fromServiceSessionToken": "kvs8FFo6aisUAX+p4ESMMO5Q"
}
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
[]
```

## GET /service_session

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{
    "fromServiceSessionToken": "kvs8FFo6aisUAX+p4ESMMO5Q"
}
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
false
```

- 

```javascript
true
```

## GET /service_session/meta

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{
    "fromServiceSessionToken": "kvs8FFo6aisUAX+p4ESMMO5Q"
}
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{
    "srvSessMdUser": "alice"
}
```

## DELETE /thentos_session

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
"gFfrBSRVY2s415QIEs+xcS9n"
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
[]
```

## GET /thentos_session

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
"gFfrBSRVY2s415QIEs+xcS9n"
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
false
```

- 

```javascript
true
```

## POST /thentos_session

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{
    "user": [
        12,
        "secret"
    ]
}
```

#### Response:

- Status code 201
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
"gFfrBSRVY2s415QIEs+xcS9n"
```

## POST /user

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{
    "udEmail": "alice@example.com",
    "udName": "alice",
    "udPassword": "secret"
}
```

#### Response:

- Status code 201
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
12
```

## DELETE /user/:uid

#### Captures:

- *uid*: user ID

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
[]
```

## GET /user/:uid/email

#### Captures:

- *uid*: user ID

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
"alice@example.com"
```

## GET /user/:uid/name

#### Captures:

- *uid*: user ID

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
"alice"
```

## POST /user/login

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{
    "ldName": "alice",
    "ldPassword": "secret"
}
```

#### Response:

- Status code 201
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
"gFfrBSRVY2s415QIEs+xcS9n"
```

