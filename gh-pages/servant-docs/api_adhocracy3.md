POST `login_email`
==================

Request Body
------------

``` {.javascript}
{
    "name": "wef",
    "password": "[password hidden]"
}
```

Response
--------

-   Status code 201
-   Response body at below
    ``` {.javascript}
    {
        "status": "success",
        "user_path": "/proposals/environment",
        "token": "abde1234llkjh"
    }
    ```

POST `login_username`
=====================

Request Body
------------

``` {.javascript}
{
    "name": "wef",
    "password": "[password hidden]"
}
```

Response
--------

-   Status code 201
-   Response body at below
    ``` {.javascript}
    {
        "status": "success",
        "user_path": "/proposals/environment",
        "token": "abde1234llkjh"
    }
    ```

POST `principals/users`
=======================

Request Body
------------

``` {.javascript}
{
    "data": {
        "adhocracy_core.sheets.principal.IPasswordAuthentication": {
            "password": "[password hidden]"
        },
        "adhocracy_core.sheets.principal.IUserBasic": {
            "email": "cobain@nirvana.com",
            "name": "Kurt Cobain"
        }
    },
    "content_type": "adhocracy_core.resources.principal.IUser"
}
```

Response
--------

-   Status code 201
-   Response body at below
    ``` {.javascript}
    {
        "path": "/proposals/environment",
        "data": {
            "adhocracy_core.sheets.principal.IUserBasic": {
                "email": "cobain@nirvana.com",
                "name": "Kurt Cobain"
            }
        },
        "content_type": "adhocracy_core.resources.principal.IUser"
    }
    ```

POST `activate_account`
=======================

Request Body
------------

``` {.javascript}
{
    "path": "/proposals/environment"
}
```

Response
--------

-   Status code 201
-   Response body at below
    ``` {.javascript}
    {
        "status": "success",
        "user_path": "/proposals/environment",
        "token": "abde1234llkjh"
    }
    ```

GET ``
======

Response
--------

-   Status code 200
-   No response body
