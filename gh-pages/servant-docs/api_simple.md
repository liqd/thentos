GET `session/:token`
====================

Captures
--------

-   *“token”*: Session Token

Response
--------

-   Status code 200
-   Response body at below
    ``` {.javascript}
    true
    ```

GET `user/:userid/email`
========================

Captures
--------

-   *“userid”*: User ID

Response
--------

-   Status code 200
-   Response body at below
    ``` {.javascript}
    "cobain@nirvana.com"
    ```

DELETE `user/:userid`
=====================

Captures
--------

-   *“userid”*: User ID

Response
--------

-   Status code 204
-   No response body

PUT `user/:userid/name`
=======================

Captures
--------

-   *“userid”*: User ID

Request Body
------------

``` {.javascript}
"Kurt Cobain"
```

Response
--------

-   Status code 200
-   Response body at below
    ``` {.javascript}
    []
    ```

DELETE `session/:token`
=======================

Captures
--------

-   *“token”*: Session Token

Response
--------

-   Status code 204
-   No response body

GET `proxy-test`
================

Response
--------

-   Status code 200
-   No response body

GET `session/:token/login/:sid`
===============================

Captures
--------

-   *“token”*: Session Token
-   *“sid”*: Service ID

Response
--------

-   Status code 200
-   Response body at below
    ``` {.javascript}
    true
    ```

POST `user`
===========

Request Body
------------

``` {.javascript}
{
    "udEmail": "cobain@nirvana.com",
    "udName": "Kurt Cobain",
    "udPassword": "[password hidden]"
}
```

Response
--------

-   Status code 201
-   Response body at below
    ``` {.javascript}
    12
    ```

GET `user/:userid/name`
=======================

Captures
--------

-   *“userid”*: User ID

Response
--------

-   Status code 200
-   Response body at below
    ``` {.javascript}
    "Kurt Cobain"
    ```

POST `service`
==============

Response
--------

-   Status code 201
-   No response body

DELETE `session/:token/login/:sid`
==================================

Captures
--------

-   *“token”*: Session Token
-   *“sid”*: Service ID

Response
--------

-   Status code 204
-   No response body

PUT `user/:userid/email`
========================

Captures
--------

-   *“userid”*: User ID

Request Body
------------

``` {.javascript}
"cobain@nirvana.com"
```

Response
--------

-   Status code 200
-   Response body at below
    ``` {.javascript}
    []
    ```

GET `user`
==========

Response
--------

-   Status code 200
-   Response body at below
    ``` {.javascript}
    [
        3,
        7,
        23
    ]
    ```

GET `service`
=============

Response
--------

-   Status code 200
-   Response body at below
    ``` {.javascript}
    [
        "23t92ege0n",
        "f4ghwgegin0"
    ]
    ```

POST `session/:token/login/:sid`
================================

Captures
--------

-   *“token”*: Session Token
-   *“sid”*: Service ID

Response
--------

-   Status code 201
-   Response body at below
    ``` {.javascript}
    []
    ```

POST `session`
==============

Request Body
------------

``` {.javascript}
12
```

Response
--------

-   Status code 201
-   Response body at below
    ``` {.javascript}
    "abde1234llkjh"
    ```

