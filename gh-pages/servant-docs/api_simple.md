PUT `user/:uid/name`
====================

Captures
--------

-   *“uid”*: User ID

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

GET `session/:sid`
==================

Captures
--------

-   *“sid”*: Service ID

Request Body
------------

``` {.javascript}
{
    "fromSessionToken": "abde1234llkjh"
}
```

Response
--------

-   Status code 200
-   Response body at below
    ``` {.javascript}
    true
    ```

GET `user/:uid/email`
=====================

Captures
--------

-   *“uid”*: User ID

Response
--------

-   Status code 200
-   Response body at below
    ``` {.javascript}
    "cobain@nirvana.com"
    ```

DELETE `session/:sid`
=====================

Captures
--------

-   *“sid”*: Service ID

Request Body
------------

``` {.javascript}
{
    "fromSessionToken": "abde1234llkjh"
}
```

Response
--------

-   Status code 204
-   No response body

DELETE `user/:uid`
==================

Captures
--------

-   *“uid”*: User ID

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

POST `session/:sid`
===================

Captures
--------

-   *“sid”*: Service ID

Request Body
------------

``` {.javascript}
{
    "fromSessionToken": "abde1234llkjh"
}
```

Response
--------

-   Status code 201
-   Response body at below
    ``` {.javascript}
    []
    ```

DELETE `session`
================

Request Body
------------

``` {.javascript}
{
    "fromSessionToken": "abde1234llkjh"
}
```

Response
--------

-   Status code 204
-   No response body

PUT `user/:uid/email`
=====================

Captures
--------

-   *“uid”*: User ID

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

DELETE `service/:sid`
=====================

Captures
--------

-   *“sid”*: Service ID

Response
--------

-   Status code 204
-   No response body

POST `user`
===========

Request Body
------------

``` {.javascript}
{
    "udEmail": "cobain@nirvana.com",
    "udName": "Kurt Cobain",
    "udPassword": "Hunter2"
}
```

Response
--------

-   Status code 201
-   Response body at below
    ``` {.javascript}
    12
    ```

POST `service`
==============

Response
--------

-   Status code 201
-   No response body

GET `session`
=============

Request Body
------------

``` {.javascript}
{
    "fromSessionToken": "abde1234llkjh"
}
```

Response
--------

-   Status code 200
-   Response body at below
    ``` {.javascript}
    true
    ```

GET `user/:uid/name`
====================

Captures
--------

-   *“uid”*: User ID

Response
--------

-   Status code 200
-   Response body at below
    ``` {.javascript}
    "Kurt Cobain"
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
        {
            "fromServiceId": "23t92ege0n"
        },
        {
            "fromServiceId": "f4ghwgegin0"
        }
    ]
    ```

POST `session`
==============

Request Body
------------

``` {.javascript}
[
    12,
    "geheim"
]
```

Response
--------

-   Status code 201
-   Response body at below
    ``` {.javascript}
    {
        "fromSessionToken": "abde1234llkjh"
    }
    ```

