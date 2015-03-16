Thentos: The swiss army knife of privacy-preserving identity management
=======================================================================

Warning
-------

This software is actively developed, but incomplete (many of the
features promised here are missing) and highly unstable (the existing
code will change in unexpected and possibly undocumented ways).  If
you are interested in using it, please contact us to negotiate a
release plan.

Having that said: enjoy!  (:


Overview
--------

Thentos (/'tentɒs/) is the swiss army knife of web application user
management.  You can:

- use it as a library to offer SSO (single-sign-on) via github or
  facebook to your users,

- run it as a proxy in front of your web application that does all the
  user management for you (a bit like
  [sproxy](https://github.com/zalora/sproxy)),

- set up your own, federated SSO service hierarchy,

- use a third-party SSO service to completely hide user information
  from your application and only work with pre-authenticated,
  anonymised sessions,

- and many other things.

Thentos is based on [acid-state](http://acid-state.seize.it/) for
persistence (modules `DB.*`),
[servant](http://haskell-servant.github.io/) for rest apis (modules
`Backend.*`), and [snap](http://snapframework.com/) for
HTML-form-based user interfaces (modules `Frontend.*`).  While `DB.*`
provides mostly acidic transactions, `Api.*` offers a more high-level
access to the database.  It lives in between frontend/backend and
persistence layer.

We use parts of [lio](https://github.com/scslab/lio) for authorization
management.  Acid transactions are labelled with authorization
expressions that are enforced against clearance expressions (almost)
implicitly.  (We do not use the 'LIO' monad so far, as this requires
changes to both acid-state and servant, and is most relevant when
running a mix of trusted and untrusted Haskell modules.)

Thentos is designed as both a library and an out-of-the-box web
application and service.  You can use any of the parts that work for
you and build something completely different from them:

- implement your own rest api dialect on top of `DB.*` and `Api`;

- write a new database schema derived from the old one and a lens into
  the old one, and reuse all existing transactions on the new schema;

- use special-purpose blaze combinators and handlers from the default
  frontend to build your own.


Installation
------------

Tested on [ghc-7.8.4](https://www.haskell.org/ghc/download_ghc_7_8_4)
with all versions pinned (see `cabal.config`).  You should be able to
build with other ghc versions sind 7.8 and without pinning, but it may
involve some tweaking (and hence some familiarity with ghc).

To build, make sure ghc is in your path and `ghc --version` sais it is
7.8.4.  Then:

```shell
$ cabal sandbox init
$ cabal install --enable-tests --enable-documentation
```

This will take a while, as it will pull and build a lot of library
dependencies.  `--enable-tests` and `--enable-documentation` are
optional (but it will still take a while to build).

To run the tests:

```shell
$ cabal test
$ cabal run -- thentos run --runbackend --runfrontend
```

(If you haven't set up a selenium grid server, some of the tests will
fail.  Read `./misc/selenium/Makefile` for some hints on how to set up
a selenium grid server.)

Benchmarks:

```shell
$ git clone https://github.com/fhartwig/pronk -b barely-working-state
$ cabal sandbox add-source ./pronk
$ cabal install --enable-bench
$ cabal bench  # requires thentos to be running in another shell
```


Demo
----

There is a helloworld service that you can use to test a simple
oauth-like setup where browser and service connect to Thentos in order
to perform user and session management.  Once the session is
established, the browser will talk directly to the service with the
negotiated session token.

Keep Thentos running in a different terminal (see above).

```shell
$ cd services/helloworld/
$ cabal sandbox init
$ cabal install
$ cabal run
```

In order to obtain a service identity for helloworld to authenticate
against Thentos, connect to the [Thentos
frontend](http://localhost:7002/), click on `create_service`, and on
`create_service` again.

Add the information to `services/helloworld/devel.config`, stop the
`cabal run` process, and start it again.

Create a user (use god/god as username/password if you want to skip
this step): visit the [Thentos fronend](http://localhost:7002/) again,
click on `create_user`.  Email confirmation is configured to work if
there is a mail system running that supports email to local users.
Just use user unix user name as email address and hope for the best.
There should also be a line in ./log/thentos.log that contains the
confirmation token (logging needs to be set to `DEBUG`, but that is
currently the hard-wired default).

Visit the [helloworld service](http://localhost:8000/).  You should be
able to log in and out now.

There is also a highly experimental (even more so than the rest of
Thentos) alternative rest api that mimics the
[adhocracy3](https://github.com/liqd/adhocracy3.mercator) backend:

```shell
cabal run -- runa3 --runbackend --runfrontend
curl -XPOST -d '{"name": "god", "password": "god"}' http://localhost:7001/login_username
```

(Try a bad password to run into one of the gaps in the
implementation. :)


Further Reading
---------------

- `./docs/related_work.md`

    An incomplete list of related software projects.

- Generated Thentos documentation (version 0.0.1):
    - [servant-docs](https://liqd.github.io/thentos/gh-pages/servant-docs/)
    - [haddock](https://liqd.github.io/thentos/gh-pages/haddock/)
    - [SourceGraph](https://liqd.github.io/thentos/gh-pages/SourceGraph/thentos.html)


Thanks!
-------

(in alphanumerical order)

- Christian Siefkes
- Julian Arni
- Sönke Hahn
