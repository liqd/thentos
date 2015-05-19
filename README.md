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


Unstable dependencies
---------------------

You need to add a few sources not yet available from hackage to your
cabal sandbox:

```bash
$ git clone https://github.com/zerobuzz/servant-pandoc -b may-fix-issue-2
$ cabal sandbox add-source servant-pandoc
# only required if you want to run benchmarks
$ git clone https://github.com/liqd/pronk -b thentos-patches
$ cabal sandbox add-source pronk
```


Overview
--------

Thentos (/'tentɒs/) is the swiss army knife of web application user
management.  You can:

- use it as a library to offer github or twitter single-sign-on to the
  users of your application,

- run it as a proxy in front of your application that does all the
  user management for you (a bit like
  [sproxy](https://github.com/zalora/sproxy), but with more use cases
  in mind),

- run your own, federated SSO service hierarchy,

- protect your application against any information about your users
  and let some third party that your users trust do authorization
  management for you,

- distribute user information from your corporate legacy databases to
  your services with minimal exposure.

- ...

Thentos uses [acid-state](http://acid-state.seize.it/) for persistence
(modules `Transaction*`), [lio](https://github.com/scslab/lio) for
information flow control and authorization management (modules
`Action*`), [servant](http://haskell-servant.github.io/) for rest apis
(modules `Backend.*`), and [snap](http://snapframework.com/) for
HTML-form-based user interfaces (modules `Frontend.*`).

Thentos is designed as both a library and an out-of-the-box web
application and service.  You can use any of the parts that work for
you and build something completely different from them:

- write a new database schema derived from the old one and a lens into
  the old one, and reuse all existing transactions and actions on the
  new schema;

- implement your own rest api dialect on top of `Action*` (and, thanks
  to lio, rely on enforcement of the authorization policy implemented
  there, even if your own code is malicious),

- use a collection of application-specific rest api handlers, snap
  handlers, and blaze combinators to build your own web interfaces.

- ...


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
$ cabal install --enable-tests --enable-documentation --dependencies-only
```

This will take a while, as it will pull and build a lot of library
dependencies.  `--enable-tests` and `--enable-documentation` are
optional (but it will take a while to build no matter what).

Start like this (in interpreted mode):

```shell
$ cabal run -- thentos run --runbackend --runfrontend
```

To run the tests:

```shell
$ cabal test
```

If you have no selenium grid set up, you can either read
`./misc/selenium/Makefile` and get it to work (see there for more
details and links to the download page), or do without:

```shell
$ cabal test --test-options="--skip selenium"
```

Benchmarks:

```shell
$ cabal install --enable-bench
$ cabal bench  # requires thentos to be running in another shell
```

Generated Thentos documentation (thentos-0.0.1) can be found online:

*[FIXME: this is quite outdated!]*

- [servant-docs](https://liqd.github.io/thentos/gh-pages/servant-docs/)
- [haddock](https://liqd.github.io/thentos/gh-pages/haddock/)
- [SourceGraph](https://liqd.github.io/thentos/gh-pages/SourceGraph/thentos.html)


Demo
----

*[FIXME: this section is outdated!]*

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


Related Work
------------

Please notify us if you want something to be added.

- [http://barada.sourceforge.net/](http://barada.sourceforge.net/)
- [http://jasig.github.io/cas/](http://jasig.github.io/cas/)
- [http://oauth.net/2/](http://oauth.net/2/)
- [http://openid.net/connect/](http://openid.net/connect/)
- [https://github.com/DeDiS/Dissent](https://github.com/DeDiS/Dissent)
- [https://github.com/zalora/sproxy](https://github.com/zalora/sproxy)
- [http://shibboleth.net/](http://shibboleth.net/)
- [http://www.openldap.org/](http://www.openldap.org/)


Thanks!
-------

In alphanumerical order.  Please let us know if we forgot to add you.

- Christian Siefkes
- Julian Arni
- Sönke Hahn
