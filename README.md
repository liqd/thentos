Thentos: The Swiss army knife of privacy-preserving identity management
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

Thentos (/'tentɒs/) is the Swiss army knife of web application user
management.  You can:

- use it as a library to offer GitHub or Twitter single-sign-on to the
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
`Action*`), [servant](http://haskell-servant.github.io/) for rest APIs
(modules `Backend.*`), and [snap](http://snapframework.com/) for
HTML-form-based user interfaces (modules `Frontend.*`).

Thentos is designed as both a library and an out-of-the-box web
application and service.  You can use any of the parts that work for
you and build something completely different from them:

- write a new database schema derived from the old one and a lens into
  the old one, and reuse all existing transactions and actions on the
  new schema;

- implement your own rest API dialect on top of `Action*` (and, thanks
  to lio, rely on enforcement of the authorization policy implemented
  there, even if your own code is malicious),

- use a collection of application-specific rest API handlers, snap
  handlers, and blaze combinators to build your own web interfaces.

- ...


Code Structure
--------------

This is a possible quick walk through the code:

- **Thentos.Types**: gives you the `DB` type that describes the
    Thentos data model (and some polymorphism over it).

- **Thentos.Transaction...**: acid-state events with 'EitherT'
    exceptions.  `Thentos.Transaction.Transactions` implements an
    abstract API over the `DB` type.

- **Thentos.Action...**: non-acidic actions in the `Action` monad.
    `Action` provides access to acid state, randomness, and config
    data (feel free to divert to `Thentos.Config` from here, not
    covered in this tour).  Perhaps most importantly, it is not based
    on `IO`, but on `LIO`, which provides information flow and access
    control.  Actions can be composed of transactions and other things
    like reading the system time.  `Thentos.Action` implements an API
    that both frontend and backend use.

- **Thentos.Backend...**: rest APIs based on servant and wai.

- **Thentos.Frontend...**: browser frontend for direct
    user-interaction based on snap.


Installation
------------

If you want to use vagrant, visit https://github.com/tarleb/thentos-vagrant.

Tested on [ghc-7.8.4](https://www.haskell.org/ghc/download_ghc_7_8_4)
with all versions pinned (see `cabal.config`).  You should be able to
build with other ghc versions since 7.8 and without pinning, but it may
involve some tweaking (and hence some familiarity with ghc).

But it's best to use ghc 7.8.x, where x >= 4. ghc 7.10 is not yet
supported. If your package manager doesn't have a suitable ghc version, you
can download it manually from https://www.haskell.org/ghc/.

Clone the Thentos repository from GitHub. There are several packages in
this repository:

* `thentos-core`: the core package
* `thentos-tests`: tests for `thentos-core`
* `thentos-adhocracy`: integration with the Adhocracy software
* ...

You'll need to build `thentos-core` in any case. `thentos-tests` is only
required if you want to run tests. Whether you need other packages will
depend on your use case.

### Stack

Download [stack](https://github.com/commercialhaskell/stack/wiki/Downloads).
Then run `stack build` to install and build all packages, and `stack test` to
run the tests.

### Cabal sandboxes

We recommend building all required packages into the same sandbox. To
install `thentos-core`, change into the `thentos-core` directory and
execute the following commands:

```shell
$ cabal sandbox init --sandbox=../.cabal-sandbox
$ cabal install -fwith-thentos-executable --enable-documentation
```

This will take a while, as it will pull and build a lot of library
dependencies.  `--enable-documentation` is optional (but it will take a
while to build no matter what).

Start like this (in interpreted mode):

```shell
$ cabal run thentos
```

To build the tests, change into the `thentos-tests` directory and execute
the following commands:

```shell
$ cabal sandbox init --sandbox=../.cabal-sandbox
$ cabal sandbox add-source ../thentos-core
$ cabal install --enable-tests --enable-documentation
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
If you also want to install the Adhocracy integration, change into the
`thentos-adhocracy` directory and execute the following commands:

```shell
$ cabal sandbox init --sandbox=../.cabal-sandbox
$ cabal sandbox add-source ../thentos-core
$ cabal sandbox add-source ../thentos-tests
$ cabal install --enable-tests --enable-documentation
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
Just use your Unix user name as email address and hope for the best.
There should also be a line in ./log/thentos.log that contains the
confirmation token (logging needs to be set to `DEBUG`, but that is
currently the hard-wired default).

Visit the [helloworld service](http://localhost:8000/).  You should be
able to log in and out now.

There is also a highly experimental (even more so than the rest of
Thentos) alternative rest API that mimics the
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


Contributors
------------

In alphanumerical order.  Please let us know if we forgot to add you,
or if you would like us to link to your GitHub handle / email.

- Albert Krewinkel
- Andres Löh
- Christian Siefkes
- Florian Hartwig
- Julian Arni
- Matthias Fischmann
- Robert Vollmert
- Sönke Hahn

If you want to get involved, you're very welcome! Please read
docs/dev-howtos.md and docs/styleguide.md to learn more about our
development practices and our coding guidelines for Haskell.
