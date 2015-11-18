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

Thentos uses [PostgreSQL](http://postgresql.org/) for persistence
(modules `Transaction*`), [lio](https://github.com/scslab/lio) for
information flow control and authorization management (modules
`Action*`), and [servant](http://haskell-servant.github.io/) for rest
APIs (modules `Backend.*`) and for HTML-form-based user interfaces
(modules `Frontend.*`).

Thentos is designed as both a library and an out-of-the-box web
application and service.  You can use any of the parts that work for
you and build something completely different from them:

- write a new database schema derived from the old one and a lens into
  the old one, and reuse all existing transactions and actions on the
  new schema;

- implement your own rest API dialect on top of `Action*` (and, thanks
  to lio, rely on enforcement of the authorization policy implemented
  there, even if your own code is malicious),

- use a collection of application-specific servant handlers and blaze
  combinators to build your own web interfaces.

- ...


Code Structure
--------------

This is a possible quick walk through the code:

- **Thentos.Types**: gives you the core types that describes the
    Thentos data model.

- **Thentos.Transaction...**: SQL queries with 'EitherT'
    exceptions.  `Thentos.Transaction` implements an
    abstract API over the database schema.

- **Thentos.Action...**: access-controlled actions in the `Action` monad.
    `Action` provides access to the database, randomness, and config
    data (feel free to divert to `Thentos.Config` from here, not
    covered in this tour).  Perhaps most importantly, it is not based
    on `IO`, but on `LIO`, which provides information flow and access
    control.  Actions can be composed of transactions and other things
    like reading the system time.  `Thentos.Action` implements an API
    that both frontend and backend use.

- **Thentos.Backend...**: rest APIs based on servant and wai.

- **Thentos.Frontend...**: browser frontend for direct
    user-interaction.


Installation
------------

If you want to use vagrant, visit https://github.com/tarleb/thentos-vagrant.

Tested on [7.10.2](https://www.haskell.org/ghc/download_ghc_7_10_2). You
should be able to build with later ghc versions (if any), but it may
involve some tweaking (and hence some familiarity with ghc).

If your package manager doesn't have a suitable ghc version, you can
download it manually from https://www.haskell.org/ghc/.

Clone the Thentos repository from GitHub. There are several packages in
this repository:

* `thentos-core`: the core package
* `thentos-tests`: tests for `thentos-core`
* `thentos-adhocracy`: integration with the Adhocracy software
* `thentos-purescript`: code to run in the browser
* ...

You'll need to build `thentos-core` in any case. `thentos-tests` is only
required if you want to run tests. Whether you need other packages will
depend on your use case.

To run executables or tests, you will need to install PostgreSQL.
Depending on your setup, you may need to cast some authorization
spells.  Here is what works on debian:

```shell
$ sudo -u postgres createuser thentos -d
$ echo 'alter role thentos superuser' | sudo -u postgres psql
$ export PGUSER=thentos
```

(Instead of `thentos`, you can choose your unix login name as postgres
user name and skip setting the shell variable.)

### Stack

(Will not build purescript; see next section.)

Download [stack](https://github.com/commercialhaskell/stack/wiki/Downloads).
Then run `stack setup` to install the right ghc version, `stack build` to
install and build all packages, and `stack test` to run the tests, all from
this directory.

To build the `thentos-core` executable, build with

```shell
$ stack build --flag thentos-core:with-thentos-executable
```

which will drop it in `.stack-work/install/.../bin/thentos-core`.

### Cabal sandboxes

Before you start, you need to have node, npm, pulp, and purescript
installed.  One way to do this:

```shell
# apt-get install nodejs npm
$ mkdir $HOME/opt
$ cd $HOME/opt
$ npm install pulp
$ npm install purescript
(then add $HOME/opt/node_modules/.bin to your $PATH)
```

We recommend building all required packages into the same sandbox using the
provided script.

```shell
$ misc/thentos-install.sh
```

This will take a while, as it will pull and build a lot of library
dependencies.

The executable will be created as `.cabal-sandbox/bin/thentos`.

To run the tests, change into the `thentos-tests` directory and execute
the following command:

```shell
$ cabal test
```

If you have no selenium grid set up, you can either read
`./misc/selenium/Makefile` and get it to work (see there for more
details and links to the download page), or do without:

```shell
$ cabal test --test-options="--skip selenium"
```

Generated Thentos documentation (thentos-0.0.1) can be found online:

*[FIXME: this is quite outdated!]*

- [servant-docs](https://liqd.github.io/thentos/gh-pages/servant-docs/)
- [haddock](https://liqd.github.io/thentos/gh-pages/haddock/)
- [SourceGraph](https://liqd.github.io/thentos/gh-pages/SourceGraph/thentos.html)


Demo
----

If you built `thentos-core` with flag `with-thentos-executable`, you
can run it from the top leve directory:

```shell
$ createdb thentosdev
$ ./path/to/thentos
```

You can visit the Thentos frontend at http://localhost:7002/, and
log in as god/god.

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


Future Directions
-----------------

- There are some exciting advances in IFC in Haskell: http://www.cse.chalmers.se/~buiras/hlio/, [the mac package](http://hackage.haskell.org/package/mac).
