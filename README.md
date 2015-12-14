# A tool for privacy-preserving identity management (PPIM)


## Status

EXPERIMENTAL.


## Philosophy

Thentos (/'tentɒs/) is the Swiss army knife of web application user
management.  Its focus is on privacy and decentralization of control.
It is actively developed by [liquid democracy e.V.](http://liqd.net/),
a non-profit NGO that has no stakes in user data as a product.  It is
not designed as a closed platform, but for cooperative and autonomous
operation by many independent organisations.

Things we are going to do with the Thentos code base:

- use it as a library to offer Twitter or GitHub SSO to your users,

- run it as a proxy in front of your application that does all the
  user management for you (a bit like
  [sproxy](https://github.com/zalora/sproxy), but not restricted to
  oauth/google+),

- run your own PPIM service or connect it to a federated network of
  PPIM services,

- get rid of user data as a liability, and let some trusted third
  party do authorization and identity management for you,

- distribute user information from your corporate legacy databases to
  your services with minimal exposure.


## Architecture overview and code structure

There are several packages in this repository:

* `thentos-core`: the core package with the base functionality shared
  by most use cases.

* `thentos-tests`: tests for `thentos-core` and common test utility
   functions for derived Thentos packages as a library

* `thentos-adhocracy`: integration with
  [adhocracy3](https://github.com/liqd/adhocracy3)

* `thentos-purescript`: UI widgets (not cabal; highly experimental).

A quick walk through the code of `thentos-core`:

- **Thentos.Types**: the core types of the Thentos data model.

- **Thentos.Transaction...**: SQL queries with 'EitherT' exceptions.
    `Thentos.Transaction` implements an abstract API over the database
    schema.  Thentos persistence is based on
    [PostgreSQL](http://postgresql.org/).

- **Thentos.Action...**: authorization-controlled actions in the
    `Action` monad.  Actions usually involve calling transactions, but
    also access to randomness, system time, configuration options, and
    other things.

    This is where the application logic goes so it can be shared by
    backend and frontend (see below).  `Action` is based on
    [`LIO`](https://github.com/scslab/lio) rather than `IO` and
    provides information flow as well as authorization control.  This
    makes it possible to, say, write a new REST API dialect in a
    [`Safe` module](https://ghc.haskell.org/trac/ghc/wiki/SafeHaskell)
    so that the compiler can generate a proof of the adherence to the
    security policy expressed in the types of the actions.

    `Action` also provides polymorphic `StateT` that is used by the
    frontend for session management.

- **Thentos.Backend...**: REST APIs based on
    [servant](http://haskell-servant.github.io/), wai, warp.  Servant
    allows to organise APIs by features and compose them freely for
    different deployment scenarios.  For example, you can pluck a set
    of user registration end-points and handlers from
    `Thentos.Backend.Api.Simple` and use them in an
    adhocracy-compatible API as an add-on in package
    `thentos-adhocracy`.

- **Thentos.Frontend...**: HTML-based user interface, also based on
    [servant](http://haskell-servant.github.io/), wai, warp (highly
    experimental).

    This contains a prototype of a user management dashboard that can
    be used by application owners and users to manage many
    applications.

    (In principle, servant makes it possible to run the same
    end-points in both backend (delivering JSON) and frontend
    (delivering HTML) mode based on the content-type header.  However,
    in practice there are many differences: REST APIs are stateless,
    but HTML-based UIs have sessions; REST APIs serve self-contained
    pieces of data, but HTML-based UIs deliver pages containing many
    independent bits of information.)


### Other documentation

- [haddock, servant-docs, SourceGraph](https://liqd.github.io/thentos/gh-pages/)


## Installation

Start by cloning the Thentos repository from GitHub.

```shell
$ git clone https://github.com/liqd/thentos
$ cd thentos
$ git submodule update --init
```

You need to have ghc-7.10 and some extra tools installed.  On debian,
you can do this:

```shell
$ sudo bash
# add-apt-repository ppa:hvr/ghc
# apt-get update
# apt-get install ghc-7.10.2 happy-1.19.3 alex-3.1.4 cabal-install-1.22
# apt-get install xvfb  # (for selenium tests; see below).
# apt-get install sox espeak  # (for audio captchas).
```

To run executables or tests, you will need to install PostgreSQL.
Depending on your setup, you may need to cast some authorization
spells.  Here is what works on debian:

```shell
$ export PGUSER=thentos
$ sudo -u postgres createuser $PGUSER -d
$ echo "alter role $PGUSER superuser" | sudo -u postgres psql
```

(Instead of `thentos`, you can choose your unix login name as postgres
user name and skip setting the shell variable.)

For building the purescript UI code, you will need to have a few more
tools installed:

```shell
# apt-get install nodejs npm
$ mkdir $HOME/opt
$ cd $HOME/opt
$ npm install pulp
$ npm install purescript
(then add $HOME/opt/node_modules/.bin to your $PATH)
```

Now run the installation script and the tests:

```shell
$ ./misc/thentos-install.sh
$ cd thentos-tests && cabal test
$ cd thentos-adhocracy && cabal test
```

This will take a while, as it will pull and build a lot of library
dependencies.

Note that the tests require selenium to work.  If you have no selenium
grid set up, you can either read `./misc/selenium/Makefile` and get it
to work (see there for more details and links to the download page),
or do without that part of the test suite:

```shell
$ cabal test --test-options="--skip selenium"
```

If you want to use vagrant to run Thentos in a virtual machine, visit
https://github.com/tarleb/thentos-vagrant.

If you run into any problems, you can check `.travis.yml` on an
up-to-date way of getting all the dependencies installed.


## Related work

Please notify us if you want something to be added.

- [http://barada.sourceforge.net/](http://barada.sourceforge.net/)
- [http://jasig.github.io/cas/](http://jasig.github.io/cas/)
- [http://oauth.net/2/](http://oauth.net/2/)
- [http://openid.net/connect/](http://openid.net/connect/)
- [https://github.com/DeDiS/Dissent](https://github.com/DeDiS/Dissent)
- [https://github.com/zalora/sproxy](https://github.com/zalora/sproxy)
- [http://shibboleth.net/](http://shibboleth.net/)
- [http://www.openldap.org/](http://www.openldap.org/)


## Contributors

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

If you want to get involved or have any questions, we would love to
hear from you! Please also read docs/dev-howtos.md and
docs/styleguide.md to learn more about our development practices and
our coding guidelines for Haskell.


## Future directions

- There are some exciting advances in IFC in Haskell:
  [http://www.cse.chalmers.se/~buiras/hlio/](http://www.cse.chalmers.se/~buiras/hlio/),
  [the mac package](http://hackage.haskell.org/package/mac).
