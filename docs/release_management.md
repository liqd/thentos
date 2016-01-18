# Release Strategy

This document is most relevant for maintainers.


## Versioning

Update versions with `./misc/bump-version.sh`.  The script makes sure
that cabal files (package versions and dependencies) and git tags are
all in sync.  It also prints information on how to publish a new
release.

The version name must obey these rules:

1. Follow the [Haskell
   PVP](https://wiki.haskell.org/Package_versioning_policy).

2. All release versions have the form `A.B.C`; new B-level or A-level
   releases start with zeroz (2.8.0, 3.0.0).

3. Every release is tagged in git like this: `git tag v0.0.1`.

4. If version `1.8.1` has been released, and then `1.7.3` needs a
   bugfix release `1.7.4`: start a git branch named `v1.7.3`,
   add commits until satisfied, tag the release commit, and leave it
   on that branch.


## Binary release distribution

`./misc/release/` contains scripts for building release tar balls from
tagged and uploaded releases.  See shell code for the specifics.

We use github for distributing binary release tarballs once created.
To be specific:

1. Push the new release commit and tag as instructed in the output of
`./misc/bump-version.sh`.

2. Visit https://github.com/liqd/thentos/releases/

3. Click on `Draft new release`.

4. Name tag of new release in the corresponding field; fill in the
details.

5. Attach tar.xz and hash files.

6. Click on `Publish release`.

See https://help.github.com/articles/about-releases/ for further info.
