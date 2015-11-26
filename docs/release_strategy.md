# Release Strategy

1. Follow the [Haskell
   PVP](https://wiki.haskell.org/Package_versioning_policy).

2. Deviation of 1.: there are only few version constrainst in
   `thentos.cabal`.  Instead, we provide a `cabal.config` that pins
   exactly and transitively all dependencies.  (FUTUREWORK: generate
   `>=A.B.C.D && <A.B` constraints for `thentos.cabal` from
   `cabal.config`.)

3. All release versions have the form `A.B.C`; version `1.8.1` can be
   considered the major release `1.8`. FIXME Shouldn't that be `1.8.0`?

4. Every release is tagged in git (example: `git tag release-0.0.1`).

5. If version `1.8.1` has been released, and then `1.7.3` needs a
   bugfix release `1.7.4`, start a git branch named `release-1.7.3`,
   add commits until satisfied, tag the release commit, and leave it
   on that branch.
