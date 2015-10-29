# Thentos Terminology

## Introduction

Let's start with explaining the obvious.

Users, names, roles, groups, persmissions, etc. are names for rather
abstract concepts.  As it goes with abstraction, having a good
intuition of their practical meaning can be hard.  Different software
systems use different abstractions and thus use the same names with
different meaning without always being aware of the differences.

This document is attempt to establish common terminology and make it
accessible for everybody who gets in contact with thentos.

Unless stated otherwise, all terms relate to concepts as implemented
in thentos.  Having that said, as
https://github.com/liqd/adhocracy3.mercator is the first web app to
use thentos, we are covering the terminology for that, too.  Other web
apps will have different terms and concepts, and this document may be
extended in the future to cover more of them.


### User

A (thentos) user corresponds to a real person using thentos and
associated web apps.

It contains a login name, password, and possibly other data like email
or residential address (more likely) or payment information in various
payment systems (currently less likely).

Interactions between the real person and thentos or a web app are
called actions.  An attempted action may trigger an authorization
error (see below).


### Nym

FIXME: used to be called persona in thentos, too.  fisx likes 'nym'
better: less overloading, shorter, prettier.  should we have a vote on
renaming it?  other options: pseudonym, nick, identity.

A person may want to assume different identities at different times.
A simple way to achieve that would be to create many users, but on the
other hand some things like residential address or system
notifications, should be shared between those identities.

A nym is like a login name, but one user can have a list of many nyms.
Web app can be set up to have access to the nym, but not the user name
or any of the other data stored with the user.

A nym is something intended direct for manipulation by the user (the
real person, not the structure).  There may be a pull-down menu behind
the "you are logged in as..." button the upper right corner that lets
you pick which nym you want to be.

There are two related use cases:

*Pseudonymity:* Consider a discussion forum web app.  In some
contributions, a user may want to use her real name to leverage or
increase her reputation in the community.  In others, possibly even
under the same discussion, she may want to remain anonymous.  This can
be achieved by using two nyms: one identical to the user's real name,
and one generated at random.

*The Moderator who also wants to engage in a debate:* If the same user
enters a channel where she acts as moderator, she wants to have
different controls in the UI from ordinary users (e.g., she doesn't
want to be able to accidentally write a comment, but she needs to be
able to delete comments of others).

This can be achieved by registering several nyms with the same web
app, and having them assigned different roles (see below).

FIXME: not sure if nyms are the right tool to achieve this moderator /
contributor double-role distinctions.  perhaps roles alone can do that
when exposed to the UI?

FIXME: see also:
https://cms.liqd.net/intranet/copy_of_projekte/adhocracy-3/grundkonzept-und-terminologie#section-15


### Role

A role is a set that contains users and other roles.


### Permission

A permission is just a bit that tells you if an action is allowed or
not.  A permission can be assigned to a user or a role.

If permission X is assigned to role Q, then all users member in role Q
are allowed to perform action X.


### Group

Groups are very similar to roles.  In fact while they still live
inside thentos, they are the equivalent of roles in web apps.  Where
roles are about users, groups are about nyms.  And where a role is
valid in a thentos installation globally, a group is local to a web
app registered with thentos.  (If thentos only serves one web app,
this is not much of a distinction.  But if there are many web apps
sharing the same thentos server, each of them can manage their own
groups.)


### User (Adhocracy3)

See https://cms.liqd.net/intranet/copy_of_projekte/adhocracy-3/grundkonzept-und-terminologie#section-14


### Group (Adhocracy3)

See https://cms.liqd.net/intranet/copy_of_projekte/adhocracy-3/grundkonzept-und-terminologie#section-16


### Process Role (Adhocracy3)

See https://cms.liqd.net/intranet/copy_of_projekte/adhocracy-3/grundkonzept-und-terminologie#section-17


### Permission (Adhocracy3)

See https://cms.liqd.net/intranet/copy_of_projekte/adhocracy-3/grundkonzept-und-terminologie#section-19.


## FIXME:

should we make this an alphabetical list?

what about these terms?:

- authorization error
- authorization
- identification
- qualification (in the sense of berlin servicekonto)
