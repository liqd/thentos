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
[Adhocracy3](https://github.com/liqd/adhocracy3) is the first web app to
use thentos, we are covering the terminology for that, too.  Other web
apps will have different terms and concepts, and this document may be
extended in the future to cover more of them.


### Web App, or App, or Service

These terms are used synonymously.  They refer to the software that
wants users to be managed and interoperates with thentos to get that
job done.

Example: Adhocracy.


### User

A (thentos) user corresponds to a real person using thentos and
associated web apps.

It contains a login name, password, and possibly other data like email
or residential address (more likely) or payment information in various
payment systems (currently less likely).

Interactions between the real person and thentos or a web app are
called actions.  An attempted action may trigger an authorization
error (see below).


### Persona

Motivation: A person may want to assume different identities at
different times.  A simple way to achieve that would be to create many
users, but on the other hand some things like residential address or
system notifications, should be shared between those identities.

'Persona' is a technical concept to meet these needs.  Depending on
the specifics, the UI may look very different, ranging from "user has
an anonymity button" to "admin manages privacy setting in a
configuration file".  This document is only about the technical terms,
not how they are exposed to the user.

A persona is like a login name, but one user can have a list of many personas.
Web app can be set up to have access to the persona, but not the user name
or any of the other data stored with the user.

A persona is something intended direct for manipulation by the user (the
real person, not the structure).  There may be a pull-down menu behind
the "you are logged in as..." button the upper right corner that lets
you pick which persona you want to be.

There are two related use cases:

- *Pseudonymity:* Consider a discussion forum web app.  In some
  contributions, a user may want to use her real name to leverage or
  increase her reputation in the community.  In others, possibly even
  under the same discussion, she may want to remain anonymous.  This can
  be achieved by using two personas: one identical to the user's real name,
  and one generated at random.

- *Doing different things in the same place:* Example: If the same user
  enters a channel where she acts as moderator, she wants to have
  different controls in the UI from ordinary users (e.g., she doesn't
  want to be able to accidentally write a comment, but she needs to be
  able to delete comments of others).
  This can be achieved by registering several personas with the same web
  app, and having them assigned different persona groups (see below).

There are many possible security requirements involving personas, from
secrecy of specific information about the user (usually the user wants
that) to authenticity and uniqueness of the anonymized user (such as
sybil attack, or sock puppet countermeasures; usually the web app
wants that).


### Context, or App Context

A context is an opaque string that identifies something in the app
that determines persona selection.

Example: An app is hosting a number of discussion threads
simultaneously, and users are to create one new persona for each
thread.

There is a mapping from `(user, app, context)` to `persona` that
allows the system to do this.

NOTE: This mapping must not be managed by the app, because that would
expose the user id to it, which would break the pseudonymity use case
of personas.


### User Group

A user group is a set that contains users and other user groups.

Motivation: User groups are needed for giving permissions to many
users in one atomic operation.  An example for why groups of groups
can be useful: consider the groups `user`, `confirmed user`, `admin`.
An `admin` is a `confirmed user` with something extra.  Groups of
groups allow us to construct a group by expressing that directly.


### Action

FIXME.


### Authorization check

FIXME: explain the idea of `assertAuth`, `isUser`, `hasRole`.


### Information flow control (IFC)

FIXME.


### Permission

Permissions are not used as a concept in thentos.  Instead, we use a
more powerful notion of information flow control (IFC), and,
implemented in terms of IFC, the notion of authorization checks.  See
above.


### Persona Group

Persona groups are very similar to user groups.  In fact, while they still live
inside thentos, they are the equivalent of user groups for web apps.  Where
user groups are about users, persona groups are about personas.  And where a user group is
valid in a thentos installation globally, a persona group is local to a web
app registered with thentos.  (If thentos only serves one web app,
this is not much of a distinction.  But if there are many web apps
sharing the same thentos server, each of them can manage their own
groups.)


### User (Adhocracy3)

This corresponds both to thentos user (in the sense that adhocracy
does the same things with it that thentos does), and to thentos
persona (in the sense that there is a one-on-one mapping).

See
https://cms.liqd.net/intranet/copy_of_projekte/adhocracy-3/grundkonzept-und-terminologie#section-14


### Group (Adhocracy3)

Has the same double-correspondence from thentos user groups and
thentos persona groups.

See
https://cms.liqd.net/intranet/copy_of_projekte/adhocracy-3/grundkonzept-und-terminologie#section-16


### Process Role (Adhocracy3)

Time-aware set of adhocracy permissions.  Thentos does not need to
(and therefore should not) know about adhocracy roles.

Example: "group X has permission Y in process phase Z."

See
https://cms.liqd.net/intranet/copy_of_projekte/adhocracy-3/grundkonzept-und-terminologie#section-17


### Permission (Adhocracy3)

Like thentos permission.  See
https://cms.liqd.net/intranet/copy_of_projekte/adhocracy-3/grundkonzept-und-terminologie#section-19.


## FIXME:

should we make this an alphabetical list?

what about these terms?:

- identification
- qualification (in the sense of berlin servicekonto)
