# Thentos Terminology

## Introduction

Let's start with explaining the obvious.

Users, names, roles, groups, persmissions, etc. are names for rather
abstract concepts.  As it goes with abstraction, having a good
intuition of their practical meaning can be hard.  Different software
systems use different abstractions and thus use the same names with
different meaning without always being aware of the differences.

This document is attempt to establish common terminology and make it
accessible for everybody who gets in contact with thentos.  In order
to minimize forward references, it is organized semantically, not
alphabetically.

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


### Action

A user can perform out actions.  An action is characterized by a
result value extracted from thentos and/or connected web apps, and the
effect that it has on them.

Everything the user does is at some point, under the hood, an action.
An action may carry out authorization checks in order to decide
whether a user is allowed to perform it (see above).


### Information flow control (IFC)

IFC can be used to express simple permissions, but is more powerful
than that.  For example, when programming an action, it lets express
logic along the following lines:

    - user X requests the list of connected web apps;
    - service Y gets notified that a user with group Y wants to view
      its profile;
    - service Y clears its profile for 'can be viewed by X'.
    - user X gets a response.

Obviously, this can be expressed in any web framework.  The benefit of
a good IFC system is that it lets you do so in a very concise and
robust way.

Thentos uses [lio](https://hackage.haskell.org/package/lio) for IFC.


### Authorization check

When writing an action, the programmer can check things like that the
performing user is logged in, has a given user id, or is part of a
user group.

If the check fails, an authorization error occurs.  This can be quite
harmless and handled without appearing as an error on the user
interface.  For example, if the user mis-spells their user name during
login, a good login system will catch the authorization error and
present a deliberately vague message: "user name *or* password are
wrong", to make it harder to guess and confirm user names.

In thentos, auth checks are implemented in terms of IFC.


### Permission

A common notion of permission is that of an object that groups an
action (the thing that is allowed to to) and another object (the thing
that it is allowed to do it to).  Permissions can be assigned to users
(or groups, see below) to express that the user may perform an action
on an object.

In thentos, permissions do not occur as a concept.  Instead, we use a
more powerful notions of information flow control and authorization
checks (see above).


### Identification

Identification is the process of recording user attributes or
disseminating them from thentos to web apps.  Examples for user
attributes are user name, residential address, or persona name.


### Qualification

Qualification is the process of establishing that the identification
information is valid.  Examples: confirmation by link in email;
captchas; sms-tan; registration with government-issued electronic
identity card.


### Authentication

Authentication is the process of establishing that an identification
is reliable.  Password checks or tan-generator devices are
authentication mechanisms.  Authentication is conceptually different
from identification, but the two really only make sense together.


### User Group

A user group is a set that contains users and other user groups.

Motivation: User groups are needed for giving permissions to many
users in one atomic operation.  An example for why groups of groups
can be useful: consider the groups `user`, `confirmed user`, `admin`.
An `admin` is a `confirmed user` with something extra.  Groups of
groups allow us to construct a group by expressing that directly.


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
