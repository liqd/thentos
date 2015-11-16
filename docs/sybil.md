

# STATUS OF THIS DOCUMENT

DRAFT.  NOT ALL FEATURES MENTIONED ARE FULLY IMPLEMENTED.


# Sybil attack countermeasures

A Sybil attack is an attempt of a user to appear as more than one
physical person.  This document explains how you can use thentos to
counter such attacks.


## Captchas

Captchas are a tool for a web server to distinguish an algorithm from
a human.  This is not directly a sybil attack countermeasuer (the same
human will still be a human if she registers twice), but some sybil
attacks are launched by bot nets.

Even when facing a botnet, captchas are only a weak protection,
though.  First of all, there is an arms race for algorithms that
create challenges that humans are better at solving than machines on
the one hand, and algorithms that solve these challenges better than
humans in the end.

But worse, there is a market for the service of solving captchas for
10ct each or less (example: www.deathbycaptcha.com/).  Companies that
operate in this market can operate seemingly unrelated web sites on
which they present the captchas they receive to their unsuspecting
users.  So the solutions offered by these services are both
indistinguishable in principle from those offered by honest users, and
not more expensive (as the users often do not have to be paid).

Captchas are weak beyond rescue against even moderatly sophisticated
attackers.  The hope is that many attackers in the wild still fall
short of that threshold.  (Traffic analysis should help you to draw
your own conclusions there.)

We have deliberately chosen to not rely on google's recaptcha.  It
collects IP addresses of all your users, and the javascript code that
it loads into your browser is obfuscated and may generate additional
data traces now or in the future.  Furthermore, even though it is
probably slightly harder to crack than our built-in solution, there is
no price difference for those who decide to counter Captcha security
between the two.


## Looking for patterns in registration traffic and blacklists

If, say, there are hundrets of emails creating accounts on the same
afternoon, and they all have the form `anon<a-few-digits>@yahoo.com`,
then something is probably going wrong.

Thentos allows to download tables of the following form via rest
(end-point /analytics/sybil), filtered by time windows:

    1. given user name
    2. email
    3. timestamp

The end-point supports content types text/csv and application/json;
the analysis happens off-line with either office or scripting tools of
your choice.

If the analyst decides that there is a sybil attack, there two
counter-measures are supported:

    1. you can upload a blacklist of email addresses that are not
       accepted for user registration.  registration attempts from
       addresses on any list are silently dropped on the floor.  (this
       comes at the risk of fending off legitimate users, with all the
       usual implications of bad PR, smaller user base, and higher
       support costs.  use this tool carefully!)
       blacklists can be csv or json.  in the latter case, they
       consist of only one row per email address.  `*` means *any
       sequence of characters or nothing*.  if two blacklists are
       uploaded, the second one deletes the first one; to remove any
       blacklists, upload an empty blacklist.  *end-point:
       /blacklist/register*
    2. registration can be disallowed completely; either indefinitely
       (to be manually re-enabled), or for a pre-set time interval in
       hours or minutes.  *end-point: /blacklist/register/global*


## Other approaches (future work)

- bitcoin-style proof of work (could be done in browser; bad UX)
- sms/tan authentication
- paper letter with tan to residential address
- post-ident
- leveraging payment systems for proof of identity
