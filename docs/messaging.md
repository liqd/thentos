# Messaging API

See also issue #472.

## Messaging Request

Services (such as A3) can use Thentos to send messages to users or arbitrary
email addresses. To send a message, the service sends a POST request with
JSON payload to the `THENTOS_URL/email` endpoint. The payload must be a
JSON object with the following required fields:

* subject: the subject of the message
* body: the plain-text body of the message
* recipient: a JSON object described thereafter

The following field is optional:

* html: the HTML-formatted body of the message

The recipient is a JSON object with the following optional fields:

* personas: a list of strings where each string is the user
  path of a user. The message will be sent to each of these users.
  Implementation note: *users* in a service (such as A3) are *personas* in
  Thentos. In Thentos, a service's notation of a user path is stored as
  *ExternalUrl* of the persona.
* emails: a list of strings where each string is an email address.
  The message will be sent to each of these addresses. A single
  string value containing one email address is also accepted.
* groups: a list of strings where each string is a group of personas
  (called *ServiceGroup* in Thentos). The message will be sent to
  every member of the group, including the members of the sub-groups
  and so recursively.

## Messaging Reply

If Thentos could successfully send all emails, it replies with status
204 OK and thus no payload.

If the request was malformed, it replies with status 400 Bad Request and an
error description in JSON format. FIXME Document the typical/expected cases.

Services need to run on a privileged IP in order to be able to send
messages. By default, only localhost is privileged, but that can be changed
by modifying the Thentos config setting called `allow_ips`. If
somebody from a non-privileged IP address sends a POST request to the `message`
endpoint, Thentos replies with 401 Unauthorized.

If the messages could not be sent due to some internal problem not caused
by the sender, Thentos replies with 500 Internal Server Error.

FIXME: if the error lies beyond thentos in the smtp world, i'm not
sure 500 is the right one.  perhaps then it depends on the smtp error?

## Duplicate Handling

Thentos makes sure that a message is sent at most once to each user.

If one recipient is listed more than once (either directly or via
different personas belonging to the same user, or groups that the same
user is a member of), the email is only sent once.  This behavior
guarantees that the service does not learn about which personas belong
to the same user.
