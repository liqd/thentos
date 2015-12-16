# Messaging API

## Messaging Request

Services (such as A3) can use Thentos to send messages to users or arbitrary
email addresses. To send a message, the service sends a POST request with
JSON payload to the `THENTOS_URL/message` endpoint. The payload must be a
JSON object with the following required fields:

* subject: the subject of the message
* body: the plain-text body of the message

The following field is optional:

* html: the HTML-formatted body of the message

The following fields are alternative--exactly one of them must be present:

* target_users: a list of one or more strings where each string is the user
  path of a user. The message will be send to each of these users.
  Implementation note: *users* in a service (such as A3) are *personas* in
  Thentos. In Thentos, a service's notation of a user path is stored as
  *ExternalUrl* of the persona.
* target_email: a list of one or more strings where each string is an email
  address. The message will be send to each of these addresses.

## Messaging Reply

If Thentos could successfully send all emails, it replies with status
200 OK and the following JSON payload:

    {"data": {}}

FIXME Or maybe just send an empty object (`{}`) instead?

If the request was malformed, it replies with status 400 Bad Request and an
error description in JSON format. FIXME Document the typical/expected cases.

Services need to run on a privileged IP in order to be able to send
messages. By default, only localhost is privileged, but that can be changed
by modifying the Thentos config file (FIXME document config setting). If
somebody on a privileged IP address send a POST request to `message`
endpoint, Thentos replies with 401 Unauthorized.

If the messages could not be send due to some internal problem not caused
by the sender, Thentos replies with 500 Internal Server Error.

## Duplicate Handling

Thentos makes sure that a message is sent at most once to each user. If the
`target_users` or `target_email` field contains a user path / email
address several times, the message is rejected with 400 Bad Request and a
suitable error message. FIXME document

Several personas may belong to the same user, but services cannot know
this. If several of the user paths listed in `target_users` belong to the
same user, Thentos sends exactly only copy of the message to the user. This
is not an error.
