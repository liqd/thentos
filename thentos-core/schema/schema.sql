-- All statements in this file should be idempotent.
CREATE TABLE IF NOT EXISTS "users" (
    id         bigint      PRIMARY KEY,
    name       text        NOT NULL UNIQUE,
    password   text        NOT NULL,
    email      text        NOT NULL UNIQUE,
    confirmed  bool        NOT NULL,
    created    timestamptz NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS "user_confirmation_tokens" (
    id         bigint      NOT NULL REFERENCES users (id) ON DELETE CASCADE,
    token      text        NOT NULL UNIQUE
);

CREATE TABLE IF NOT EXISTS "password_reset_tokens" (
    token      text        PRIMARY KEY,
    uid        bigint      NOT NULL REFERENCES users (id) ON DELETE CASCADE,
    timestamp  timestamptz NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS "email_change_tokens" (
    token      text        PRIMARY KEY,
    uid        bigint      NOT NULL REFERENCES users (id) ON DELETE CASCADE,
    timestamp  timestamptz NOT NULL DEFAULT now(),
    new_email  text        NOT NULL
);

CREATE TABLE IF NOT EXISTS "user_roles" (
    uid  bigint NOT NULL REFERENCES users (id) ON DELETE CASCADE,
    role text   NOT NULL,
    UNIQUE (uid, role)
);

CREATE TABLE IF NOT EXISTS "services" (
    id          text       PRIMARY KEY,
    owner       bigint     NOT NULL REFERENCES users (id),
    name        text       NOT NULL,
    description text       NOT NULL,
    key         text       NOT NULL
);

CREATE TABLE IF NOT EXISTS "user_sessions" (
    token  text        PRIMARY KEY,
    uid    bigint      NOT NULL REFERENCES users (id) ON DELETE CASCADE,
    start  timestamptz NOT NULL,
    end_   timestamptz NOT NULL,
    period interval    NOT NULL
);

CREATE TABLE IF NOT EXISTS "service_sessions" (
    token                 text        PRIMARY KEY,
    service               text        NOT NULL REFERENCES services (id) ON DELETE CASCADE,
    start                 timestamptz NOT NULL,
    end_                  timestamptz NOT NULL,
    period                interval    NOT NULL,
    thentos_session_token text        NOT NULL REFERENCES user_sessions (token) ON DELETE CASCADE
);
