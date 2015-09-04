-- All statements in this file should be idempotent.
CREATE TABLE IF NOT EXISTS "users" (
    id         bigint      PRIMARY KEY,
    name       text        UNIQUE NOT NULL,
    password   text        NOT NULL,
    email      text        UNIQUE NOT NULL,
    confirmed  bool        NOT NULL,
    created    timestamptz NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS "user_confirmation_tokens" (
    id         bigint      REFERENCES users (id) ON DELETE CASCADE,
    token      text        UNIQUE NOT NULL
);

CREATE TABLE IF NOT EXISTS "password_reset_tokens" (
    token      text        NOT NULL,
    uid        bigint      NOT NULL REFERENCES users (id) ON DELETE CASCADE,
    timestamp  timestamptz DEFAULT now()
);

CREATE TABLE IF NOT EXISTS "email_change_tokens" (
    token      text        NOT NULL,
    uid        bigint      NOT NULL REFERENCES users (id) ON DELETE CASCADE,
    timestamp  timestamptz DEFAULT now(),
    new_email  text        NOT NULL
);

CREATE TABLE IF NOT EXISTS "user_roles" (
    uid bigint NOT NULL REFERENCES users (id) ON DELETE CASCADE,
    role text NOT NULL,
    UNIQUE (uid, role)
);

CREATE TABLE IF NOT EXISTS "services" (
    id          text       PRIMARY KEY,
    owner       bigint     REFERENCES users (id),
    name        text       NOT NULL,
    description text       NOT NULL,
    key         text       NOT NULL
);
