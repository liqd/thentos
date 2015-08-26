-- All statements in this file should be idempotent.
CREATE TABLE IF NOT EXISTS "users" (
    id         bigint    UNIQUE NOT NULL,
    name       text      UNIQUE NOT NULL,
    password   text,
    email      text      UNIQUE NOT NULL
);

CREATE TABLE IF NOT EXISTS "password_reset_tokens" (
    token      text      NOT NULL,
    uid        integer   REFERENCES users (id),
    timestamp  timestamp DEFAULT now()
);
