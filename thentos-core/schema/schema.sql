-- All statements in this file should be idempotent.
CREATE TABLE IF NOT EXISTS "users" (
    id         integer   UNIQUE NOT NULL,
    name       text      UNIQUE NOT NULL,
    password   text,
    email      text      UNIQUE NOT NULL
);
