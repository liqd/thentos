-- All statements in this file should be idempotent.
CREATE TABLE IF NOT EXISTS "account" (
    id         text   NOT NULL,
    name       text   NOT NULL,
    password   text,
    email      text   NOT NULL
);
