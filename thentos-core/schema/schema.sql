-- All statements in this file must be idempotent (it is called at start time, every time).
CREATE TABLE IF NOT EXISTS users (
    id         bigserial   PRIMARY KEY,
    name       text        NOT NULL UNIQUE,
    password   text        NOT NULL,
    email      text        NOT NULL UNIQUE,
    confirmed  bool        NOT NULL,
    created    timestamptz NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS user_confirmation_tokens (
    id         bigint      NOT NULL REFERENCES users (id) ON DELETE CASCADE,
    token      text        NOT NULL UNIQUE,
    timestamp  timestamptz NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS password_reset_tokens (
    token      text        PRIMARY KEY,
    uid        bigint      NOT NULL REFERENCES users (id) ON DELETE CASCADE,
    timestamp  timestamptz NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS email_change_tokens (
    token      text        PRIMARY KEY,
    uid        bigint      NOT NULL REFERENCES users (id) ON DELETE CASCADE,
    timestamp  timestamptz NOT NULL DEFAULT now(),
    new_email  text        NOT NULL
);

CREATE TABLE IF NOT EXISTS user_roles (
    uid  bigint NOT NULL REFERENCES users (id) ON DELETE CASCADE,
    role text   NOT NULL,
    UNIQUE (uid, role)
);

CREATE TABLE IF NOT EXISTS services (
    id            text       PRIMARY KEY,
    owner_user    bigint     NOT NULL REFERENCES users (id),
    name          text       NOT NULL,
    description   text       NOT NULL,
    key           text       NOT NULL
);

CREATE TABLE IF NOT EXISTS service_roles (
    sid         text       REFERENCES services (id) ON DELETE CASCADE,
    role        text       NOT NULL,
    UNIQUE (sid, role)
);

CREATE TABLE IF NOT EXISTS user_services (
    uid       bigint NOT NULL REFERENCES users (id)    ON DELETE CASCADE,
    sid       text   NOT NULL REFERENCES services (id) ON DELETE CASCADE,
    anonymous bool   NOT NULL,
    UNIQUE (uid, sid)
);

CREATE TABLE IF NOT EXISTS personas (
    id           bigserial   PRIMARY KEY,
    name         text        NOT NULL UNIQUE,
    uid          bigint      NOT NULL REFERENCES users (id) ON DELETE CASCADE,
    external_url text                 UNIQUE,  -- e.g. A3 user path
    created      timestamptz NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS contexts (
    id            serial    PRIMARY KEY,
    name          text      NOT NULL UNIQUE,
    owner_service text      NOT NULL REFERENCES services (id),
    description   text      NOT NULL,
    url           text      NOT NULL
);

-- Which persona should be used for which context?
CREATE TABLE IF NOT EXISTS personas_per_context (
    id            serial     PRIMARY KEY,
    persona_id    bigint     NOT NULL REFERENCES personas (id) ON DELETE CASCADE,
    context_id    bigint     NOT NULL REFERENCES contexts (id) ON DELETE CASCADE,
    UNIQUE (persona_id, context_id)
);

CREATE TABLE IF NOT EXISTS thentos_sessions (
    token  text        PRIMARY KEY,
    uid    bigint      REFERENCES users (id)    ON DELETE CASCADE,
    sid    text        REFERENCES services (id) ON DELETE CASCADE,
    start  timestamptz NOT NULL,
    end_   timestamptz NOT NULL,
    period interval    NOT NULL,
    CHECK ((uid IS NULL) <> (sid IS NULL))
);

CREATE TABLE IF NOT EXISTS service_sessions (
    token                 text        PRIMARY KEY,
    service               text        NOT NULL REFERENCES services (id) ON DELETE CASCADE,
    start                 timestamptz NOT NULL,
    end_                  timestamptz NOT NULL,
    period                interval    NOT NULL,
    thentos_session_token text        NOT NULL REFERENCES thentos_sessions (token) ON DELETE CASCADE,
    meta                  text        NOT NULL
);

-- A persona can be a member of any number of groups
CREATE TABLE IF NOT EXISTS persona_groups (
    pid bigint NOT NULL REFERENCES personas (id) ON DELETE CASCADE,
    grp text   NOT NULL,
    UNIQUE (pid, grp)
);

-- Groups can be members of other groups (any member of subgroup is also a member of supergroup)
CREATE TABLE IF NOT EXISTS group_tree (
    supergroup text NOT NULL,
    subgroup   text NOT NULL,
    UNIQUE (supergroup, subgroup)
);
