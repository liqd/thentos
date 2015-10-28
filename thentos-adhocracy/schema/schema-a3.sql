-- All statements in this file must be idempotent (it is called at start time, every time).
-- This augments the DB schema defined in thentos-core.

-- Mapping from persona ID to the path of the corresponding A3 user
CREATE TABLE IF NOT EXISTS a3_user_paths (
    pid   bigint  PRIMARY KEY REFERENCES personas (id) ON DELETE CASCADE,
    path  text    NOT NULL UNIQUE
);
