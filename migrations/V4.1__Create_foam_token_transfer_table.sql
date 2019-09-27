-- This table records Transfer events emitted by the FoamToken contract. The indexer
-- stores the deserialized arguments to the event, and associates it with its metadata
-- in "raw_change" via the event_id column.
CREATE TABLE "foam_token_transfer"
  ( "to" text NOT NULL,
    "from" text NOT NULL,
    "value" numeric NOT NULL,
    "event_id" text NOT NULL PRIMARY KEY REFERENCES "raw_change"("event_id")
  );
CREATE INDEX ON "foam_token_transfer" ("from");
CREATE INDEX ON "foam_token_transfer" ("to");