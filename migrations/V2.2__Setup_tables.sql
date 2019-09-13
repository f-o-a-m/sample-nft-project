CREATE TABLE "raw_change"
  ( "log_index" numeric NOT NULL,
    "transaction_hash" text NOT NULL,
    "block_hash" text NOT NULL,
    "address" text NOT NULL,
    "event_id" text NOT NULL PRIMARY KEY
  )

CREATE TABLE "foam_token_transfer"
  ( "to" text NOT NULL,
    "from" text NOT NULL,
    "value" numeric NOT NULL,
    "event_id" text NOT NULL PRIMARY KEY REFERENCES "raw_change"("event_id")
  )
CREATE INDEX ON "foam_token_transfers" ("from");
CREATE INDEX ON "foam_token_transfers" ("to");