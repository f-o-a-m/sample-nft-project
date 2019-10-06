-- Similar to V4.1, this table records Transfer events emitted by the SignalToken contract.
CREATE TABLE "signal_token_transfer"
  ( "to" text NOT NULL,
    "from" text NOT NULL,
    "token_id" numeric NOT NULL,
    "event_id" text NOT NULL PRIMARY KEY REFERENCES "raw_change"("event_id")
  );
CREATE INDEX ON "signal_token_transfer" ("from");
CREATE INDEX ON "signal_token_transfer" ("to");
