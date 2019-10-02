CREATE TABLE "signal_unlisted"
  ( "sale_id" numeric NOT NULL,
    "token_id" numeric NOT NULL,
    "event_id" text NOT NULL PRIMARY KEY REFERENCES "raw_change"("event_id")
  );
CREATE INDEX ON "signal_token_tracked_token" ("sale_id");
CREATE INDEX ON "signal_token_tracked_token" ("token_id");
CREATE INDEX ON "signal_token_tracked_token" ("owner")