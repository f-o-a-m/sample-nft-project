CREATE TABLE "signal_unlisted"
  ( "sale_id" numeric NOT NULL,
    "token_id" numeric NOT NULL,
    "event_id" text NOT NULL PRIMARY KEY REFERENCES "raw_change"("event_id")
  );
CREATE INDEX ON "signal_unlisted" ("sale_id");
CREATE INDEX ON "signal_unlisted" ("token_id");
