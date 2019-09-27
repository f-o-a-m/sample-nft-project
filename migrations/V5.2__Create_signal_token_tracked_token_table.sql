-- Just a table for the SignalToken.TrackedToken event
-- Freshly minted signals have a TrackedToken event associated with them.
CREATE TABLE "signal_token_tracked_token"
  ( "nft_address" text NOT NULL,
    "cst" text NOT NULL,
    "geohash" text NOT NULL,
    "radius" numeric NOT NULL,
    "token_id" numeric NOT NULL,
    "event_id" text NOT NULL PRIMARY KEY REFERENCES "raw_change"("event_id")
  );
CREATE INDEX ON "signal_token_tracked_token" ("nft_address");
CREATE INDEX ON "signal_token_tracked_token" ("token_id");