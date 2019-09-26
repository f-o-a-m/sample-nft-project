CREATE TABLE "signals"
  ( "token_id" text NOT NULL PRIMARY KEY
  , "owner" text NOT NULL
  , "creator" text NOT NULL
  , "cst" text
  , "geohash" text
  , "radius" numeric
  , "amount_staked" numeric
  , "tokens_staked" text REFERENCES "signal_token_tokens_staked" ("event_id")
  , "tokens_unstaked" text REFERENCES "signal_token_tokens_unstaked" ("event_id")
  , "tracked_token" text REFERENCES "signal_token_tracked_token" ("event_id")
  -- Transfer happens first, hence will always be not null
  , "last_transfer" text NOT NULL REFERENCES "signal_token_transfer" ("event_id")
  -- ditto, will always have at least one transfer.
  , "minting_transfer" text NOT NULL REFERENCES "signal_token_transfer" ("event_id")
  );

CREATE INDEX ON "signals" ("token_id");
CREATE INDEX ON "signals" ("cst");
CREATE INDEX ON "signals" ("owner");
CREATE INDEX ON "signals" ("creator");