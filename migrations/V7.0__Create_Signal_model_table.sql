-- The Signal model is a mostly-hydrated, cache of the state of the signal at the latest
-- indexed event. It is used to simplify queries against signals while also creating a 
-- sort of audit trail with how the state was generated.
--
-- This table can also be essentially synthesized by Opaleye, but for the sake of keeping
-- the databse querying code easy-to-follow, we opted to just have the indexer fill these
-- in as the chain is indexed.
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