-- This migration creates two tables, for SignalToken.TokensStaked, and .TokensUnstaked.
-- They're similar enough that they could even be put into the same table, but that would
-- complicate the logic of the indexer as well as break the pattern we have in the other tables.

-- freshly minted signals have stakes associated with them
CREATE TABLE "signal_token_tokens_staked"
  ( "staker" text NOT NULL,
    "token_id" numeric NOT NULL,
    "stake_amount" numeric NOT NULL,
    "timestamp" numeric NOT NULL,
    "event_id" text NOT NULL PRIMARY KEY REFERENCES "raw_change"("event_id")
  );
CREATE INDEX ON "signal_token_tokens_staked" ("token_id");
CREATE INDEX ON "signal_token_tokens_staked" ("staker");

-- burnt signals have unstakes associated with them
CREATE TABLE "signal_token_tokens_unstaked"
  ( "staker" text NOT NULL,
    "token_id" numeric NOT NULL,
    "stake_amount" numeric NOT NULL,
    "timestamp" numeric NOT NULL,
    "event_id" text NOT NULL PRIMARY KEY REFERENCES "raw_change"("event_id")
  );
CREATE INDEX ON "signal_token_tokens_unstaked" ("token_id");
CREATE INDEX ON "signal_token_tokens_unstaked" ("staker");