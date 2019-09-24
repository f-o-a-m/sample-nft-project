CREATE TABLE "raw_change"
  ( "log_index" numeric NOT NULL,
    "transaction_hash" text NOT NULL,
    "block_hash" text NOT NULL,
    "block_number" numeric NOT NULL,
    "address" text NOT NULL,
    "event_id" text NOT NULL PRIMARY KEY,
    /* asserting that event_id is unique*/
    UNIQUE ("block_hash", "log_index")
  );

/*
FoamToken
*/

CREATE TABLE "foam_token_transfer"
  ( "to" text NOT NULL,
    "from" text NOT NULL,
    "value" numeric NOT NULL,
    "event_id" text NOT NULL PRIMARY KEY REFERENCES "raw_change"("event_id")
  );
CREATE INDEX ON "foam_token_transfer" ("from");
CREATE INDEX ON "foam_token_transfer" ("to");

/*
SignalToken
*/

CREATE TABLE "signal_token_transfer"
  ( "to" text NOT NULL,
    "from" text NOT NULL,
    "token_id" numeric NOT NULL REFERENCES "signal_token_tracked_token"("token_id"),
    "event_id" text NOT NULL PRIMARY KEY REFERENCES "raw_change"("event_id")
  );
CREATE INDEX ON "signal_token_transfer" ("from");
CREATE INDEX ON "signal_token_transfer" ("to");

/* freshly minted signals */
CREATE TABLE "signal_token_tracked_token"
  ( "nft_address" text NOT NULL,
    "cst" text NOT NULL, /* no idea why this is text; foam-tcr uses `text` here */
    "geohash" text NOT NULL,
    "radius" numeric NOT NULL,
    "token_id" numeric NOT NULL UNIQUE,
    "event_id" text NOT NULL PRIMARY KEY REFERENCES "raw_change"("event_id")
  );
CREATE INDEX ON "signal_token_tracked_token" ("nft_address");
CREATE INDEX ON "signal_token_tracked_token" ("token_id");

/*
SignalMarket

Since singals are nfts, these should all be able to use `token_id` as their primary keys,
but maybe not, since signals can be sold, bought, and sold again.

*/

CREATE TYPE salestatus AS ENUM('active', 'complete', 'unlisted');

CREATE TABLE "signal_market_signal_for_sale"
  ( "sale_id" numeric NOT NULL UNIQUE,
    "token_id" numeric NOT NULL REFERENCES "signal_token_tracked_token"("token_id"),
    "price" numeric NOT NULL,
    "sale_status" salestatus NOT NULL,
    "seller" text NOT NULL,
    "event_id" text NOT NULL PRIMARY KEY REFERENCES "raw_change"("event_id")
  );
CREATE INDEX ON "signal_market_signal_for_sale" ("sale_id");
CREATE INDEX ON "signal_market_signal_for_sale" ("token_id");
CREATE INDEX ON "signal_market_signal_for_sale" ("sale_status");
CREATE INDEX ON "signal_market_signal_for_sale" ("seller");

CREATE TABLE "signal_market_signal_sold"
  ( "sale_id" numeric NOT NULL REFERENCES "signal_market_signal_for_sale"("sale_id"),
    "token_id" numeric NOT NULL REFERENCES "signal_token_tracked_token"("token_id"),
    "price" numeric NOT NULL,
    "sold_from" text NOT NULL REFERENCES "signal_market_signal_for_sale"("seller"),
    "sold_to" text NOT NULL,
    "event_id" text NOT NULL PRIMARY KEY REFERENCES "raw_change"("event_id")
  );
CREATE INDEX ON "signal_market_signal_sold" ("sale_id");
CREATE INDEX ON "signal_market_signal_sold" ("token_id");
CREATE INDEX ON "signal_market_signal_sold" ("sold_from");
CREATE INDEX ON "signal_market_signal_sold" ("sold_to");

CREATE TABLE "checkpoint"
  ( "name" text NOT NULL,
    "log_index" numeric NOT NULL,
    "block_number" numeric NOT NULL,
    "status" text NOT NULL,
    "event_id" text NOT NULL PRIMARY KEY

  );
CREATE INDEX ON "checkpoint" ("log_index");
CREATE INDEX ON "checkpoint" ("block_number");
