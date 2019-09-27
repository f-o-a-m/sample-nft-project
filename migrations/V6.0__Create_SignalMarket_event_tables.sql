-- TODO: @Carl: Split these up if you decide to split out into the SignalMarket model.
CREATE TYPE salestatus AS ENUM('active', 'complete', 'unlisted');

CREATE TABLE "signal_market_signal_for_sale"
  ( "sale_id" numeric NOT NULL UNIQUE,
    "token_id" numeric NOT NULL,
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
  ( "sale_id" numeric NOT NULL,
    "token_id" numeric NOT NULL,
    "price" numeric NOT NULL,
    "sold_from" text NOT NULL,
    "sold_to" text NOT NULL,
    "event_id" text NOT NULL PRIMARY KEY REFERENCES "raw_change"("event_id")
  );
CREATE INDEX ON "signal_market_signal_sold" ("sale_id");
CREATE INDEX ON "signal_market_signal_sold" ("token_id");
CREATE INDEX ON "signal_market_signal_sold" ("sold_from");
CREATE INDEX ON "signal_market_signal_sold" ("sold_to");
