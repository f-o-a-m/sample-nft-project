-- The raw_change table stores metadata about an event emitted by the EVM.
-- Each event has a unique event_id that is the hash of these metadata elements.
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