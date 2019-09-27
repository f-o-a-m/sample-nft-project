-- The checkpoint table stores information about which events the indexer has started or finished processing.
-- In the event of a failure or a graceful shutdown, checkpoints are used to automatically resume from the 
-- last event processed as opposed to starting the indexing process from scratch.
CREATE TABLE "checkpoint"
  ( "name" text NOT NULL,
    "log_index" numeric NOT NULL,
    "block_number" numeric NOT NULL,
    "status" text NOT NULL,
    "event_id" text NOT NULL PRIMARY KEY
  );
CREATE INDEX ON "checkpoint" ("log_index");
CREATE INDEX ON "checkpoint" ("block_number");