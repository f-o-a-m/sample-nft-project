-- The raw_change table stores metadata about an event emitted by the EVM.
-- Each event has a unique event_id that is the hash of these metadata elements.
CREATE TABLE "raw_change"
  ( "log_index" numeric NOT NULL,
    "transaction_index" numeric NOT NULL,
    "transaction_hash" text NOT NULL,
    "removed" boolean NOT NULL,
    "block_hash" text NOT NULL,
    "block_number" numeric NOT NULL,
    "address" text NOT NULL,
    "data" text NOT NULL,
    "topics" text[] NOT NULL,
    "event_id" text NOT NULL PRIMARY KEY,
    /* asserting that event_id is unique*/
    UNIQUE ("block_hash", "log_index")
  );

  CREATE FUNCTION notify_trigger() RETURNS trigger AS $$
    DECLARE
    BEGIN
    PERFORM pg_notify('raw_change_channel', NEW."event_id");
    RETURN new;
    END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER raw_change_table_trigger AFTER INSERT ON "raw_change"
  FOR EACH ROW EXECUTE PROCEDURE notify_trigger();
