-- This is a repeated migration that will run whenever a change is detected in the file.
-- We will define roles, some user defined functions, and use postgraphile's smart comments to customize our graphql api.


-- First we create two `ROLES`, one we name `anonymous_user` that has no permission to access the schema and
-- one called `graphql_api_user` that we will grant access to all all the tables.

-- Create the `anonymous_user`, if one exists, revoke all its privleges and recreate it.
DO
$do$
BEGIN
  IF EXISTS (
    SELECT
    FROM   pg_catalog.pg_roles
    WHERE  rolname = 'anonymous_user') THEN
    REVOKE ALL PRIVILEGES ON ALL TABLES IN SCHEMA public FROM anonymous_user;
    REVOKE ALL PRIVILEGES ON SCHEMA public FROM anonymous_user;
    DROP USER IF EXISTS anonymous_user;
  END IF;
    CREATE USER anonymous_user;
    GRANT anonymous_user to postgres;
END
$do$;

-- Create the `graphql_api_user`, if one exists, revoke all its privleges and recreate it.
DO
$do$
BEGIN
  IF EXISTS (
    SELECT
    FROM   pg_catalog.pg_roles
    WHERE  rolname = 'graphql_api_user') THEN
    REVOKE ALL PRIVILEGES ON ALL TABLES IN SCHEMA public FROM graphql_api_user;
    REVOKE ALL PRIVILEGES ON SCHEMA public FROM graphql_api_user;
    DROP USER IF EXISTS graphql_api_user;
  END IF;
    CREATE USER graphql_api_user;
    GRANT graphql_api_user to postgres;
END
$do$;

-- Grant usage for public schema.
GRANT USAGE ON SCHEMA public to graphql_api_user;

-- Grant select on for tables.
GRANT SELECT ON TABLE signal_market_signal_for_sale          TO graphql_api_user;
GRANT SELECT ON TABLE signal_market_signal_sold              TO graphql_api_user;
GRANT SELECT ON TABLE signal_token_tracked_token             TO graphql_api_user;
GRANT SELECT ON TABLE signal_token_transfer                  TO graphql_api_user;
GRANT SELECT ON TABLE signal_unlisted                        TO graphql_api_user;
GRANT SELECT ON TABLE spatial_ref_sys                        TO graphql_api_user;
GRANT SELECT ON TABLE raw_change                             TO graphql_api_user;
GRANT SELECT ON TABLE foam_token_transfer                    TO graphql_api_user;
GRANT SELECT ON TABLE "checkpoint"                           TO graphql_api_user;


-- Postgraphile Smart Comments for GraphQL API customization

-- Remove table `flyway_schema_history` from the API as it is for migrations.
COMMENT ON TABLE flyway_schema_history          IS E'@omit';

-- Remove create, update, and delete mutations from our API.
COMMENT ON TABLE "checkpoint"                   IS E'@omit create,update,delete';
COMMENT ON TABLE foam_token_transfer            IS E'@omit create,update,delete';
COMMENT ON TABLE raw_change                     IS E'@omit create,update,delete';
COMMENT ON TABLE signal_market_signal_for_sale  IS E'@omit create,update,delete';
COMMENT ON TABLE signal_market_signal_sold      IS E'@omit create,update,delete';
COMMENT ON TABLE signal_token_tracked_token     IS E'@omit create,update,delete';
COMMENT ON TABLE signal_token_transfer          IS E'@omit create,update,delete';
COMMENT ON TABLE signal_unlisted                IS E'@omit create,update,delete';
COMMENT ON TABLE spatial_ref_sys                IS E'@omit create,update,delete';

