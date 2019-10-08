-- This is a repeated migration that will run whenever a change is detected in the file.
-- We will define roles, some user defined functions, and use postgraphile's smart comments to customize our graphql api.
-- For more details on working with Postgraphile see: https://www.graphile.org/postgraphile/introduction.


-------------------------------------------------------------------------
-- | Roles and Grants
-------------------------------------------------------------------------
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


-------------------------------------------------------------------------
-- | Postgraphile Smart Comments for GraphQL API customization
-------------------------------------------------------------------------
-- Remove table `flyway_schema_history` from the API as it is for migrations.
COMMENT ON TABLE flyway_schema_history          IS E'@omit';

-- Remove create, update, and delete mutations from our API, since we want it READ-ONLY.
COMMENT ON TABLE "checkpoint"                   IS E'@omit create,update,delete';
COMMENT ON TABLE foam_token_transfer            IS E'@omit create,update,delete';
COMMENT ON TABLE raw_change                     IS E'@omit create,update,delete';
COMMENT ON TABLE signal_market_signal_for_sale  IS E'@omit create,update,delete';
COMMENT ON TABLE signal_market_signal_sold      IS E'@omit create,update,delete';
COMMENT ON TABLE signal_token_tracked_token     IS E'@omit create,update,delete';
COMMENT ON TABLE signal_token_transfer          IS E'@omit create,update,delete';
COMMENT ON TABLE signal_unlisted                IS E'@omit create,update,delete';
COMMENT ON TABLE spatial_ref_sys                IS E'@omit create,update,delete';


-------------------------------------------------------------------------
-- |  Function and Type Definition
-------------------------------------------------------------------------
--  Type for Stats on owners of signal tokens.
DROP TYPE IF EXISTS signal_owner_stats CASCADE;
CREATE TYPE signal_owner_stats AS
    ( eth_address               TEXT
    , number_owned              NUMERIC
    , number_of_purchases       NUMERIC
    , average_purchase_price    NUMERIC
    , total_purchases           NUMERIC
    , number_of_sales           NUMERIC
    , average_sale_price        NUMERIC
    , total_sales               NUMERIC
    );

--  Ordering type for owners of signal tokens.
DROP TYPE IF EXISTS signal_owner_stats_order_by CASCADE;
CREATE TYPE signal_owner_stats_order_by AS ENUM
  ( 'number_owned_asc'
  , 'number_owned_desc'
  , 'number_of_purchases_asc'
  , 'number_of_purchases_desc'
  , 'average_purchase_price_asc'
  , 'average_purchase_price_desc'
  , 'total_purchases_asc'
  , 'total_purchases_desc'
  , 'number_of_sales_asc'
  , 'number_of_sales_desc'
  , 'average_sale_price_asc'
  , 'average_sale_price_desc'
  , 'total_sales_asc'
  , 'total_sales_desc'
  );

--|  User defined SQL function that builds stats for owners.
--   The arguments allow for :
--     - filtering on specific owners, otherwise includes all
--     - restricting the stats for events between specific blocks
--     - ordering by the stats
DROP FUNCTION IF EXISTS signal_owner_stats CASCADE;
CREATE OR REPLACE FUNCTION signal_owner_stats
  ( owner_addresses   TEXT[] DEFAULT '{}'
  , start_block       NUMERIC DEFAULT NULL
  , end_block         NUMERIC DEFAULT NULL
  , order_by          signal_owner_stats_order_by DEFAULT 'number_owned_desc'
  )
  RETURNS SETOF signal_owner_stats AS $$
    SELECT stats.*
      FROM (
        SELECT owned_tokens.eth_address                                        AS eth_address
             , COALESCE(owned_tokens.number_owned, 0)               :: NUMERIC AS number_owned
             , COALESCE(purchases.number_of_purchases, 0)     :: NUMERIC AS number_of_purchases
             , COALESCE(purchases.average_purchase_price, 0)  :: NUMERIC AS average_purchase_price
             , COALESCE(purchases.total_purchases, 0)         :: NUMERIC AS total_purchases
             , COALESCE(sales.number_of_sales, 0)             :: NUMERIC AS number_of_sales
             , COALESCE(sales.average_sale_price, 0)          :: NUMERIC AS average_sale_price
             , COALESCE(sales.total_sales, 0)                 :: NUMERIC AS total_sales
          FROM (
            SELECT stt.to                       AS eth_address
                 , COUNT(DISTINCT(stt.token_id)) AS number_owned
              FROM signal_token_transfer AS stt
              INNER JOIN raw_change AS rc
                ON stt.event_id = rc.event_id
                AND (
                  (rc.block_number >= start_block AND rc.block_number < end_block)
                  OR (start_block IS NULL AND end_block IS NULL)
                )
              WHERE stt.to = ANY(owner_addresses)
                OR  owner_addresses = '{}'
              GROUP BY stt.to
          ) AS owned_tokens
          LEFT JOIN (
            SELECT sales.sold_from AS eth_address
                 , COUNT(sales.event_id) AS number_of_sales
                 , SUM(sales.price) AS total_sales
                 , AVG(sales.price) AS average_sale_price
              FROM signal_market_signal_sold AS sales
              INNER JOIN raw_change AS rc
                ON sales.event_id = rc.event_id
                AND (
                  (rc.block_number >= start_block AND rc.block_number < end_block)
                  OR (start_block IS NULL AND end_block IS NULL)
                )
              WHERE sales.sold_from = ANY(owner_addresses)
                OR  owner_addresses = '{}'
              GROUP BY sales.sold_from
          ) AS sales
            ON sales.eth_address = owned_tokens.eth_address
          LEFT JOIN (
            SELECT purchases.sold_to AS eth_address
                 , COUNT(purchases.event_id) AS number_of_purchases
                 , SUM(purchases.price) AS total_purchases
                 , AVG(purchases.price) AS average_purchase_price
              FROM signal_market_signal_sold AS purchases
              INNER JOIN raw_change AS rc
                ON purchases.event_id = rc.event_id
                AND (
                  (rc.block_number >= start_block AND rc.block_number < end_block)
                  OR (start_block IS NULL AND end_block IS NULL)
                )
              WHERE purchases.sold_to = ANY(owner_addresses)
                OR  owner_addresses = '{}'
              GROUP BY purchases.sold_to
          ) AS purchases
            ON purchases.eth_address = owned_tokens.eth_address
      ) AS stats
  ORDER BY CASE WHEN order_by = 'number_owned_asc' THEN stats.number_owned END ASC
         , CASE WHEN order_by = 'number_owned_desc' THEN stats.number_owned END DESC
         , CASE WHEN order_by = 'number_of_purchases_asc' THEN stats.number_of_purchases END ASC
         , CASE WHEN order_by = 'number_of_purchases_desc' THEN stats.number_of_purchases END DESC
         , CASE WHEN order_by = 'average_purchase_price_asc' THEN stats.average_purchase_price END ASC
         , CASE WHEN order_by = 'average_purchase_price_desc' THEN stats.average_purchase_price END DESC
         , CASE WHEN order_by = 'total_purchases_asc' THEN stats.total_purchases END ASC
         , CASE WHEN order_by = 'total_purchases_desc' THEN stats.total_purchases END DESC
         , CASE WHEN order_by = 'number_of_sales_asc' THEN stats.number_of_sales END ASC
         , CASE WHEN order_by = 'number_of_sales_desc' THEN stats.number_of_sales END DESC
         , CASE WHEN order_by = 'average_sale_price_asc' THEN stats.average_sale_price END ASC
         , CASE WHEN order_by = 'average_sale_price_desc' THEN stats.average_sale_price END DESC
         , CASE WHEN order_by = 'total_sales_asc' THEN stats.total_sales END ASC
         , CASE WHEN order_by = 'total_sales_desc' THEN stats.total_sales END DESC
$$ LANGUAGE SQL STABLE;
