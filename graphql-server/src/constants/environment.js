const {
  GRAPHQL_PG_SCHEMAS,
  GRAPHQL_PORT,
  GRAPHQL_PATH,
  GRAPHQL_SIMPLE_AUTH_TOKEN,
  PGUSER,
  PGPASSWORD,
  PGHOST,
  PGPORT,
  PGDATABASE,

} = process.env

const POSTGRES_CONFIG = {
  user: PGUSER,
  password: PGPASSWORD,
  host: PGHOST,
  port: PGPORT,
  database: PGDATABASE,
}


module.exports = {
  GRAPHQL_PATH,
  POSTGRES_CONFIG,
  GRAPHQL_PG_SCHEMAS,
  GRAPHQL_PORT,
  GRAPHQL_SIMPLE_AUTH_TOKEN,
}
