const express = require('express')
const { postgraphile } = require('postgraphile')
const { setPgSettings } = require('./graphileSettings')
const {
  GRAPHQL_PATH,
  POSTGRES_CONFIG,
  GRAPHQL_PG_SCHEMAS,
  GRAPHQL_PORT,
} = require('./constants/environment')

// Create express app
const app = express()

// Set port to listen on
app.set('port', GRAPHQL_PORT)

// Set Response headers
app.use((req, res, next) => {
  res.header('Access-Control-Allow-Origin', '*')
  res.header(
    'Access-Control-Allow-Headers',
    'Origin, X-Requested-With, Content-Type, Accept, Authorization',
  )
  next()
})

// Set the postgraphile graphql server
app.use(
  // The path to reach the graphql server. Defaults to `/`
  GRAPHQL_PATH || '/',
  postgraphile(POSTGRES_CONFIG, GRAPHQL_PG_SCHEMAS, {
    // pgSettings object that contains any authorization that may need to happen
    pgSettings: setPgSettings,

    // Enable the graphql Interface
    graphiql: true,

    // Rebuild the graphql schema when any tables change their structure.
    watchPg: true,

    // Enable the enhanced GraphiQL interface to be used with headers.
    enhanceGraphiql: true,
  }),
)

const start = () => {
  app.listen(app.get('port'), () => {
    console.log(`Find the server at: http://localhost:${app.get('port')}/`) // eslint-disable-line no-console
  })
}

module.exports = {
  start,
}
