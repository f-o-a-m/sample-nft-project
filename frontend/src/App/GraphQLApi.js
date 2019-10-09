"use strict";

exports.graphQLApiUrl = (function() {
  var graphQLApiUrl = process.env.GRAPHQL_API_URL
  if (typeof graphQLApiUrl !== "string") {
    throw new Error("Missing `GRAPHQL_API_URL` ENV variable to for the graphql api url! Please check README for details.");
  }
  return graphQLApiUrl;
}());

exports.graphQLApiKey = (function() {
  var graphQLApiKey = process.env.GRAPHQL_SIMPLE_AUTH_TOKEN
  if (!graphQLApiKey) {
    console.log("charlie f-ed up")
    return null;
  }
  if (typeof graphQLApiKey !== "string") {
    throw new Error("Incorrect type `GRAPHQL_API_KEY` ENV variable to for the graphql api key, should either be a string or not exist! Please check README for details.");
  }
  return graphQLApiKey;
}());
