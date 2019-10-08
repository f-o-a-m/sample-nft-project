"use strict";

exports.apiBaseURL = (function() {
  var apiBaseURL = process.env.API_BASE_URL
  if (typeof apiBaseURL !== "string") {
    throw new Error("Missing `API_BASE_URL` ENV variable to get baseURL of API! Please check README for details.");
  }
  try {
    if (typeof window.location.protocol === "string") {
      apiBaseURL = window.location.protocol + apiBaseURL
    }
  } catch (_){
    apiBaseURL = "http:" + apiBaseURL
  }
  return apiBaseURL
}());
