"use strict";

exports.apiBaseURL = (function() {
    var baseURL = process.env.API_BASE_URL;
    var protocol = "";
    if (typeof baseURL !== "string") {
        throw new Error("Missing `API_BASE_URL` ENV variable to get baseURL of API! Please check README for details.");
    }
    try {
        if (typeof window.location.protocol === "string") {
            // @NOTE: this value is always suffixed with a `:`
            protocol = window.location.protocol;
        }
    } catch (_){
        protocol = "http:";
    }
    return { baseURL: baseURL, protocol: protocol };
}());
