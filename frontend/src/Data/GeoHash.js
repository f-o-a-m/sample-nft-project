"use strict";

var Geohash = require("latlon-geohash");

exports.toGeohash = function (precision, latLong) {
  return Geohash.encode(latLong.lat, latLong.lon, precision);
};

exports.fromGeohash = Geohash.decode;
