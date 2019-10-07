"use strict";

exports.readCanceler = function(key, instance) {
  return instance["__canceler__" + key];
};

exports.setCanceler = function(key, canceler, instance) {
  instance["__canceler__" + key] = canceler;
};
