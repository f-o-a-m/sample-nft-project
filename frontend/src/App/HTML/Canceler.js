"use strict";

exports.readCanceler = function(instance) {
  return instance.__canceler__;
};

exports.setCanceler = function(canceler, instance) {
  instance.__canceler__ = canceler;
};
