"use strict";

var jazzicon = require("jazzicon");

exports.generate_ = function (dimension, seed) {
  // NOTE: jazzicon returns svg inside of a div with width and height set to dimension but the svg
  // itself has width and height set to 100, so need to extract svg and set it's dimensions properly
  var res = jazzicon(dimension, seed);
  var svg = res.children[0];
  if (svg == null) {
    throw "jazzicon must return element containing a child";
  }
  svg.setAttribute("height", dimension);
  svg.setAttribute("width", dimension);
  return svg;
};
