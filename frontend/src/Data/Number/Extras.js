"use strict";

exports.toFixed = function (digits) {
  return function(num) {
    return num.toFixed(digits);
  };
};

// same as one from Data.Int but without `(i | 0) === i`
exports.fromStringAsImpl = function (just) {
  return function (nothing) {
    return function (radix) {
      var digits;
      if (radix < 11) {
        digits = "[0-" + (radix - 1).toString() + "]";
      } else if (radix === 11) {
        digits = "[0-9a]";
      } else {
        digits = "[0-9a-" + String.fromCharCode(86 + radix) + "]";
      }
      var pattern = new RegExp("^[\\+\\-]?" + digits + "+$", "i");

      return function (s) {
        if (pattern.test(s)) {
          return just(parseInt(s, radix));
        } else {
          return nothing;
        }
      };
    };
  };
};
