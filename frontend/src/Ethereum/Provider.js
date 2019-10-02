"use strict";

exports.getEthereumProvider_ = function (just, nothing) {
  if (typeof window.ethereum !== "undefined") {
    return just(window.ethereum);
  } else {
    return nothing;
  }
};


exports.enable_ = function (provider, onEnabled, onRejected) {
  provider.enable().then(onEnabled, onRejected);
};
