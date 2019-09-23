"use strict";

exports.getEnabledEthereumProvider_ = function (just, nothing, onEnabled, onRejected) {
  if (typeof window.ethereum !== "undefined") {
    window.ethereum.enable().then(onEnabled, onRejected);
    return just(window.ethereum);
  } else {
    return nothing;
  }
};
