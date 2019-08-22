pragma solidity ^0.4.26;

import "./FoamToken.sol";
import "./SignalToken.sol";

contract SignalMarket {
  FoamToken public foamToken;
  SignalToken public signalToken;

  constructor(address _signalToken, address _foamToken) public {
    // these are basically type conversions; they are _not_ constructors
    foamToken = FoamToken(_foamToken);
    signalToken = SignalToken(_signalToken);
  }
}
