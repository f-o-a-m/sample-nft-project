pragma solidity ^0.4.26;

import "./FoamToken.sol";
import "./SignalToken.sol";
import "./SimpleStorage.sol";

contract SignalMarket {
  // @TODO replace with real token types
  FoamToken public foamToken;
  SignalToken public signalToken;

  constructor(address _signalToken, address _foamToken) public {
    foamToken = FoamToken(_foamToken);
    signalToken = SignalToken(_signalToken);
  }
}
