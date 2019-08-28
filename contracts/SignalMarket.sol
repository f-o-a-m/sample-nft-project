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

  function markTokenForSale(address owner, uint256 tokenID, uint256 price) public returns (uint256 tokenID) {
    // @TODO: need to check if token is actually owned by owner
    return tokenID;
  }

  function buyToken(uint256 tokenID, uint256 payment) public {
    // maybe some sort of owner transfer
  }

}
