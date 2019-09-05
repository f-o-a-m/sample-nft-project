pragma solidity ^0.4.24;

contract TokenControllerMock {
  function transferAllowed(address, address)
    external
    pure
    returns (bool) {
      return true;
    }

  function purchaseCheck(address _contributor)
    external
    view returns (bool) {
      return true;
    }
}
