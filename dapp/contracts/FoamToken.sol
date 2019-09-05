pragma solidity ^0.4.24;

import "./ERC20Controllable.sol";
import "openzeppelin-solidity/contracts/token/ERC20/StandardToken.sol";
import "openzeppelin-solidity/contracts/token/ERC20/DetailedERC20.sol";

contract FoamToken is ERC20Controllable, DetailedERC20 {
    event FoamTokenConstructor(address _from, uint _value);

    uint constant public SUPPLY = 1000000000000000000000000000; // this should amount to 1 Billion * denomination

    constructor() DetailedERC20("FOAM Token", "FOAM", 18) public {
        balances[msg.sender] = SUPPLY;
        totalSupply_ = SUPPLY;
        emit FoamTokenConstructor(msg.sender,SUPPLY);
    }
}
