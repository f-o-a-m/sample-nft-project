pragma solidity ^0.5.0;

import "openzeppelin-solidity/contracts/token/ERC20/ERC20Capped.sol";
import "openzeppelin-solidity/contracts/token/ERC20/ERC20Detailed.sol";

contract FoamToken is ERC20Capped, ERC20Detailed {
    event FoamTokenConstructor(address _creator, uint _supply);

    uint256 constant public SUPPLY   = 1000000000 * DECIMALS; // this should amount to 1 Billion * denomination
    uint8   constant public DECIMALS = 18;

    constructor() ERC20Detailed("FOAM Token", "FOAM", DECIMALS) ERC20Capped(SUPPLY) public {
        _mint(msg.sender, SUPPLY);
        emit FoamTokenConstructor(msg.sender, SUPPLY);
    }
}
