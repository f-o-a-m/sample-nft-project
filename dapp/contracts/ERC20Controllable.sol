pragma solidity ^0.4.24;

import "./Interfaces/TokenControllerI.sol";
import "openzeppelin-solidity/contracts/ownership/Ownable.sol";
import "openzeppelin-solidity/contracts/token/ERC20/StandardToken.sol";

/**
 * @title Controllable ERC20 token
 *
 * @dev Token that queries a token controller contract to check if a transfer is allowed.
 * @dev controller state var is going to be set with the address of a TokenControllerI contract that has
 * implemented transferAllowed() function.
 *
 !!!!!!!!!!!!!!!!!!!!!
 !!! IMPORTANT
 !!!!!!!!!!!!!!!!!!!!!
 !!!
 !!! The Token Foundry ControllableToken (here named ERC20 controllable) does
 !!! not have the controller != address(0) check on lines 29 and 31, and additionally
 !!! imposes a require(_controller != address(0)) before line 37. We have made these
 !!! modifications to allow us to test ERC20Controllable functionality selectively in our suite
 !!! and the mainnnet FoamToken contract does not have these modifications.
 */
contract ERC20Controllable is Ownable, StandardToken {
    TokenControllerI public controller;

    /// @dev Executes transferAllowed() function from the Controller.
    modifier isAllowed(address _from, address _to) {
        if(controller != address(0)) {
          require(controller.transferAllowed(_from, _to));
        }
        _;
    }

    /// @dev Sets the controller that is going to be used by isAllowed modifier
    function setController(TokenControllerI _controller) onlyOwner public {
        controller = _controller;
    }

    /// @dev It calls parent BasicToken.transfer() function. It will transfer an amount of tokens to an specific address
    /// @return True if the token is transfered with success
    function transfer(address _to, uint256 _value)
        isAllowed(msg.sender, _to)
        public
        returns (bool)
    {
        return super.transfer(_to, _value);
    }

    /// @dev It calls parent StandardToken.transferFrom() function. It will transfer from an address a certain amount of tokens to another address
    /// @return True if the token is transfered with success
    function transferFrom(address _from, address _to, uint256 _value)
        isAllowed(_from, _to)
        public
        returns (bool)
    {
        return super.transferFrom(_from, _to, _value);
    }
}
