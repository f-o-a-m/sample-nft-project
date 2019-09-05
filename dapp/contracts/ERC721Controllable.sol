pragma solidity ^0.4.24;

import "./Interfaces/TokenControllerI.sol";
import "openzeppelin-solidity/contracts/ownership/Ownable.sol";
import "openzeppelin-solidity/contracts/token/ERC721/ERC721BasicToken.sol";
import "openzeppelin-solidity/contracts/token/ERC20/StandardToken.sol";

/**
 * @title Controllable ERC721Basic token
 *
 * @dev Token that queries a token controller contract to check if a transfer is allowed.
 * @dev controller state var is going to be set with the address of a TokenControllerI contract that has
 * implemented transferAllowed() function.
 */

contract ERC721Controllable is Ownable, ERC721BasicToken {
    TokenControllerI public controller;

    /// @dev Executes transferAllowed() function from the Controller.
    modifier isAllowed(address _from, address _to) {
        require(controller.transferAllowed(_from, _to), "controller must allow the transfer");
        _;
    }

    /// @dev Sets the controller that is going to be used by isAllowed modifier
    function setController(TokenControllerI _controller) public onlyOwner {
        require(_controller != address(0), "controller must be a valid address");
        controller = _controller;
    }

     /// @dev It calls parent StandardToken.transferFrom() function. It will transfer from an address a certain amount of tokens to another address
    /// @return True if the token is transfered with success
    function transferFrom(address _from, address _to, uint256 _tokenID)
        public
        isAllowed(_from, _to)
    {
        super.transferFrom(_from, _to, _tokenID);
    }
}
