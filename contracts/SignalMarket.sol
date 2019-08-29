pragma solidity ^0.4.26;

import "./FoamToken.sol";
import "./SignalToken.sol";
import "openzeppelin-solidity/contracts/token/ERC721/ERC721Holder.sol";

contract SignalMarket is ERC721Holder {

  struct Sale {
    uint256 tokenId;
    uint256 price;
    address owner;
  }

  FoamToken public foamToken;
  SignalToken public signalToken;

  mapping(uint256 => Sale) public signalToSale;

  event SignalForSale(uint256 signalId, uint256 price);

  constructor(address _signalToken, address _foamToken) public {
    // these are basically type conversions; they are _not_ constructors
    foamToken = FoamToken(_foamToken);
    signalToken = SignalToken(_signalToken);
  }

  function forSale(uint256 _tokenId, uint256 _price) public {
    // @NOTE: the SignalToken contract has to approve _this_ contract
    // to do this?

    // Check for caller ownership
    require(signalToken.ownerOf(_tokenId) == msg.sender);

    // give ownership to SignalMarket
    signalToken.safeTransferFrom(msg.sender, address(this), _tokenId);

    // create sale struct
    Sale memory s = Sale({
      tokenId: _tokenId,
      price: _price,
      owner: msg.sender
    });

    signalToSale[_tokenId] = s;

    // emit new sale
    emit SignalForSale(_tokenId, _price);
  }

  /* function buy(uint256 tokenID, uint256 payment) public { */
  /*   // some sort of owner transfer via the SignalToken.transfer? */
  /*   // if token is for sale, the token should already be approved */
  /*   // for working in this contract */
  /* } */

}
