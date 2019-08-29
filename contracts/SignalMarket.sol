pragma solidity ^0.4.26;

import "./FoamToken.sol";
import "./SignalToken.sol";
import "openzeppelin-solidity/contracts/token/ERC721/ERC721Holder.sol";

contract SignalMarket is ERC721Holder {

  struct Sale {
    uint256 signalId;
    uint256 price;
    address owner;
  }

  Sale[] public sales;
  FoamToken public foamToken;
  SignalToken public signalToken;

  mapping(uint256 => uint256) public signalToSale;

  event SignalForSale(uint256 signalId, uint256 price, uint256 saleId);

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

    // given ownership to SignalMarket
    signalToken.safeTransferFrom(msg.sender, address(this), _tokenId);

    // create sale struct
    Sale memory s = Sale({
      signalId: _tokenId,
      price: _price,
      owner: msg.sender
    });

    // make saleId
    uint256 saleId = sales.push(s) - 1;

    signalToSale[_tokenId] = saleId;

    // emit new sale
    emit SignalForSale(_tokenId, _price, saleId);
  }

  /* function buy(uint256 tokenID, uint256 payment) public { */
  /*   // some sort of owner transfer via the SignalToken.transfer? */
  /*   // if token is for sale, the token should already be approved */
  /*   // for working in this contract */
  /* } */

}
