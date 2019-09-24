pragma solidity ^0.5.0;

import "./FoamToken.sol";
import "./SignalToken.sol";
import "openzeppelin-solidity/contracts/token/ERC721/ERC721Holder.sol";

contract SignalMarket is ERC721Holder {

  struct Sale {
    uint256 saleId;
    uint256 tokenId;
    uint256 price;
    address payable owner;
  }

  // provides a unique key for sales.
  uint256 saleNonce;

  FoamToken public foamToken;
  SignalToken public signalToken;

  mapping(uint256 => Sale) public signalToSale;
  mapping(uint256 => Sale) public saleIdToSale;

  event SignalForSale(uint256 saleId, uint256 indexed signalId, uint256 price, address seller);
  event SignalUnlisted(uint256 saleId);
  event SignalSold(uint256 saleId, uint256 signalId, uint256 price, address owner, address newOwner);

  constructor(address _signalToken, address _foamToken) public {
    // these are basically type conversions; they are _not_ constructors
    foamToken = FoamToken(_foamToken);
    signalToken = SignalToken(_signalToken);
    saleNonce = 1;
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
      saleId: saleNonce,
      tokenId: _tokenId,
      price: _price,
      owner: msg.sender
    });

    // index sale
    signalToSale[_tokenId] = s;
    saleIdToSale[saleNonce] = s;

    // emit new sale
    emit SignalForSale(saleNonce, _tokenId, _price, msg.sender);
    saleNonce = saleNonce + 1;
  }

  // unlists a signal from the marketplace
  function unlist(uint256 _saleId) public {
    Sale storage sale = saleIdToSale[_saleId];
    uint256 tokenId = sale.tokenId;
    // only the owner can do this
    require(signalToSale[tokenId].owner == msg.sender);
    // remove from sale list
    delete signalToSale[tokenId];
    // give signal back (from this contract) to owner address
    signalToken.safeTransferFrom(address(this), msg.sender, tokenId);

    emit SignalUnlisted(_saleId);
  }

  function buy(uint256 _saleId) public payable {
    // some sort of owner transfer via the SignalToken.transfer?
    // if token is for sale, the token should already be approved
    // for working in this contract
    Sale storage s = saleIdToSale[_saleId];

    // check payment
    require(msg.value >= s.price);

    uint256 refund = msg.value - s.price;
    if(refund > 0)
      msg.sender.transfer(refund);

    // get money
    s.owner.transfer(s.price);

    emit SignalSold(s.saleId, s.tokenId, s.price, s.owner, msg.sender);

    // transfer signal token
    signalToken.approve(msg.sender, s.tokenId);
    signalToken.safeTransferFrom(address(this), msg.sender, s.tokenId);

    // unmark sale
    delete signalToSale[s.tokenId];
    delete saleIdToSale[_saleId];
  }
}
