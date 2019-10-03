pragma solidity ^0.5.0;

import "openzeppelin-solidity/contracts/token/ERC721/IERC721.sol";

contract CSTRegistry {
  function getGeohash(bytes32 cst) public view returns (bytes32 geohash);
  function getRadius(bytes32 cst) public view returns (uint256 radius);
  function getCreatedOn(bytes32 cst) public view returns (uint256 timestamp);
  function getDeletedOn(bytes32 cst) public view returns (uint256 timestamp);

  function isTracked(bytes32 cst) public view returns (bool);

  event TrackedToken(bytes32 cst, address indexed nftAddress, uint256 tokenID, address owner, uint256 stake, bytes32 geohash, uint256 radius);

/*
  function trackToken(IERC721 nftContract, uint256 tokenID, bytes32 geohash, uint256 radius) public returns (bytes32) {
    require(radius > 0, "radius must be nonzero");

    address tokenOwner = nftContract.ownerOf(tokenID);
    require(    msg.sender == address(this) // so that contracts which inherit from this registry and also mint can track without doing approve/transfer gymnastics
             || tokenOwner == msg.sender // the rest are basically nftContract.isApprovedOrOwner(tokenID, msg.sender), since they made it internal for some bizarre reason
             || nftContract.getApproved(tokenID) == msg.sender
             || nftContract.isApprovedForAll(tokenOwner, msg.sender)
           , "msg.sender must have control over the token or be the registry itself");

    bytes32 cst = computeCST(nftContract, tokenID);
    require(!isTracked(cst), "CST is already tracked");

    TrackedToken storage theToken = trackedTokens[cst];
    theToken.geohash = geohash;
    theToken.radius = radius;

    return cst;
  } */


  function computeCST(address nftContract, uint256 tokenID) public pure returns (bytes32) {
    return keccak256(abi.encodePacked(nftContract, tokenID));
  }
}
