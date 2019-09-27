pragma solidity ^0.5.0;

import "openzeppelin-solidity/contracts/token/ERC20/ERC20.sol";
import "./StakeToken.sol";
import "./CSTRegistry.sol";

contract SignalToken is StakeToken, CSTRegistry {
  mapping (uint256 => bytes32) public tokenGeohash;
  mapping (uint256 => uint256) public tokenRadius;
  mapping (bytes32 => uint256) public cstToID;

  constructor(ERC20 _token) StakeToken(_token) public { }

  function mint(address, uint256) public returns (uint256) {
    revert("use mintSignal(address,uint256,bytes32,uint256) instead");
  }

  function mintSignal(address owner, uint256 stake, bytes32 geohash, uint256 radius) public returns (uint256 tokenID) {
    tokenID = super.mint(owner, stake);
    tokenGeohash[tokenID] = geohash;
    tokenRadius[tokenID] = radius;

    bytes32 cst = computeCST(address(this), tokenID);
    cstToID[cst] = tokenID;

    emit TrackedToken(cst, address(this), tokenID, geohash, radius);

    return tokenID;
  }

  // implement CSTRegistry
  function getGeohash(bytes32 cst) public view returns (bytes32 geohash) {
    return tokenGeohash[cstToID[cst]];
  }

  function getRadius(bytes32 cst) public view returns (uint256 radius) {
    return tokenRadius[cstToID[cst]];
  }

  function getCreatedOn(bytes32 cst) public view returns (uint256 timestamp) {
    return tokenMintedOn[cstToID[cst]];
  }

  function getDeletedOn(bytes32 cst) public view returns (uint256 timestamp) {
    return tokenBurntOn[cstToID[cst]];
  }

  function isTracked(bytes32 cst) public view returns (bool) {
    return cstToID[cst] != 0;
  }

  function name() external pure returns (string memory) {
    return "FOAM Signal";
  }

  function symbol() external pure returns (string memory) {
    return "FSX";
  }
}