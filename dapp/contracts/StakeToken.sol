pragma solidity ^0.5.0;

import "openzeppelin-solidity/contracts/math/SafeMath.sol";
import "openzeppelin-solidity/contracts/token/ERC20/IERC20.sol";
import "openzeppelin-solidity/contracts/token/ERC721/ERC721.sol";

contract StakeToken is ERC721 {
  IERC20 intrinsicToken;
  uint256 nftNonce;

  using SafeMath for uint;

  mapping (uint256 => uint256) public tokenStake;
  mapping (uint256 => uint256) public tokenMintedOn;
  mapping (uint256 => uint256) public tokenBurntOn;

  constructor(IERC20 _token) public {
    intrinsicToken = _token;
  }

  function mint(address mintedTokenOwner, uint256 stake) public returns (uint256 tokenID) {
    require(msg.sender == mintedTokenOwner, "msg.sender == mintedTokenOwner");

    nftNonce += 1;
    tokenID = nftNonce;
    tokenStake[tokenID] = stake;
    tokenMintedOn[tokenID] = block.timestamp;
    super._mint(mintedTokenOwner, tokenID);

    require(intrinsicToken.transferFrom(mintedTokenOwner, address(this), stake), "transferFrom");

    return tokenID;
  }

  function burn(uint256 tokenID) public {
    address burntTokenOwner = ownerOf(tokenID);
    require(msg.sender == burntTokenOwner, "msg.sender == burntTokenOwner"); // _burn doesn't do any ownership checks...
    uint256 stake = tokenStake[tokenID];
    super._burn(burntTokenOwner, tokenID);
    tokenBurntOn[tokenID] = block.timestamp;
    require(intrinsicToken.transfer(burntTokenOwner, stake), "transfer");
  }
}
