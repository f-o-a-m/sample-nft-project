pragma solidity ^0.4.24;

import "erc20-tokens/contracts/eip20/EIP20.sol";
import "openzeppelin-solidity/contracts/math/SafeMath.sol";
import "openzeppelin-solidity/contracts/token/ERC721/ERC721BasicToken.sol";

contract StakeToken is ERC721BasicToken {
  EIP20Interface intrinsicToken;
  uint256 nftNonce;

  using SafeMath for uint;

  mapping (uint256 => uint256) public tokenStake;
  mapping (uint256 => uint256) public tokenMintedOn;
  mapping (uint256 => uint256) public tokenBurntOn;

  constructor(EIP20Interface _token) public {
    intrinsicToken = _token;
  }

  function mint(address mintedTokenOwner, uint256 stake) public returns (uint256 tokenID) {
    require(msg.sender == mintedTokenOwner, "msg.sender == mintedTokenOwner");

    nftNonce += 1;
    tokenID = nftNonce;
    tokenStake[tokenID] = stake;
    tokenMintedOn[tokenID] = block.timestamp;
    super._mint(mintedTokenOwner, tokenID);

    require(intrinsicToken.transferFrom(mintedTokenOwner, this, stake), "transferFrom");

    return tokenID;
  }

  function burn(uint256 tokenID) public {
    address burntTokenOwner = tokenOwner[tokenID];
    require(msg.sender == burntTokenOwner, "msg.sender == burntTokenOwner"); // _burn doesn't do any ownership checks...
    uint256 stake = tokenStake[tokenID];
    super._burn(burntTokenOwner, tokenID);
    tokenBurntOn[tokenID] = block.timestamp;
    require(intrinsicToken.transfer(burntTokenOwner, stake), "transfer");
  }

  function removeTokenFrom(address _from, uint256 _tokenId) internal {
    super.removeTokenFrom(_from, _tokenId);
  }

  function addTokenTo(address _to, uint256 _tokenId) internal {
    super.addTokenTo(_to, _tokenId);
  }
}
