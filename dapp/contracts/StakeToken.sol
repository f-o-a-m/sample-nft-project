pragma solidity ^0.4.24;

import "erc20-tokens/contracts/eip20/EIP20.sol";
import "./ERC721Controllable.sol";
import "./Interfaces/POUInterface.sol";
import "openzeppelin-solidity/contracts/math/SafeMath.sol";

contract StakeToken is ERC721Controllable, POUInterface {
  EIP20Interface intrinsicToken;
  uint256 nftNonce;

  using SafeMath for uint;


  function numApplications(address prover) external view returns(uint256) {
    return balanceOf(prover);
  }

  function totalStaked(address prover) external view returns(uint256) {
    return _totalStaked[prover];
  }

  mapping (address => uint256) _totalStaked;
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
    _totalStaked[_from] = _totalStaked[_from].sub(tokenStake[_tokenId]);
  }

  function addTokenTo(address _to, uint256 _tokenId) internal {
    super.addTokenTo(_to, _tokenId);
    _totalStaked[_to] = _totalStaked[_to].add(tokenStake[_tokenId]);
  }
}
