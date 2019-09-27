pragma solidity ^0.5.0;

import "openzeppelin-solidity/contracts/math/SafeMath.sol";
import "openzeppelin-solidity/contracts/token/ERC20/IERC20.sol";
import "openzeppelin-solidity/contracts/token/ERC721/ERC721.sol";

contract StakeToken is ERC721 {
  IERC20 intrinsicToken;
  uint256 nftNonce;

  using SafeMath for uint;

  // Without this event, to know the stake in a StakeToken,
  // `stake` will have to be looked up through a join on
  // `SignalToken.TrackedToken.tokenID` and
  // `ERC721BasicToken.Transfer(address(0), _, tokenID)` and
  // `ERC20.Transfer(_, address(nftToken), value)`
  event TokensStaked(address indexed staker, uint256 tokenID, uint256 stakeTimestamp, uint256 tokensStaked);

  // This exists for similar reasons as TokensStaked
  event TokensUnstaked(address indexed staker, uint256 tokenID, uint256 unstakeTimestamp, uint256 tokensUnstaked);

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
    uint256 stakeTimestamp = block.timestamp;

    tokenStake[tokenID] = stake;
    tokenMintedOn[tokenID] = stakeTimestamp;
    super._mint(mintedTokenOwner, tokenID);

    require(intrinsicToken.transferFrom(mintedTokenOwner, address(this), stake), "transferFrom");

    // Without this event, to know the stake in a StakeToken,
    // `stake` will have to be looked up through a join on
    // `SignalToken.TrackedToken.tokenID` and
    // `ERC721BasicToken.Transfer(address(0), _, tokenID)` and
    // `ERC20.Transfer(_, address(nftToken), value)`
    emit TokensStaked(mintedTokenOwner, tokenID, stakeTimestamp, stake);

    return tokenID;
  }

  function burn(uint256 tokenID) public {
    address burntTokenOwner = ownerOf(tokenID);
    require(msg.sender == burntTokenOwner, "msg.sender == burntTokenOwner"); // _burn doesn't do any ownership checks...
    uint256 stake = tokenStake[tokenID];
    super._burn(burntTokenOwner, tokenID);

    uint256 unstakeTimestamp = block.timestamp;
    tokenBurntOn[tokenID] = unstakeTimestamp;
    require(intrinsicToken.transfer(burntTokenOwner, stake), "transfer");

    emit TokensUnstaked(burntTokenOwner, tokenID, unstakeTimestamp, stake);
  }
}
