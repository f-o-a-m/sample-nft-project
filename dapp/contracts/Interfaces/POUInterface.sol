pragma solidity ^0.4.24;

interface POUInterface {

    function totalStaked(address) external view returns(uint256);
    function numApplications(address) external view returns(uint256);

}