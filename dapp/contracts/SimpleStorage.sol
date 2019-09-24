pragma solidity ^0.5.0;

contract SimpleStorage {
    uint public count;

    event CountSet(uint _count);

    function setCount(uint _count) public {
        count = _count;
        emit CountSet(_count);
    }
}
