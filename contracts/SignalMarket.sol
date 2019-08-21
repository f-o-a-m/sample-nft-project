
import "./SimpleStorage.sol";

contract SignalMarket {
  // @TODO replace with real token types
  SimpleStorage public foamToken;
  SimpleStorage public signalToken;

  constructor(address _signalToken, address _foamToken) public {
    foamToken = SimpleStorage(_foamToken);
    signalToken = SimpleStorage(_signalToken);
    
  }

}
