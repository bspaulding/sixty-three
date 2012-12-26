var HumanPlayer = (function() {
  function HumanPlayer() {
    return arguments.callee.parent.apply(arguments);
  }

  HumanPlayer.prototype = Player;
  HumanPlayer.constructor = HumanPlayer;
  HumanPlayer.parent = Player;

  return HumanPlayer;
}());
