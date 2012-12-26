var RandomPlayer = (function() {
  function RandomPlayer() {
    return arguments.callee.parent.apply(arguments);
  }

  RandomPlayer.prototype = Player;
  RandomPlayer.constructor = RandomPlayer;
  RandomPlayer.parent = Player;

  RandomPlayer.bid = function() {
    return parseInt(Math.random() * (63-15)) + 15;
  }

  return RandomPlayer;
}());
