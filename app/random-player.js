var RandomPlayer = (function(_super) {
  __hasProp = {}.hasOwnProperty,
  __extends = function(child, parent) { for (var key in parent) { if (__hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; };

  __extends(RandomPlayer, _super);

  function RandomPlayer() {
    return RandomPlayer.__super__.constructor.apply(this, arguments);
  }

  RandomPlayer.prototype.bid = function() {
    return parseInt(Math.random() * (63-15), 10) + 15;
  }

  RandomPlayer.prototype.declareTrump = function() {
    return Suit.suits[parseInt(Math.random() * 4, 10)];
  }

  return RandomPlayer;
}(Player));
