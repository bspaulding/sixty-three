/*global __extends: false, Suit: false, Player: false */
var RandomPlayer = (function (_super) {
  "use strict";

  __extends(RandomPlayer, _super);

  function RandomPlayer() {
    return RandomPlayer.__super__.constructor.apply(this, arguments);
  }

  RandomPlayer.prototype.bid = function () {
    return parseInt(Math.random() * (63 - 15), 10) + 15;
  };

  RandomPlayer.prototype.declareTrump = function () {
    return Suit.suits[parseInt(Math.random() * 4, 10)];
  };

  RandomPlayer.prototype.nextCardToPlay = function () {
    return this.cards[0];
  };

  RandomPlayer.prototype.cardsToDiscard = function () {
    return this.cards.slice(6);
  };

  return RandomPlayer;
}(Player));
