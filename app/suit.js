var Suit = (function () {
  "use strict";

  function Suit(name) {
    this.name = name;
  }

  Suit.spades = new Suit('Spades');
  Suit.clubs = new Suit('Clubs');
  Suit.diamonds = new Suit('Diamonds');
  Suit.hearts = new Suit('Hearts');

  Suit.suits = [
    Suit.spades,
    Suit.clubs,
    Suit.diamonds,
    Suit.hearts
  ];

  Suit.oppositeSuitMap = {
    "spades": Suit.clubs,
    "clubs": Suit.spades,
    "diamonds": Suit.hearts,
    "hearts": Suit.diamonds
  };

  Suit.prototype.oppositeSuit = function () {
    return Suit.oppositeSuitMap[this.name.toLowerCase()];
  };

  Suit.prototype.toString = function () {
    return this.name;
  };

  return Suit;
}());
