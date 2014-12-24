var Card = (function () {
  "use strict";

  function Card(value, suit) {
    this.value = value;
    this.suit = suit;
  }

  Card.prototype.toString = function () {
    return this.value + ' of ' + this.suit;
  };

  Card.prototype.equals = function (object) {
    return object instanceof Card && object.value === this.value && object.suit === this.suit;
  };

  Card.prototype.valueOf = function () {
    return this.toString();
  };

  return Card;
}());
