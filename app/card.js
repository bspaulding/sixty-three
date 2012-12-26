var Card = (function() {
  function Card(value, suit) {
    this.value = value;
    this.suit = suit;
  }

  Card.prototype.toString = function() {
    return this.value + ' of ' + this.suit;
  }

  return Card;
}());
