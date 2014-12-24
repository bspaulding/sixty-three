/*global _: false, Card: false, Suit: false */
var Deck = (function () {
  "use strict";

  function Deck(options) {
    if (undefined === options) {
      options = {};
    }

    this.options = options;
    this.cards = this.createCards();
  }

  Deck.prototype.createCards = function () {
    var cards = [],
      suits = this.suits(),
      values = _.reject(this.values(), function (value) { return value === "Joker"; }),
      i,
      j;
    for (i in suits) {
      if (suits.hasOwnProperty(i)) {
        for (j in values) {
          if (values.hasOwnProperty(j)) {
            cards.push(new Card(values[j], suits[i]));
          }
        }
      }
    }
    cards.push(new Card("Joker"));

    return cards;
  };

  Deck.prototype.shuffle = function () {
    var pickedIndices = [],
      i,
      value,
      newIndex;
    for (i = 0; i < this.cards.length; i += 1) {
      value = this.cards[i];
      newIndex = parseInt(Math.random() * this.cards.length, 10);
      while (pickedIndices.indexOf(newIndex) >= 0) {
        newIndex = parseInt(Math.random() * this.cards.length, 10);
      }
      pickedIndices.push(newIndex);
      this.cards[i] = this.cards[newIndex];
      this.cards[newIndex] = value;
    }
  };

  Deck.prototype.pop = function () {
    return this.cards.pop();
  };

  Deck.prototype.draw = Deck.prototype.pop;

  Deck.prototype.length = function () {
    return this.cards.length;
  };

  Deck.prototype.suits = function () {
    return Suit.suits;
  };

  Deck.prototype.values = function () {
    return ['Joker', 2, 3, 4, 5, 6, 7, 8, 9, 10, 'Jack', 'Queen', 'King', 'Ace'];
  };

  return Deck;
}());
