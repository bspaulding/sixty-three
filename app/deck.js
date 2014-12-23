var Deck = (function() {
  function Deck(options) {
    if ( 'undefined' === typeof options ) {
      options = {};
    }

    this.options = options;
    this.cards = this.createCards();
  }

  Deck.prototype.createCards = function() {
    var cards = [];

    var suits = this.suits();
    var values = _.reject(this.values(), function(value) { return value === "Joker"; });
    for ( var i in suits ) {
      for ( var j in values ) {
        cards.push(new Card(values[j], suits[i]));
      }
    }
    cards.push(new Card("Joker"));

    return cards;
  }

  Deck.prototype.shuffle = function() {
    var pickedIndices = [];
    for ( var i = 0; i < this.cards.length; i += 1 ) {
      var value = this.cards[i];
      var newIndex = parseInt(Math.random() * this.cards.length);
      while ( pickedIndices.indexOf(newIndex) >= 0 ) {
        newIndex = parseInt(Math.random() * this.cards.length);
      }
      pickedIndices.push(newIndex);
      this.cards[i] = this.cards[newIndex];
      this.cards[newIndex] = value;
    }
  }

  Deck.prototype.pop = function() {
    return this.cards.pop();
  }

  Deck.prototype.draw = Deck.prototype.pop;

  Deck.prototype.length = function() {
    return this.cards.length;
  }

  Deck.prototype.suits = function() {
    return Suit.suits;
  }

  Deck.prototype.values = function() {
    return ['Joker', 2, 3, 4, 5, 6, 7, 8, 9, 10, 'Jack', 'Queen', 'King', 'Ace'];
  }

  return Deck;
}());
