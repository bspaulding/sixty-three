function TrumpUndefinedError(message) {
  this.name = arguments.callee.name
  this.message = message;
};
TrumpUndefinedError.prototype = Error.prototype;

var SixtyThree = (function() {
  function SixtyThree() {
    this.deck = new Deck();
  }

  SixtyThree.prototype.showShuffledDeck = function() {
    this.deck.shuffle();
    for ( var i = 0; i < this.deck.cards.length; i += 1 ) {
      console.log(this.deck.cards[i].toString());
    }
  }

  SixtyThree.prototype.winningCardForCards = function(cards) {
    return this.rankOrderCards(cards)[0];
  }

  SixtyThree.prototype.rankOrderCards = function(cards) {
    return this.arraySort(cards, this.rankForCard);
  }

  SixtyThree.prototype.rankForCard = function(card) {
    return card.value;
  }

  SixtyThree.prototype.pointsForCard = function(card) {
    if ( this.cardIsOppositeFive(card) ) {
      return 5;
    }

    if ( !this.cardIsTrump(card) ) {
      return 0;
    }

    var points = this.valuePointMap[String(card.value).toLowerCase()];
    if ( 'undefined' === typeof points ) {
      return 0;
    }

    return points;
  }

  SixtyThree.prototype.cardIsOppositeFive = function(card) {
    return card.suit === this.oppositeSuit() && card.value === 5;
  }

  SixtyThree.prototype.oppositeSuit = function() {
    return this.trumpSuit.oppositeSuit();
  }

  SixtyThree.prototype.valuePointMap = {
    "ace":   1,
    "king":  25,
    "jack":  1,
    "10":    1,
    "9":     9,
    "5":     5,
    "2":     1,
    "joker": 15
  }

  SixtyThree.prototype.cardIsTrump = function(card) {
    if ( 'undefined' === typeof this.trumpSuit ) {
      throw new TrumpUndefinedError();
    }

    return this.trumpSuit === card.suit || String(card.value).toLowerCase() === "joker";
  }

  SixtyThree.prototype.setTrump = function(trumpSuit) {
    this.trumpSuit = trumpSuit;
  }

  SixtyThree.prototype.resetTrump = function() {
    delete this.trumpSuit;
  }

  SixtyThree.prototype.arraySort = _.sortBy;

  return SixtyThree;
}());
