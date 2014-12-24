/*global _: false */
var RuleSet = (function () {
  "use strict";

  function TrumpUndefinedError(message) {
    this.name = 'TrumpUndefinedError';
    this.message = message;
  }
  TrumpUndefinedError.prototype = Error.prototype;

  function RuleSet() {
    return this;
  }

  RuleSet.prototype.winningCardForCards = function (cards) {
    this.ensureTrumpIsDefined();

    //if ( this.numTrumpInCards(cards) === 0 ) {
      //return this.highestValueForCards(this.selectBySuit(cards, cards[0].suit));
    //}

    return this.orderByValue(this.selectTrump(cards))[0];
  };

  RuleSet.prototype.orderByValue = function (cards) {
    var self = this;
    return _.sortBy(cards, function (card) {
      return self.rankValueForCard(card);
    });
  };

  RuleSet.prototype.rankValueForCard = function (card) {
    var normalizedCardValue = String(card.value).toLowerCase();
    if (card.value === 5 && card.suit === this.trumpSuit.oppositeSuit()) {
      normalizedCardValue = "opposite 5";
    }

    return this.orderedCardValues.indexOf(normalizedCardValue);
  };

  RuleSet.prototype.orderedCardValues = ["joker", "2", "3", "4", "opposite 5", "5", "6", "7", "8", "9", "10", "jack", "queen", "king", "ace"];

  RuleSet.prototype.numTrumpInCards = function (cards) {
    return this.selectBySuit(cards, this.trumpSuit).length;
  };

  RuleSet.prototype.selectTrump = function (cards) {
    return this.selectBySuit(cards, this.trumpSuit);
  };

  RuleSet.prototype.selectBySuit = function (cards, suit) {
    return _.select(cards, function (card) {
      return card.suit === suit;
    });
  };

  RuleSet.prototype.pointsForCard = function (card) {
    this.ensureTrumpIsDefined();

    if (this.cardIsOppositeFive(card)) {
      return 5;
    }

    if (!this.cardIsTrump(card)) {
      return 0;
    }

    var points = this.valuePointMap[String(card.value).toLowerCase()];
    if (undefined === points) {
      return 0;
    }

    return points;
  };

  RuleSet.prototype.cardIsOppositeFive = function (card) {
    return card.suit === this.oppositeSuit() && card.value === 5;
  };

  RuleSet.prototype.oppositeSuit = function () {
    return this.trumpSuit.oppositeSuit();
  };

  RuleSet.prototype.valuePointMap = {
    "ace":   1,
    "king":  25,
    "jack":  1,
    "10":    1,
    "9":     9,
    "5":     5,
    "2":     1,
    "joker": 15
  };

  RuleSet.prototype.cardIsTrump = function (card) {
    this.ensureTrumpIsDefined();

    return this.trumpSuit === card.suit || String(card.value).toLowerCase() === "joker";
  };

  RuleSet.prototype.setTrump = function (trumpSuit) {
    this.trumpSuit = trumpSuit;
  };

  RuleSet.prototype.resetTrump = function () {
    delete this.trumpSuit;
  };

  RuleSet.prototype.ensureTrumpIsDefined = function () {
    if (undefined === this.trumpSuit) {
      throw new TrumpUndefinedError();
    }
  };

  RuleSet.TrumpUndefinedError = TrumpUndefinedError;

  return RuleSet;
}());
