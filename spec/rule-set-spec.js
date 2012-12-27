describe("RuleSet", function() {
  var ruleSet, trumpSuit;

  beforeEach(function() {
    ruleSet = new RuleSet();
    trumpSuit = Suit.spades;
    ruleSet.setTrump(trumpSuit);
  });

  describe("rankValueForCard", function() {
    function rankTestFactory(highValue, highSuit, lowValue, lowSuit, trumpSuit) {
      var testFunction = function() {
        ruleSet.setTrump(arguments.callee.trumpSuit);

        var highCard = new Card(arguments.callee.highCardValue, arguments.callee.highCardSuit);
        var lowCard = new Card(arguments.callee.lowCardValue, arguments.callee.lowCardSuit);

        var highCardRank = ruleSet.rankValueForCard(highCard);
        var lowCardRank = ruleSet.rankValueForCard(lowCard);

        expect(highCardRank > lowCardRank).toEqual(true);
      }
      testFunction.highCardValue = highValue;
      testFunction.highCardSuit = highSuit;
      testFunction.lowCardValue = lowValue;
      testFunction.lowCardSuit = lowSuit;
      testFunction.trumpSuit = trumpSuit;
      testFunction.testName = "should rank " + testFunction.highCardValue + " of " + testFunction.highCardSuit + " over " + testFunction.lowCardValue + " of " + testFunction.lowCardSuit + " when trump is " + testFunction.trumpSuit;

      return testFunction;
    }

    for ( var j = 0; j < Suit.suits.length; j += 1 ) {
      var trumpSuit = Suit.suits[j];
      var cardValues = ["Ace", "King", "Queen", "Jack", 10, 9, 8, 7, 6, 5, 4, 3, 2, "Joker"];
      for ( var i = 0; i < cardValues.length - 1; i += 1 ) {
        var testFunction = rankTestFactory(cardValues[i], trumpSuit, cardValues[i+1], trumpSuit, trumpSuit);
        it(testFunction.testName, testFunction);
      }

      var testFunction = rankTestFactory(5, trumpSuit, 5, trumpSuit.oppositeSuit(), trumpSuit);
      it(testFunction.testName, testFunction);
      var testFunction = rankTestFactory(5, trumpSuit.oppositeSuit(), 4, trumpSuit, trumpSuit);
      it(testFunction.testName, testFunction);
    }
  });

  describe("selectBySuit", function() {
    it('should return only cards of the suit', function() {
      var cards = [
        new Card("Ace", Suit.hearts),
        new Card("Ace", Suit.diamonds),
        new Card("Ace", Suit.clubs),
        new Card("Ace", Suit.spades)
      ];

      var selectedCards = ruleSet.selectBySuit(cards, Suit.hearts);
      expect(selectedCards[0] instanceof Card).toEqual(true);
      expect(selectedCards.length).toEqual(1);
    });
  });

  describe("winningCardForCards", function() {
    it("should return the trump card if there is only one", function() {
      var winningCard = new Card(2, trumpSuit);

      var cards = [
        new Card("Ace", Suit.hearts),
        new Card("Ace", Suit.diamonds),
        winningCard,
        new Card("Ace", Suit.clubs)
      ];

      expect(ruleSet.winningCardForCards(cards)).toEqual(winningCard);
    });

    xit("should return the joker if its the only trump");
    xit("should return the highest acting trump card if no trump card");
    xit("should return the highest trump card");
  });

  describe("pointsForCard", function() {
    it("should return 0 if the card is not trump", function() {
      var card = new Card(2, Suit.clubs);
      expect(ruleSet.pointsForCard(card)).toEqual(0);
    });

    it("should return 1 if the card is the trump ace", function() {
      var card = new Card("Ace", trumpSuit);
      expect(ruleSet.pointsForCard(card)).toEqual(1);
    });

    it("should return 25 if the card is the trump king", function() {
      var card = new Card("King", trumpSuit);
      expect(ruleSet.pointsForCard(card)).toEqual(25);
    });

    it("should return 1 if the card is the trump jack", function() {
      var card = new Card("Jack", trumpSuit);
      expect(ruleSet.pointsForCard(card)).toEqual(1);
    });

    it("should return 1 if the card is the trump 10", function() {
      var card = new Card(10, trumpSuit);
      expect(ruleSet.pointsForCard(card)).toEqual(1);
    });

    it("should return 9 if the card is the trump 9", function() {
      var card = new Card(9, trumpSuit);
      expect(ruleSet.pointsForCard(card)).toEqual(9);
    });

    it("should return 5 if the card is the trump 5", function() {
      var card = new Card(5, trumpSuit);
      expect(ruleSet.pointsForCard(card)).toEqual(5);
    });

    it("should return 5 if the card is the opposite 5", function() {
      var card = new Card(5, trumpSuit.oppositeSuit());
      expect(ruleSet.pointsForCard(card)).toEqual(5);
    });

    it("should return 1 if the card is the trump 2", function() {
      var card = new Card(2, trumpSuit);
      expect(ruleSet.pointsForCard(card)).toEqual(1);
    });

    it("should return 15 if the card is the joker", function() {
      var card = new Card("Joker");
      expect(ruleSet.pointsForCard(card)).toEqual(15);
    });
  });

  describe("cardIsOppositeFive", function() {
    it("should return true if trump is Spades and the card is the 5 of Clubs", function() {
      ruleSet.setTrump(Suit.spades);
      var card = new Card(5, Suit.clubs);
      expect(ruleSet.cardIsOppositeFive(card)).toEqual(true);
    });

    it("should return true if trump is Clubs and the card is the 5 of Spades", function() {
      ruleSet.setTrump(Suit.clubs);
      var card = new Card(5, Suit.spades);
      expect(ruleSet.cardIsOppositeFive(card)).toEqual(true);
    });

    it("should return true if trump is Hearts and the card is the 5 of Diamonds", function() {
      ruleSet.setTrump(Suit.hearts);
      var card = new Card(5, Suit.diamonds);
      expect(ruleSet.cardIsOppositeFive(card)).toEqual(true);
    });

    it("should return true if trump is Diamonds and the card is the 5 of Hearts", function() {
      ruleSet.setTrump(Suit.diamonds);
      var card = new Card(5, Suit.hearts);
      expect(ruleSet.cardIsOppositeFive(card)).toEqual(true);
    });
  });

  describe("cardIsTrump", function() {
    it("should return true if the card is the same suit as trump", function() {
      var card = new Card(2, Suit.spades);
      expect(ruleSet.cardIsTrump(card)).toEqual(true);
    });

    it("should return false if the card is not the same suit as trump", function() {
      var card = new Card(2, Suit.clubs);
      expect(ruleSet.cardIsTrump(card)).toEqual(false);
    });

    it("should return true if the card is the joker", function() {
      var card = new Card("Joker");
      expect(ruleSet.cardIsTrump(card)).toEqual(true);
    });

    it("should throw a TrumpUndefinedError if trump has not been set", function() {
      ruleSet.resetTrump();
      var card = new Card("Joker");
      expect(function() {
        ruleSet.cardIsTrump(card)
      }).toThrow(new TrumpUndefinedError());
    });
  });
});

