describe("SixtyThree", function() {
  var sixtyThree, trumpSuit;

  beforeEach(function() {
    sixtyThree = new SixtyThree();
    trumpSuit = Suit.spades;
    sixtyThree.setTrump(trumpSuit);
  });

  describe("pointsForCard", function() {
    it("should return 0 if the card is not trump", function() {
      var card = new Card(2, Suit.clubs);
      expect(sixtyThree.pointsForCard(card)).toEqual(0);
    });

    it("should return 1 if the card is the trump ace", function() {
      var card = new Card("Ace", trumpSuit);
      expect(sixtyThree.pointsForCard(card)).toEqual(1);
    });

    it("should return 25 if the card is the trump king", function() {
      var card = new Card("King", trumpSuit);
      expect(sixtyThree.pointsForCard(card)).toEqual(25);
    });

    it("should return 1 if the card is the trump jack", function() {
      var card = new Card("Jack", trumpSuit);
      expect(sixtyThree.pointsForCard(card)).toEqual(1);
    });

    it("should return 1 if the card is the trump 10", function() {
      var card = new Card(10, trumpSuit);
      expect(sixtyThree.pointsForCard(card)).toEqual(1);
    });

    it("should return 9 if the card is the trump 9", function() {
      var card = new Card(9, trumpSuit);
      expect(sixtyThree.pointsForCard(card)).toEqual(9);
    });

    it("should return 5 if the card is the trump 5", function() {
      var card = new Card(5, trumpSuit);
      expect(sixtyThree.pointsForCard(card)).toEqual(5);
    });

    it("should return 5 if the card is the opposite 5", function() {
      var card = new Card(5, trumpSuit.oppositeSuit());
      expect(sixtyThree.pointsForCard(card)).toEqual(5);
    });

    it("should return 1 if the card is the trump 2", function() {
      var card = new Card(2, trumpSuit);
      expect(sixtyThree.pointsForCard(card)).toEqual(1);
    });

    it("should return 15 if the card is the joker", function() {
      var card = new Card("Joker");
      expect(sixtyThree.pointsForCard(card)).toEqual(15);
    });
  });


  describe("cardIsOppositeFive", function() {
    it("should return true if trump is Spades and the card is the 5 of Clubs", function() {
      sixtyThree.setTrump(Suit.spades);
      var card = new Card(5, Suit.clubs);
      expect(sixtyThree.cardIsOppositeFive(card)).toEqual(true);
    });

    it("should return true if trump is Clubs and the card is the 5 of Spades", function() {
      sixtyThree.setTrump(Suit.clubs);
      var card = new Card(5, Suit.spades);
      expect(sixtyThree.cardIsOppositeFive(card)).toEqual(true);
    });

    it("should return true if trump is Hearts and the card is the 5 of Diamonds", function() {
      sixtyThree.setTrump(Suit.hearts);
      var card = new Card(5, Suit.diamonds);
      expect(sixtyThree.cardIsOppositeFive(card)).toEqual(true);
    });

    it("should return true if trump is Diamonds and the card is the 5 of Hearts", function() {
      sixtyThree.setTrump(Suit.diamonds);
      var card = new Card(5, Suit.hearts);
      expect(sixtyThree.cardIsOppositeFive(card)).toEqual(true);
    });
  });

  describe("cardIsTrump", function() {
    it("should return true if the card is the same suit as trump", function() {
      var card = new Card(2, Suit.spades);
      expect(sixtyThree.cardIsTrump(card)).toEqual(true);
    });

    it("should return false if the card is not the same suit as trump", function() {
      var card = new Card(2, Suit.clubs);
      expect(sixtyThree.cardIsTrump(card)).toEqual(false);
    });

    it("should return true if the card is the joker", function() {
      var card = new Card("Joker");
      expect(sixtyThree.cardIsTrump(card)).toEqual(true);
    });

    it("should throw a TrumpUndefinedError if trump has not been set", function() {
      sixtyThree.resetTrump();
      var card = new Card("Joker");
      expect(function() {
        sixtyThree.cardIsTrump(card)
      }).toThrow(new TrumpUndefinedError());
    });
  });
});
