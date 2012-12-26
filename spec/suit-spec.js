describe("Suit", function() {
  describe("oppositeSuit", function() {
    it("should be spades for clubs", function() {
      expect(Suit.clubs.oppositeSuit()).toEqual(Suit.spades);
    });

    it("should be clubs for spades", function() {
      expect(Suit.spades.oppositeSuit()).toEqual(Suit.clubs);
    });

    it("should be hearts for diamonds", function() {
      expect(Suit.diamonds.oppositeSuit()).toEqual(Suit.hearts);
    });

    it("should be diamonds for hearts", function() {
      expect(Suit.hearts.oppositeSuit()).toEqual(Suit.diamonds);
    });
  });

  describe("toString", function() {
    it("should return the suit name for clubs", function() {
      expect(Suit.clubs.toString()).toEqual("Clubs");
    });

    it("should return the suit name for spades", function() {
      expect(Suit.spades.toString()).toEqual("Spades");
    });

    it("should return the suit name for diamonds", function() {
      expect(Suit.diamonds.toString()).toEqual("Diamonds");
    });

    it("should return the suit name for hearts", function() {
      expect(Suit.hearts.toString()).toEqual("Hearts");
    });
  });
});
