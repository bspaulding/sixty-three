var matchers = {};

matchers.toBeInstanceOf = function(expected) {
  var actual = this.actual;
  var notText = this.isNot ? " not" : "";

  this.message = function() {
    return "Expected " + actual + notText + " to be an instance of " + expected.name;
  }

  return this.actual instanceof expected;
}

beforeEach(function() {
  this.addMatchers(matchers);
});
