exports.add = function(a) {
  return function(b) {
    return a + b;
  }
}

exports._add1 = function(a, b) {
  return a + b;
}

exports.randInt = function(a) {
  return function(b) {
    return function() {
      return Math.floor(Math.random() * (b - a) + a);
    }
  }
}

exports._randInt1 = function(a, b) {
  return function() {
    return Math.floor(Math.random() * (b - a) + a);
  }
}
