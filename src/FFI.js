exports._logWithPrefix = function(prefix, s) {
  return function() {
    console.log(prefix, s);
  };
};

exports.now = function() {
  return function() {
    return new Date().getTime();
  };
};

exports._add = function(x, y) {
  return x + y;
};

exports.random = function() {
  return Math.random();
};
