exports._log = function(s) {
  return function() {
    return console.log(s);
  };
};

exports._logWithPrefix = function(prefix, string) {
  console.log(prefix, string);
};

exports._add = function(x, y) {
  return x + y;
};
