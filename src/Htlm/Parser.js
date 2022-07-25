// @reference https://github.com/rnons/purescript-html-parser-halogen/blob/e0db4721fa18fa461507dde73a979fa0409970d7/src/Html/Parser.js
exports.decodeHtmlEntity = function(input) {
  if (typeof DOMParser === "undefined") {
    return input;
  }

  // Leading whitespaces are stripped by DOMParser
  var matches = /^\s+/.exec(input);
  var space = "";
  if (matches) {
    space = matches[0];
  }

  // https://stackoverflow.com/a/34064434
  var doc = new DOMParser().parseFromString(
    input.slice(space.length),
    "text/html"
  );
  return space + doc.documentElement.textContent;
};
