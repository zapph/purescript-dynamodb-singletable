const avTags = [
  "B",
  "BOOL",
  "BS",
  "L",
  "M",
  "N",
  "NS",
  "NULL",
  "S",
  "SS"
];

exports.readAttributeValue = function (handlers) {
  return function (av) {
    for (let i = 0; i < avTags.length; i++) {
      let tag = avTags[i];
      let v = av[tag];

      if (typeof v !== "undefined") {
        return handlers[tag](v);
      }
    }
    throw "not an attribute value";
  };
};
