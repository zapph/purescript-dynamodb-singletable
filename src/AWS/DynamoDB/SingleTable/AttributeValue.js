exports.jsonStringify = JSON.stringify;

exports.readAttributeValue = function (handlers) {
  return function (av) {
    const key = Object.keys(av)[0];
    return handlers[key](av[key]);
  };
};
