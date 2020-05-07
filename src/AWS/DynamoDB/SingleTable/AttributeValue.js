exports.jsonStringify = JSON.stringify;

exports.objEqual = function (a) {
  return function (b) {
    const aKeys = Object.keys(a);
    const bKeys = Object.keys(b);

    if (aKeys.length !== bKeys.length) {
      return false;
    }

    for (let i = 0; i < aKeys.length; i++) {
      let key = aKeys[i];
      if (a[key] !== b[key]) {
        return false;
      }
    }

    return true;
  };
};

exports.readAttributeValue = function (handlers) {
  return function (av) {
    const key = Object.keys(av)[0];
    return handlers[key](av[key]);
  };
};
