const AWS = require("aws-sdk");

exports.newDynamoDb = function () {
  return new AWS.DynamoDB();
};

exports._callDbFnEffP = function (fnName) {
  return function (dynamodb) {
    return function (params) {
      return function () {
        return dynamodb[fnName](params).promise();
      };
    };
  };
};
