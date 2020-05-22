const AWS = require("aws-sdk");

exports.newDynamoDb = function () {
  return new AWS.DynamoDB();
};

exports._getItem = function (dynamodb) {
  return function (params) {
    return function () {
      return dynamodb.getItem(params).promise();
    };
  };
};

exports._putItem = function (dynamodb) {
  return function (params) {
    return function () {
      return dynamodb.updateItem(params).promise();
    };
  };
};

exports._updateItem = function (dynamodb) {
  return function (params) {
    return function () {
      return dynamodb.updateItem(params).promise();
    };
  };
};
