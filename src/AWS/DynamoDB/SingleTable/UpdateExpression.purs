module AWS.DynamoDB.SingleTable.UpdateExpression where


-- aws dynamodb update-item --table-name zbucket-dev-db --key '{"PK":{"S":"SHOP#zbucket-dev"},"SK":{"S":"INFO"}}' --update-expression "SET #AToken = :v1" --expression-attribute-names '{"#AToken":"AToken"}' --expression-attribute-values '{":v1":{"S":"foo"}}'

-- SET a=:value1, b=:value2 DELETE :value3, :value4, :value5
