%{
% @api {post} /parcel/api/measurements Post measurements
% @apiName PostMeasurements
% @apiGroup Measurements
% @apiDescription Post measurements json for processing.
% @apiVersion 1.0.0
%
% @apiParam {String} Version version_id
%
% @apiSampleRequest off
%
% @apiExample {curl} Put content:
% curl -X POST -d ''{"sensorId": "sid1", "dataType": "accelerometer", "dataUnit": "m/s^2", "createdTime": "2017-12-27T12:12:22.260000Z", "data": [1,2,3]}'' http://localhost:8080/parcel/api/measurements
%
%
%}
