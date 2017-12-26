%{
% @api {get} /parcel/api/content/version Get current version
% @apiName GetCurrentVersion
% @apiGroup Content
% @apiDescription Returns current content version.
% @apiVersion 1.0.0
%
% @apiSampleRequest off
%
% @apiExample {curl} Get current version:
% curl -X GET http://localhost:8080/parcel/api/content/version
%
%}

%{
% @api {get} /parcel/api/content Get current content
% @apiName GetCurrentContent
% @apiGroup Content
% @apiDescription Returns current content.
% @apiVersion 1.0.0
%
% @apiSampleRequest off
%
% @apiExample {curl} Get current content:
% curl -X GET http://localhost:8080/parcel/api/content
%
%}

%{
% @api {get} /parcel/api/content/version/:version Get content by version
% @apiName GetContentByVersion
% @apiGroup Content
% @apiDescription Returns content by version.
% @apiVersion 1.0.0
%
% @apiParam {String} Version version_id
%
% @apiSampleRequest off
%
% @apiExample {curl} Get current content by version:
% curl -X GET http://localhost:8080/parcel/api/content/version/:version
%
%}

%{
% @api {delete} /parcel/api/content/version Unset current version
% @apiName UnsetVersion
% @apiGroup Content
% @apiDescription Deletes 'current.version' file.
% @apiVersion 1.0.0
%
% @apiSampleRequest off
%
% @apiExample {curl} Unset current version:
% curl -X DELETE http://localhost:8080/parcel/api/content/version
%
%}

%{
% @api {post} /parcel/api/content/version Set current version
% @apiName SetVersion
% @apiGroup Content
% @apiDescription Sets current version.
% @apiVersion 1.0.0
%
% @apiSampleRequest off
%
% @apiExample {curl} Set current version:
% curl -X POST -d 'v1.0.0' http://localhost:8080/parcel/api/content/version
%
%
%}

%{
% @api {post} /parcel/api/content/version/:version Put content
% @apiName PutContent
% @apiGroup Content
% @apiDescription Puts content under 'version' name.
% @apiVersion 1.0.0
%
% @apiHeader {String} Content-Type application/octet-stream
%
% @apiParam {String} Version version_id
%
% @apiSampleRequest off
%
% @apiExample {curl} Put content:
% curl -X POST -H 'Content-Type: application/octet-stream' --data-binary '@path/to/archive/file' http://localhost:8080/parcel/api/content/version/:version
%
%
%}
