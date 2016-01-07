/*
 * Isomorphic SmartClient
 * Version SNAPSHOT_v10.1p_2015-12-10 (2015-12-10)
 * Copyright(c) 1998 and beyond Isomorphic Software, Inc. All rights reserved.
 * "SmartClient" is a trademark of Isomorphic Software, Inc.
 *
 * licensing@smartclient.com
 *
 * http://smartclient.com/license
 */
// Visit http://www.smartclient.com for more information on Isomorphic SmartClient 

//> @class RestDataSource
// The RestDataSource implements the 4 core DataSource operations using a simple protocol of
// XML or JSON requests and responses sent over HTTP, which can be easily fulfilled by any HTTP
// server technology.
// <P>
// RestDataSource is named for the
// +externalLink{http://www.google.com/search?hl=en&q=REST+HTTP,REST} (REpresentational State
// Transfer) pattern, which in brief says that simple messages passed over HTTP is a sufficient
// protocol for many web applications, without the need for further protocols such as WSDL or
// SOAP.
// <P>
// A RestDataSource is used just like a normal DataSource.  RestDataSources are pre-configured,
// using the general-purpose databinding facilities of DataSources, to expect a particular
// format for responses and to send requests in a specific format.   These request and
// response formats represent Isomorphic's recommended best practices for binding SmartClient
// to backends which do not already support a similar, pre-existing request and response
// format and where the SmartClient Java Server cannot be used.  
// <P>
// If you have a pre-existing REST or WSDL service which is difficult to change, consider
// adapting SmartClient to the existing service instead, by starting with a normal
// +link{DataSource} and using the 
// +link{group:clientDataIntegration,client-side data integration} facilities to create a
// mapping between SmartClient's +link{DSRequest} and +link{DSResponse} objects and the message
// formats of your existing services.  <b>NOTE</b>: do <b>not</b> begin this process by
// creating or subclassing RestDataSource; for a <b>pre-existing</b> service which is unrelated
// to the protocol documented for RestDataSource, start by configuring or subclassing
// +link{DataSource} instead.
// <P>
// RestDataSource is typically used with PHP, Ruby, Python, Perl or custom server technologies,
// and represents an alternative to installing the SmartClient Server in a Java technology
// stack, or using +link{group:wsdlBinding,WSDL-based binding} with .NET or other WSDL-capable
// technologies.  Note that SmartClient Server also provides built-in support for the REST
// protocol via its RESTHandler servlet; this is primarily to allow non-SmartClient clients
// to make use of DataSource operations.  If you particularly wished to do so, you could use
// RestDataSource to make a SmartClient app talk to the SmartClient Server using REST rather 
// than the proprietary wire format normally used when communicating with SmartClient Server
// (this is how we are able to write automated tests for the RESTHandler servlet).  However,
// doing this provides no benefit, imposes a number of inconveniences, and makes a handful 
// of server-based features less useful 
// (+link{DataSourceField.viewRequiresAuthentication,field-level declarative security}, for 
// example), so we strongly recommend that you do <em>not</em> do this; it is only mentioned
// here for completeness while we are discussing REST.
// <P>
// The request and response formats used by the RestDataSource allow for many of the available
// features of SmartClient's databinding system to be used, including data paging, searching &
// sorting, +link{dsRequest.oldValues,long transactions}, 
// +link{ResultSet,automatic cache sync}, +link{group:relogin,relogin} and 
// +link{RPCManager.startQueue,queuing}.  However,  advanced
// features such as +link{group:upload,uploading / binary fields} and  
// +link{listGrid.exportData,export} aren't available with RestDataSource and need to be 
// re-implemented as needed.  Most, though not all, +link{group:iscServer,server-based features}
// are still available when using RestDataSource, as long as you are also using the RESTHandler 
// servlet that is part of SmartClient Server.  However, as noted above, this approach is not 
// recommended; if you are using Isomorphic technology both client- and server-side, it makes
// more sense to use the proprietary wire format.
// <P>
// <b>RestDataSource and binary data</b>
// <P>
// Binary data in a response provided to a RestDataSource must be delivered as valid XML or
// JSON Strings.  Once delivered to the browser as Strings, there is no way to trigger the
// browser's "Save As" dialog to download the data, and in most cases no way to trigger other
// helper applications that might be launched to handle binary data (such as Excel or a PDF
// viewer).  Hence for binary it usually makes sense to make a direct request via
// RPCManager.sendRequest() with downloadResult:true, separate from RestDataSource.
// <P>
// If you are using the SmartClient Server included in Pro, Power end Enterprise to handle your
// REST requests server-side, there is transparent support for conversion between Java 
// <code>InputStream</code>s representing binary data, and Strings containing that binary 
// data encoded using the <a href=http://en.wikipedia.org/wiki/Base64>Base64 algorithm</a>.
// Thus, on the server, the binary data is in its raw binary form, with transparent conversion
// to or from Base64 for messages to or from the REST client.
// <P>
// <span style="font-weight:bold;font-size:16px;">Examples</span>
// <p>
// <b>XML formatted responses:</b>
// <P>
// RestDataSource expects a response like the following in response to a "fetch" request:
// <pre>
// &lt;response&gt;
//    &lt;status&gt;0&lt;/status&gt;
//    &lt;startRow&gt;0&lt;/startRow&gt;
//    &lt;endRow&gt;76&lt;/endRow&gt;
//    &lt;totalRows&gt;546&lt;/totalRows&gt;
//    &lt;data&gt;
//      &lt;record&gt;
//          &lt;field1&gt;value&lt;/field1&gt;
//          &lt;field2&gt;value&lt;/field2&gt;
//      &lt;/record&gt;
//      &lt;record&gt;
//          &lt;field1&gt;value&lt;/field1&gt;
//          &lt;field2&gt;value&lt;/field2&gt;
//      &lt;/record&gt;
//      <i>... 76 total records ... </i>
//    &lt;/data&gt;
// &lt;/response&gt;
// </pre>
// The &lt;status&gt; element indicates whether the fetch operation was successful 
// (see +link{group:statusCodes}).
// <P>
// The &lt;data&gt; element contains a list of record nodes, each of which represents a record
// returned by the server.  The optional &lt;startRow&gt;, &lt;endRow&gt; and &lt;totalRows&gt;
// elements are needed only if data paging is in use, and populate the
// +link{dsResponse.startRow,startRow}, +link{dsResponse.endRow,endRow} and
// +link{dsResponse.totalRows,totalRows} properties of the +link{DSResponse}.
// <P>
// Note: for a more compact format, simple field values may be specified on record 
// nodes directly as attributes - in this case a record element might be structured like this:
// <pre>
//     &lt;record field1="value" field2="value" /&gt;
// </pre>
// <p>
// Note that a RestDataSource will bypass browser caching of all responses by default.  See
// +link{dataSource.preventHTTPCaching}.
// <p>
// Successful "add" or "update" request responses are similar in format - in this case the data
// element would be expected to contain a single record object containing the details of the
// record, as saved on the server.
// <P>
// The response from a "remove" operation would again include status and data elements, but in
// this case, only the primary key field value(s) of the removed record would be expected to be 
// present under the data element.
// <p>
// If a validation failure occurred on the server, the response would
// have status set to +link{RPCResponse.STATUS_VALIDATION_ERROR} [<code>-4</code>],
// and any validation errors could be included as per-field sub-elements of an "errors"
// element.  For a validation error, the response is not expected to contain any
// &lt;data&gt; element.  
// <P>
// A response showing a validation error might look like this:
// <pre>
// &lt;response&gt;
//    &lt;status&gt;-4&lt;/status&gt;
//    &lt;errors&gt;
//      &lt;field1&gt;
//          &lt;errorMessage&gt;A validation error occurred for this field&lt;/errorMessage&gt;
//      &lt;/field1&gt;
//    &lt;/errors&gt;
// &lt;/response&gt;
// </pre>
// <P>
// An unrecoverable error, such as an unexpected server failure, can be flagged by setting
// &lt;status&gt; to -1 and setting &lt;data&gt; to an error message.  In this case the
// &lt;errors&gt; element is not used (it's specific to validation errors).  An unrecoverable
// error causes all response processing to be skipped and +link{RPCManager.handleError} to be
// invoked, which by default will show the provided error message as an alert using
// +link{classMethod:isc.warn()}.
// <p>
// <b>JSON formatted responses:</b>
// <P>
// JSON format responses are expected to contain the same data / meta-data as XMLresponses,
// encapsulated in a simple object with a <code>"response"</code> attribute.<br>
// The response to a "fetch" request would therefore have this format:<br>
// <pre>
// {
//    "response": {
//       "status": 0,
//       "startRow": 0,
//       "endRow": 76,
//       "totalRows": 546,
//       "data": [
//           {"field1": "value", "field2": "value"},
//           {"field1": "value", "field2": "value"},
//           <i>... 76 total records ...</i>
//       ]
//    }
// }
// </pre>
// The structure successful for "add", "update" and "remove" responses would be similar, though
// the data array would be expected to contain only a single object, representing the values as
// saved.  This allows the server to return values such as an auto-generated sequence
// primaryKey, a last modified timestamp, or similar server-generated field values.
// <P>
// For a remove, only the value for the primaryKey field[s] would be required.
// <p>
// For a validation error, the <code>status</code> attribute would be set to 
// +link{RPCResponse.STATUS_VALIDATION_ERROR} [<code>-4</code>], and errors would
// be specified in the <code>errors</code> attribute of the response. For example:
// <pre>
// {    "response":
//      {   "status": -4,
//          "errors":
//              {   "field1": {"errorMessage": "A validation error on field1"},
//                  "field2": {"errorMessage": "A validation error on field2"}
//              }
//      }
// }
// </pre>
// An array of errors may also be returned for a single field, like this:
// <pre>
// {    "response":
//      {   "status": -4,
//          "errors":
//              {   "field1": [
//                      {"errorMessage": "First error on field1"},
//                      {"errorMessage": "Second error on field1"}
//                  ]
//              }
//      }
// }
// </pre>
// <P>
// As with the XML format above, an unrecoverable error is indicated by setting the
// <code>status</code> attribute to -1 and the <code>data</code> property to the error message.
// <P>
// <b>Server inbound data formats</b>
// <P>
// The format of data sent to the server is determined by the +link{OperationBinding.dataProtocol}
// specified for the operation. Request data is sent as parameters if the format is 
// specified as <code>"getParams"</code> or <code>"postParams"</code>.
// <P>
// In this case, the parameters sent to the server will consist of the DSRequest's data, and any
// parameters explicitly specified on the DSRequest object (as +link{RPCRequest.params}.<br> 
// If +link{RestDataSource.sendMetaData} is true, the DSRequest meta 
// data properties will also be present as parameters, prefixed with 
// +link{RestDataSource.metaDataPrefix}.
// <P>
// Example URL constructed with the metaDataPrefix set to <code>"_"</code> (the default):
// <p>
// <code>
// &nbsp;&nbsp;&nbsp;<i>[dataURL]</i>?field1=value1&_operationType=fetch&_startRow=0&_endRow=50&_sortBy=-field2&_dataSource=dsName
// </code>
// <p>
// In this case the server would be able to separate the request's data from the meta data 
// via the <code>"_"</code> prefix.
// <P>
// If data is sent to the server via the <code>"postMessage"</code> dataProtocol, the data will
// be serialized as an XML or JSON message according to the <code>dataFormat</code> setting.
// Both XML and JSON messages will contain request metadata such as startRow and endRow, and
// will appear exactly as though the subset of the +link{DSRequest} that is meaningful to the
// server had been passed to +link{dataSource.xmlSerialize()} or +link{JSON.encode()}
// respectively.
// <P>
// An example of an XML message might look like this:
// <pre>
//    &lt;request&gt;
//        &lt;data&gt;
//            &lt;countryCode&gt;US&lt;/countryCode&gt;
//            &lt;countryName&gt;Edited Value&lt;/countryName&gt;
//            &lt;capital&gt;Edited Value&lt;/capital&gt;
//            &lt;continent&gt;Edited Value&lt;/continent&gt;
//        &lt;/data&gt;
//        &lt;dataSource&gt;countryDS&lt;/dataSource&gt;
//        &lt;operationType&gt;update&lt;/operationType&gt;
//    &lt/request&gt;
// </pre>
// An example of an XML message for a fetch operation passing simple criteria:
// <pre>
//    &lt;request&gt;
//        &lt;data&gt;
//            &lt;continent&gt;North America&lt;/continent&gt;
//        &lt;/data&gt;
//        &lt;dataSource&gt;countryDS&lt;/dataSource&gt;
//        &lt;operationType&gt;fetch&lt;/operationType&gt;
//        &lt;startRow&gt;0&lt;/startRow&gt;
//        &lt;endRow&gt;75&lt;/endRow&gt;
//        &lt;componentId&gt;worldGrid&lt;/componentId&gt;
//        &lt;textMatchStyle&gt;exact&lt;/textMatchStyle&gt;
//    &lt/request&gt;
// </pre>
// And an example of an XML message for a fetch operation passing AdvancedCriteria:
// <pre>
//    &lt;request&gt;
//        &lt;data&gt;
//            &lt;_constructor&gt;AdvancedCriteria&lt;/_constructor&gt;
//            &lt;operator&gt;or&lt;/operator&gt;
//            &lt;criteria&gt;
//                &lt;criterion&gt;
//                    &lt;fieldName&gt;continent&lt;/fieldName&gt;
//                    &lt;operator&gt;equals&lt;/operator&gt;
//                    &lt;value&gt;North America&lt;/value&gt;
//                &lt;/criterion&gt;
//                &lt;criterion&gt;
//                    &lt;operator&gt;and&lt;/operator&gt;
//                    &lt;criteria&gt;
//                        &lt;criterion&gt;
//                            &lt;fieldName&gt;continent&lt;/fieldName&gt;
//                            &lt;operator&gt;equals&lt;/operator&gt;
//                            &lt;value&gt;Europe&lt;/value&gt;
//                        &lt;/criterion&gt;
//                        &lt;criterion&gt;
//                            &lt;fieldName&gt;population&lt;/fieldName&gt;
//                            &lt;operator&gt;greaterThan&lt;/operator&gt;
//                            &lt;value&gt;50000000&lt;/value&gt;
//                        &lt;/criterion&gt;
//                    &lt;/criteria&gt;
//                &lt;/criterion&gt;
//            &lt;/criteria&gt;
//        &lt;/data&gt;
//        &lt;dataSource&gt;countryDS&lt;/dataSource&gt;
//        &lt;operationType&gt;fetch&lt;/operationType&gt;
//        &lt;startRow&gt;0&lt;/startRow&gt;
//        &lt;endRow&gt;75&lt;/endRow&gt;
//        &lt;componentId&gt;worldGrid&lt;/componentId&gt;
//    &lt/request&gt;
// </pre>
// An example of an XML message for a fetch operation when using +link{group:serverSummaries,server-side summaries}:
// <pre>
//    &lt;request&gt;
//        &lt;data&gt;&lt;/data&gt;
//        &lt;dataSource&gt;countryDS&lt;/dataSource&gt;
//        &lt;operationType&gt;fetch&lt;/operationType&gt;
//        &lt;summaryFunctions&gt;
//            &lt;pk&gt;count&lt;/pk&gt;
//        &lt;/summaryFunctions&gt;
//        &lt;groupBy&gt;member_g8&lt;/groupBy&gt;
//    &lt/request&gt;
// </pre>
// JSON messages are just the plain JSON form of the structures shown in the above XML
// examples. The advanced criteria XML example above but in JSON form:
// <pre>
// {
//     data: {
//         _constructor: "AdvancedCriteria",
//         operator: "or",
//         criteria: [
//             {
//                 fieldName: "continent",
//                 operator: "equals",
//                 value: "North America
//             },
//             {
//                 operator: "and", criteria: [
//                     {
//                         fieldName: "continent",
//                         operator: "equals",
//                         value: "Europe"
//                     },
//                     {
//                         fieldName: "population",
//                         operator: "greaterThan",
//                         value: 50000000
//                     }
//                 ]
//             }
//         ]
//     }
//     dataSource: "countryDS",
//     operationType: "fetch",
//     startRow: 0,
//     endRow: 75,
//     componentId: "worldGrid"
// }
// </pre>
// The +link{restDataSource.operationBindings,default OperationBindings} for a RestDataSource
// specify dataProtocol as "getParams" for the fetch operation, and "postParams" for update,
// add and remove operations.  Note that most webservers impose a limit on the maximum size 
// of GET requests (specifically, on the size of the request URL + HTTP headers).  Using
// dataProtocol:"getParams" for "fetch" operations that involve complex AdvancedCriteria
// will result in a JSON serialization of the AdvancedCriteria in the request URL, and when
// combined with large cookies this can easily overflow the default limits on certain
// webservers (see
// +externalLink{http://stackoverflow.com/questions/686217/maximum-on-http-header-values}).
// For this reason, we recommend that you use the "postMessage" protocol whenever you are
// intending to use AdvancedCriteria with RestDataSource.

// <P>
// <b>Date, time and datetime values</b>
// <P>
// Date, time and datetime values must be communicated using XML Schema format, as in the 
// following examples:
// <pre>
// &nbsp;&nbsp;&lt;dateField&gt;2007-04-22&lt;/dateField&gt;
// &nbsp;&nbsp;&lt;timeField&gt;11:07:13&lt;/timeField&gt;
// &nbsp;&nbsp;&lt;dateTimeField&gt;2007-04-22T11:07:13&lt;/dateTimeField&gt;
// &nbsp;&nbsp;&lt;dateTimeField&gt;2007-04-22T11:07:13.582&lt;/dateTimeField&gt;
// </pre>
// <P>
// And the equivalent in JSON:
// <pre>
// &nbsp;&nbsp;dateField: "2007-04-22"
// &nbsp;&nbsp;timeField: "11:07:13"
// &nbsp;&nbsp;dateTimeField: "2007-04-22T11:07:13"
// &nbsp;&nbsp;dateTimeField: "2007-04-22T11:07:13.582"
// </pre>
// <P>
// Both RestDataSource on the client-side and the RESTHandler servlet on the server side 
// automatically handle encoding and decoding temporal values using these formats.  Both also
// handle datetime formats including or excluding milliseconds automatically.  When encoding,
// both honot the +link{DataSource.trimMilliseconds} setting on the DataSource, falling back
// to the <code>server.properties</code> setting <code>rest.trimMilliseconds</code>; when
// decoding, both detect whether or not to try to parse milliseconds based on the string they 
// receive.
// <P>
// Fields of type "date" and "time" are considered to hold logical date and time values, as 
// discussed in the +link{group:dateFormatAndStorage,date and time handling article}, and are 
// not affected by timezones.  Fields of type "datetime" will be converted to UTC on the 
// client side by RestDataSource, and will be sent back down to the client as UTC by the 
// server-side RESTHandler.  We recommend that your own REST client and/or server code do the
// same thing (ie, transmit all datetime values in both directions as UTC).  Note that the 
// examples given above give no timezone information, and will be treated by the SmartClient
// Server as UTC values.  If you wish to work with datetime values in a particular timezone,
// use a format like this:
// <pre>
// &nbsp;&nbsp;&lt;dateField&gt;2007-04-22T11:07:13-0800&lt;/dateField&gt;
// &nbsp;&nbsp;&lt;dateField&gt;2012-11-19T22:12:04+0100&lt;/dateField&gt;
// </pre>
// <p>
// And the equivalent in JSON:
// <pre>
// &nbsp;&nbsp;dateTimeField: "2007-04-22T11:07:13-0800"
// &nbsp;&nbsp;dateTimeField: "2012-11-19T22:12:04+0100"
// </pre>
// <P>
// <b>NOTE:</b> Although we refer above to XML Schema format, the format used for specifying
// timezone offset is slightly different from XML Schema - as shown in the above examples, you
// specify "+HHMM" or "-HHMM", as opposed to the XML Schema format which requires a ":" character
// between the hours and minutes.  The reason for this difference is simply that the Java 
// SimpleDateFormat class imposes it.
// <p>
// <b>RestDataSource queuing support</b>
// <P>
// RestDataSource supports +link{RPCManager.startQueue,queuing} of DSRequests.  This allows 
// you to send multiple requests to the server in a single HTTP turnaround, thus minimizing 
// network traffic and allowing the server to treat multiple requests as a single transaction,
// if the server is able to do so (in Power Edition and above, the SmartClient Server
// transparently supports grouping multiple REST requests in a queue into a single database
// transaction when using one of the built-in DataSource types).  Note that you can disable 
// queuing support with the +link{RestDataSource.disableQueuing} flag.
// <P>
// If you want to use queuing with RestDataSource, you must use the "postMessage" dataProtocol
// with either XML or JSON dataFormat.  Message format is similar to the non-queued examples 
// shown earlier: it is simply extended to cope with the idea of multiple DSRequests 
// encapsulated in the message.
// <P>
// An example of the XML message sent from RestDataSource to the server for two update requests
// combined into a queue, using XML dataFormat:
// <pre>
// &lt;transaction&gt;
//     &lt;operations&gt;
//         &lt;request&gt;
//             &lt;data&gt;
//                 &lt;pk&gt;1&lt;/pk&gt;
//                 &lt;countryName&gt;Edited Value&lt;/countryName&gt;
//                 &lt;capital&gt;Edited Value&lt;/capital&gt;
//                 &lt;continent&gt;Edited Value&lt;/continent&gt;
//             &lt;/data&gt;
//             &lt;dataSource&gt;countryDS&lt;/dataSource&gt;
//             &lt;operationType&gt;update&lt;/operationType&gt;
//         &lt/request&gt;
//         &lt;request&gt;
//             &lt;data&gt;
//                 &lt;pk&gt;2&lt;/pk&gt;
//                 &lt;capital&gt;Edited Value&lt;/capital&gt;
//                 &lt;population&gt;123456&lt;/population&gt;
//             &lt;/data&gt;
//             &lt;dataSource&gt;countryDS&lt;/dataSource&gt;
//             &lt;operationType&gt;update&lt;/operationType&gt;
//         &lt/request&gt;
//     &lt;/operations&gt;
// &lt;transaction&gt;
// </pre>
// And the same message in JSON format:
// <pre>
// { 
//     transaction: { 
//         operations: [{
//             dataSource:"countryDS", 
//             operationType:"update", 
//             data: {
//                 pk: 1
//                 countryName: "Edited Value",
//                 capital: "Edited Value",
//                 continent: "Edited Value"
//             }
//         }, {
//             dataSource:"countryDS", 
//             operationType:"update", 
//             data: {
//                 pk: 2,
//                 capital: "Edited Value",
//                 popuilation: 123456
//             }
//         }]
//     }
// }
// </pre>
// RestDataSource expects the response to a queue of requests to be a queue of responses in 
// the same order as the original requests.  Again, the message format is very similar to the 
// unqueued REST format, it just has an outer container construct.  Note also that the 
// individual DSResponses in a queued response have an extra property, 
// +link{DSResponse.queueStatus,<code>queueStatus</code>}.  This allows each individual
// response to determine whether the queue as a whole succeeded.  For example, if the first
// update succeeded but the second failed validation, the first response would have a
// <code>status</code> of 0, but a <code>queueStatus</code> of -1, while the second response
// would have both properties set to -1.
// <P>
// The update queue example given above would expect a response like this (in XML):
// <pre>
// &lt;responses&gt;
//     &lt;response&gt;
//         &lt;status&gt;0&lt;/status&gt;
//         &lt;queueStatus&gt;0&lt;/queueStatus&gt;
//         &lt;data&gt;
//             &lt;record&gt;
//                 &lt;countryName&gt;Edited Value&lt;/countryName&gt;
//                 &lt;gdp&gt;1700.0&lt;/gdp&gt;
//                 &lt;continent&gt;Edited Value&lt;/continent&gt;
//                 &lt;capital&gt;Edited Value&lt;/capital&gt;
//                 &lt;pk&gt;1&lt;/pk&gt;
//             &lt;/record&gt;
//         &lt;/data&gt;
//     &lt;/response&gt;
//     &lt;response&gt;
//         &lt;status&gt;0&lt;/status&gt;
//         &lt;queueStatus&gt;0&lt;/queueStatus&gt;
//         &lt;data&gt;
//             &lt;record&gt;
//                 &lt;countryName&gt;United States&lt;/countryName&gt;
//                 &lt;gdp&gt;7247700.0&lt;/gdp&gt;
//                 &lt;continent&gt;North America&lt;/continent&gt;
//                 &lt;independence&gt;1776-07-04&lt;/independence&gt;
//                 &lt;capital&gt;Washington DC&lt;/capital&gt;
//                 &lt;pk&gt;2&lt;/pk&gt;
//                 &lt;population&gt;123456&lt;/population&gt;
//             &lt;/record&gt;
//         &lt;/data&gt;
//     &lt;/response&gt;
// &lt;/responses&gt;
// </pre>
// And in JSON:
// <pre>
// [
// {
//     "response": {
//         "queueStatus": 0,
//         "status": 0, 
//         "data": [{
//             "countryName": "Edited Value",
//             "gdp": 1700.0,
//             "continent": "Edited Value",
//             "capital": "Edited Value",
//             "pk": 1
//         }]
//     }
// },
// {
//     "response": {
//         "queueStatus": 0,
//         "status": 0,
//         "data": [{
//             "countryName": "United States",
//             "gdp": 7247700.0,
//             "continent": "North America",
//             "independence": "1776-07-04",
//             "capital": "Washington DC",
//             "pk": 2,
//             "population": 123456
//         }]
//     }
// }
// ]
// </pre>
// <b>Hierarchical (Tree) data:</b>
// <P>
// To create a hierarchical DataSource, in the DataSource's <code>fields</code> array, a field 
// must be specified as the parent id field - the field which will contain a pointer to the
// id of each node's parent. 
// This can be achieved by setting the +link{DataSourceField.foreignKey} and the 
// +link{DataSourceField.rootValue} attributes on the field definition. For example:
// <pre>
// RestDataSource.create({
//    ID:"supplyItem",
//    fields : [
//        {name:"itemId", type:"sequence", primaryKey:true},
//        {name:"parentId", type:"integer", foreignKey:"supplyItem.itemId", rootValue:0},
//        ...
//    ]
// });
// </pre>
// Tree Data is then treated on the server as a flat list of records linked by parent id.
// <P>
// Tree data is typically displayed using a dataBound +link{class:TreeGrid} component. TreeGrids
// automatically create a +link{class:ResultTree} data object, which requests data directly
// from the DataSource.  ResultTrees load data on demand, only requesting currently visible 
// (open) nodes from the server. This is handled by including a specified value for the parent 
// id field in the request criteria.<br>
// To implement a standard load-on-demand tree RestDataSource back end, you should therefore 
// simply return the set of nodes that match the criteria passed in. 
// For example, if your DataSource was defined as the "supplyItem" code snippet above, 
// a fetch request for all children of a node with <code>itemId</code> set to <code>12</code> 
// would have <code>"parentId"</code> set to <code>12</code> in the request criteria.
// A valid response would then contain all the records that matched this criteria. For example:
// <pre>
// &lt;response&gt;
//    &lt;status&gt;0&lt;/status&gt;
//    &lt;data&gt;
//      &lt;record&gt;
//          &lt;itemId&gt;15&lt;/itemId&gt;
//          &lt;parentId&gt;12&lt;/parentId&gt;
//      &lt;/record&gt;
//      &lt;record&gt;
//          &lt;itemId&gt;16&lt;/itemId&gt;
//          &lt;parentId&gt;12&lt;/parentId&gt;
//      &lt;/record&gt;
//    &lt;/data&gt;
// &lt;/response&gt;
// </pre>
// The structure of responses for Add, Update and Delete type requests will be the 
// same regardless of whether the data is hierarchical. However you should be aware that 
// the underlying data storage may need to be managed slightly differently in some cases.
// <P>
// Specifically, Add and Update operations may change the structure of the tree by returning a 
// new parent id field value for the modified node. Depending on how your data is stored you 
// may need to include special back-end logic to handle this.
// <P>
// Also, if a user deletes a folder within a databound tree, any children of that folder will 
// also be dropped from the tree, and can be removed from the back-end data storage.
// <P>
// Note: For a general overview of binding components to Tree structured data, see 
// +link{group:treeDataBinding, Tree Databinding}.
// 
// @treeLocation Client Reference/Data Binding
// @visibility external
// @example restEditSave
//<
isc.defineClass("RestDataSource", "DataSource");


isc.RestDataSource.addProperties({
    //> @attr restDataSource.dataProtocol (DSProtocol : null : [IR])
    // Rather than setting +link{dataSource.dataProtocol}, to control the format in which 
    // inputs are sent to the dataURL, you must specify a replacement +link{OperationBinding} 
    // and specify +link{OperationBinding.dataProtocol} on that <code>operationBinding</code>.
    // <P>
    // This is because <code>RestDataSource</code> specifies default
    // <code>operationBindings</code> for all operationTypes - see
    // +link{restDataSource.operationBindings}.
    //
    // @group clientDataIntegration
    // @group serverDataIntegration
    // @serverDS allowed
    // @visibility external
    //<
    
    serverType:"generic",

    //> @attr restDataSource.dataFormat (DSDataFormat : "xml" : IR)
    // Expected format for server responses. RestDataSources handle <code>"json"</code> and
    // <code>"xml"</code> format responses by default. See class overview documentation for 
    // examples of responses in each format.
    // @visibility external
    //<
    dataFormat:"xml",
    
    //> @attr restDataSource.xmlRecordXPath    (string : "/response/data/*" : IR)
    // <code>recordXPath</code> mapping to the data node of XML returned by the server.
    // Applies if this.dataFormat is set to <code>"xml"</code>.<br>
    // The default value will pick up data from a response structured as follows:<br>
    // <pre>
    // &lt;response&gt;
    //    &lt;status&gt;0&lt;/status&gt;
    //    &lt;data&gt;
    //      &lt;record&gt;
    //          &lt;field1&gt;value&lt;/field1&gt;
    //          &lt;field2&gt;value&lt;/field2&gt;
    //      &lt;/record&gt;
    //      &lt;record&gt;
    //          &lt;field1&gt;value&lt;/field1&gt;
    //          &lt;field2&gt;value&lt;/field2&gt;
    //      &lt;/record&gt;
    //    &lt;/data&gt;
    // &lt;/response&gt;
    // </pre>
    // @visibility external
    //<
    xmlRecordXPath:"/response/data/*",

    //> @attr restDataSource.xmlNamespaces (Object : See below : IR)
    // When +link{dataFormat} is "xml", <code>xmlNamespaces</code> configures the set of
    // namespace prefixes that are added to the document element of the XML message sent to the
    // server.  Format is the same as +link{dataSource.xmlNamespaces}.
    // <P>
    // By default, the "xsi" prefix is bound to "http://www.w3.org/2001/XMLSchema-instance" in
    // order to allow explicit null values in Records to be sent for
    // +link{dataSourceField.nillable,fields declared nillable}.  Set to null to avoid any
    // prefixes being added.
    //
    // @see dataSourceField.nillable
    // @visibility external
    //<
    xmlNamespaces : { xsi: "http://www.w3.org/2001/XMLSchema-instance" },
    
    //> @attr restDataSource.jsonRecordXPath    (string : "/response/data" : IR)
    // <code>recordXPath</code> mapping to the data node of json returned by the server.
    // Applies if this.dataFormat is set to <code>"json"</code><br>
    // The default value will pick up data from a response structured as follows:<br>
    // <pre>
    // {response:
    //  {status:0,
    //   data:[
    //      {field1:"value", field2:"value"},
    //      {field1:"value", field2:"value"}
    //   ]
    // }
    // </pre>
    // @visibility external
    //<
    jsonRecordXPath:"/response/data",
    
    //> @attr restDataSource.recordXPath (string : null : IRW)
    // For RestDataSources, by default, either the +link{RestDataSource.xmlRecordXPath} or 
    // +link{RestDataSource.jsonRecordXPath} is used based on the +link{dataFormat}
    // setting.
    // <P>
    // Note that you can also apply record xpath binding via
    // +link{operationBinding.recordXPath}.
    //
    // @visibility external
    //< 
    
    //> @attr restDataSource.prettyPrintJSON (Boolean : true : IR)
    // When using dataFormat:"json" and dataProtocol:"postMessage" should we use the
    // +link{JSONEncoder.prettyPrint} feature to enable indented, highly readable JSON messages.
    // <P>
    // True by default because the bandwidth involved is generally negligible and the benefits for
    // troubleshooting are key.
    //
    // @visibility external
    //<
    prettyPrintJSON: true,
    
    dataFormatParamName: "isc_dataFormat",
    
    //> @attr restDataSource.disableQueuing (Boolean : false : IRW)
    // If set, disables +link{RPCManager.startQueue,request queuing} for this RestDataSource.
    //
    // @visibility external
    //<
    
	
    //> @attr restDataSource.jsonPrefix (String : See below : IRW)
    // Allows you to specify an arbitrary prefix string to apply to all json format responses 
    // sent from the server to this application.  The client will expect to find this prefix 
    // on any JSON response received for this DataSource, and will strip it off before evaluating
    // the response text.
    // <p>
    // The default prefix is "&lt;SCRIPT&gt;//'\"]]&gt;&gt;isc_JSONResponseStart&gt;&gt;".
    // <p>
    // The inclusion of such a prefix ensures your code is not directly executable outside of 
    // your application, as a preventative measure against 
    // <a href='http://www.google.com/search?q=javascript+hijacking'>javascript hijacking</a>.
    // <p>
    // You can switch off JSON wrapping altogether by setting both this and +link{jsonSuffix}
    // to empty strings.  
    // <p>
    // If you are using SmartClient Server's RESTHandler servlet, see the server-side Javadocs
    // for details of how to change the way JSON wrapping works on the server side.
    //
    // @see RestDataSource.jsonSuffix
    // @visibility external
    //<
    jsonPrefix: "<SCRIPT>//'\"]]>>isc_JSONResponseStart>>",
	
    //> @attr restDataSource.jsonSuffix (String : See below : IRW)
    // Allows you to specify an arbitrary suffix string to apply to all json format responses 
    // sent from the server to this application.  The client will expect to find this suffix 
    // on any JSON response received for this DataSource, and will strip it off before evaluating
    // the response text.
    // <p>
    // The default suffix is "//isc_JSONResponseEnd".
    // @see RestDataSource.jsonPrefix
    // @visibility external
    //<
    jsonSuffix: "//isc_JSONResponseEnd",
    
    // Override init to pick up these paths
    init : function () {
        
        this.serverType = "generic";
        this.recordXPath = this.recordXPath || 
                (this.dataFormat == "xml" ? this.xmlRecordXPath : this.jsonRecordXPath);
        return this.Super("init", arguments);
    },
    
    //Added to resolve an issue with recursion within SmartGWT, when the  
    // fetchDataURL property is being requested.
    getProperty : function (propName) {
        if (propName == "fetchDataURL") return this.fetchDataURL;
        var getter = this._getGetter(propName);
        if (getter) return this[getter]();
        return this[propName];
    }, 
    
    //> @attr RestDataSource.operationBindings (Array of OperationBinding : [...] : IR)
    // RestDataSource OperationBindings set to specify default dataProtocol per operationType.
    // Default databindings are:
    // <pre>
    //   operationBindings : [
    //     {operationType:"fetch", dataProtocol:"getParams"},
    //     {operationType:"add", dataProtocol:"postParams"},
    //     {operationType:"remove", dataProtocol:"postParams"},
    //     {operationType:"update", dataProtocol:"postParams"} 
    //   ],
    // </pre>
    // If you are integrating with a +link{RestDataSource,REST} server that requires the more
    // obscure +link{rpcRequest.httpMethod}s of "PUT", "DELETE" or "HEAD", you can specify these
    // httpMethod settings via +link{operationBinding.requestProperties}.  dataProtocol settings
    // that mention "GET" or "POST" are compatible with these additional HTTP methods as well.
    // Typical +link{dataSource.operationBindings,operationBindings} for a REST server that uses
    // "PUT" and "DELETE" are as follows:
    // <pre>
    //   operationBindings:[
    //     {operationType:"fetch", dataProtocol:"getParams"},
    //     {operationType:"add", dataProtocol:"postParams"},
    //     {operationType:"remove", dataProtocol:"getParams", requestProperties:{httpMethod:"DELETE"}},
    //     {operationType:"update", dataProtocol:"postParams", requestProperties:{httpMethod:"PUT"}}
    //   ],
    // </pre>
    // <p>
    // Note that dataProtocol:"postMessage" is always used when
    // +link{RPCManager.startQueue,queuing} is used to send multiple DSRequests to the server
    // as a single HttpRequest.  See +link{RestDataSource} docs, "queuing support".  We also 
    // recommend that you use the "postMessage" protocol whenever you are intending to use 
    // AdvancedCriteria with RestDataSource - this is discussed in the section "Server inbound
    // data format" in the +link{class:RestDataSource,RestDataSource overview}.
    // <p>
    // 
    // @visibility external
    //<
    operationBindings:[
       {operationType:"fetch", dataProtocol:"getParams"},
       {operationType:"add", dataProtocol:"postParams"},
       {operationType:"remove", dataProtocol:"postParams"},
       {operationType:"update", dataProtocol:"postParams"}
    ],
    
    //> @attr restDataSource.dataURL  (String : null : IR)
    // Default URL to contact to fulfill all DSRequests.  
    // RestDataSources also allow per-operationType dataURLs to be set via
    // <ul>
    // <li>+link{RestDataSource.fetchDataURL}</li>
    // <li>+link{RestDataSource.addDataURL}</li> 
    // <li>+link{RestDataSource.updateDataURL}</li>
    // <li>+link{RestDataSource.removeDataURL}</li>
    // </ul>
    // <b>NOTE:</b>: when using +link{RPCManager.startQueue,queuing} with RestDataSource, an
    // HTTP request containing mixed +link{dsRequest.operationType,operationTypes} (such as a
    // mixture of "add", "update" and "remove" operations resulting from
    // +link{listGrid.autoSaveEdits,Grid Mass Editing}) can only go to one URL, so you
    // should not set distinct URLs for each <code>operationType</code>; doing so will break
    // queuing of mixed operationTypes: multiple requests will be sent to distinct URLs, and a
    // warning logged.
    // @visibility external
    //<
    
    //> @attr restDataSource.fetchDataURL (String : null : IR)
    // Custom +link{dataSource.dataURL,dataURL} for +link{DSRequest,DSRequests} with
    // +link{dsRequest.operationType,operationType} "fetch".
    // <p>
    // Use +link{RestDataSource.dataURL} to configure a single URL for all requests, which is
    // required to support +link{RPCManager.startQueue()}.
    // @visibility external
    //<

    //> @attr restDataSource.updateDataURL (String : null : IR)
    // Custom +link{dataSource.dataURL,dataURL} for +link{DSRequest,DSRequests} with
    // +link{dsRequest.operationType,operationType} "update".
    // <p>
    // See +link{RestDataSource.dataURL} to configure a single URL for all requests, which is
    // required to support +link{RPCManager.startQueue()}.
    // @visibility external
    //<
    
    //> @attr restDataSource.addDataURL (String : null : IR)
    // Custom +link{dataSource.dataURL,dataURL} for +link{DSRequest,DSRequests} with
    // +link{dsRequest.operationType,operationType} "add".
    // <p>
    // See +link{RestDataSource.dataURL} to configure a single URL for all requests, which is
    // required to support +link{RPCManager.startQueue()}.
    // @visibility external
    //<
    
    //> @attr restDataSource.removeDataURL (String : null : IR)
    // Custom +link{dataSource.dataURL,dataURL} for +link{DSRequest,DSRequests} with
    // +link{dsRequest.operationType,operationType} "remove".
    // <p>
    // See +link{RestDataSource.dataURL} to configure a single URL for all requests, which is
    // required to support +link{RPCManager.startQueue()}.
    // @visibility external
    //<
    
    //> @attr RestDataSource.sendMetaData (Boolean : true : IR)
    // Should operation meta data be included when assembling parameters to send 
    // to the server? If true, meta data parameters will be prefixed with the 
    // +link{RestDataSource.metaDataPrefix}.<br>
    // Applies to operations where OperationBinding.dataProtocol is set to 
    // <code>"getParams"</code> or <code>"postParams"</code> only.
    // @visibility external
    //<
    sendMetaData:true,

    //> @attr RestDataSource.metaDataPrefix   (string : "_" :IR)
    // If +link{RestDataSource.sendMetaData} is true, this attribute is used to specify
    // the prefix to apply to 'meta data' properties when assembling parameters to send to the 
    // server.  Applies to operations where OperationBinding.dataProtocol is set to 
    // <code>"getParams"</code> or <code>"postParams"</code> only.
    // @visibility external
    //<
    metaDataPrefix:"_",
    
    //> @attr RestDataSource.sendClientContext (boolean : null : IRW)
    // If true the +link{dsRequest.clientContext} will be sent to the server as a parameter
    // along with the request.
    // @visibility internal
    //<
    // only has an effect for "postMessage" data protocol
    
    // sendClientContext:null,
    
    // getDataURL() 
    // overridden to respect fetchDataURL et al.
    getDataURL : function (dsRequest) { 
        var type = dsRequest.operationType;

        if (type == "fetch" && this.fetchDataURL != null) 
            return this.fetchDataURL;
        if (type == "update" && this.updateDataURL != null)
            return this.updateDataURL;
        if (type == "add" && this.addDataURL != null) 
            return this.addDataURL;
        if (type == "remove" && this.removeDataURL != null)
            return this.removeDataURL;
        return this.Super("getDataURL", arguments);
    },
    
    // Override getDataProtocol - treat postXML dataProtocol specification as postMessage.
    getDataProtocol : function (dsRequest) {
        var protocol = this.Super("getDataProtocol", arguments);
        if (protocol == "postXML") protocol = "postMessage";
        return protocol;
    },

    //> @method RestDataSource.transformRequest()
    // RestDataSource overrides transformRequest and handles serializing the request in the
    // appropriate format (determined by the specified
    // +link{operationBinding.dataProtocol,dataProtocol}), including the submitted
    // +link{DSRequest.data,data} as well as the meta data parameters, which may include -<br> 
    // +link{DSRequest.dataSource,dataSource},
    // +link{DSRequest.operationType,operationType}, +link{DSRequest.operationId,operationId};<br>
    // +link{DSRequest.startRow,startRow} and +link{DSRequest.endRow,endRow} (for fetches);<br>
    // +link{DSRequest.sortBy,sortBy} and +link{DSRequest.textMatchStyle,textMatchStyle} 
    // (for fetches);<br>
    // +link{DSRequest.oldValues,oldValues} (for update and remove operations);<br>
    // and possibly +link{DSRequest.componentId,componentId}.
    // <P>
    // If you override this method in order to add additional data to the DSRequest, you must
    // call +link{Class.Super,Super()} or you will remove the functionality provided by
    // RestDataSource.  For example:
    // <pre>
    //    transformRequest : function (dsRequest) {
    //        // modify dsRequest.data here, for example, add fixed criteria
    //        dsRequest.data.userId = myApplication.getCurrentUserId();
    //  
    //        return this.Super("transformRequest", arguments);
    //    }
    // </pre>
    // <P>
    // See +link{class:RestDataSource,RestDataSource overview} for a description of the 
    // standard formatting applied to requests.
    //
    // @visibility external
    //<
    transformRequest : function (dsRequest) {
        var protocol = this.getDataProtocol(dsRequest);

        dsRequest.isRestRequest = !(this.disableQueuing || this.clientOnly);

        // Lets override the dataformat for client only datasources otherwise use the DataSource preferred format.
        dsRequest.dataFormat = (this.clientOnly ? "json" : this.dataFormat);

        // "postMessage": Post data as XML serialized message
        if (protocol == "postMessage") {
        
            // Set parameter specifying request/response data format
            if (dsRequest.params == null) {
                dsRequest.params = {};
            }
            dsRequest.params[this.dataFormatParamName] = this.dataFormat;

            var params = {
                dataSource:this.getID()
            };

            // omit metadata fields if they're not set on the dsRequest
            if (dsRequest.operationType != null) params.operationType = dsRequest.operationType;
            if (dsRequest.operationId != null) params.operationId = dsRequest.operationId;
            if (dsRequest.startRow != null) params.startRow = dsRequest.startRow;
            if (dsRequest.endRow != null) params.endRow = dsRequest.endRow;
            if (dsRequest.sortBy != null) params.sortBy = dsRequest.sortBy;
            if (dsRequest.textMatchStyle != null) params.textMatchStyle = dsRequest.textMatchStyle;
            if (dsRequest.parentNode != null) params.parentNode = isc.Tree.getCleanNodeData(dsRequest.parentNode);
            if (dsRequest.useStrictJSON != null) params.useStrictJSON = dsRequest.useStrictJSON;

            if (this.sendClientContext) params.clientContext = dsRequest.clientContext;

            // send the componentId if present
            if (dsRequest.componentId) params.componentId = dsRequest.componentId;

            if (isc.DataSource.get("__criteriaSerializeDS") == null) {
                isc.DataSource.create({
                    ID: "__criteriaSerializeDS",
                    fields:[
                        {name:"_constructor", xmlAttribute:false},
                        {name:"criteria", multiple: true, type: "__criteriaSerializeDS", childTagName: "criterion"},
                        {name:"oldValues"}
                    ]
                });
            }

            var ds = isc.DataSource.create({
                fields:[
                    {name:"data", type: "__criteriaSerializeDS"},
                    {name:"oldValues"}
                ]
            });
            
            if (this.autoConvertRelativeDates == true) {
                // convert any relative dates in criteria into absolute dates so the server
                // doesn't need to know how to handle relative dates
                if (this.logIsInfoEnabled("relativeDates")) {
                    this.logInfo("Calling convertRelativeDates from getServiceInputs "+
                        "- data is\n\n"+isc.echoFull(dsRequest.data));
                }

                var transformedData = this.convertRelativeDates(dsRequest.data);

                if (this.logIsInfoEnabled("relativeDates")) {
                    this.logInfo("Called convertRelativeDates from getServiceInputs "+
                        "- data is\n\n"+isc.echoFull(transformedData));
                }
                dsRequest.data = transformedData;
            }

            // Map across the summary functions and group by data
            if (dsRequest.summaryFunctions != null) params.summaryFunctions = dsRequest.summaryFunctions;
            if (dsRequest.groupBy != null) params.groupBy = dsRequest.groupBy;

            params.data = dsRequest.data;
            params.oldValues = dsRequest.oldValues;

            if (!dsRequest.contentType) {
                dsRequest.contentType = (this.dataFormat == "json" ? 
                                         "application/json" : "text/xml");
            }
            
            var returnVal;
            
            dsRequest._unserializedData = dsRequest.data;

            if (dsRequest.dataFormat == "json") {
                if (params.data != null) params.data = this.serializeFields(params.data);
                if (params.oldValues != null) params.oldValues = this.serializeFields(params.oldValues);
                var settings = {
                    prettyPrint: this.prettyPrintJSON,
                    trimMilliseconds: !!this.trimMilliseconds
                };
                returnVal = isc.JSON.encode(params, settings);
            } else {
                var flags = {
                    // Switch off the special treatment of "_constructor" in xmlSerialize
                    ignoreConstructor: true,
                    // Provide this DS as schema to drive the serialization of individual 
                    // fields, so that we can use a different DS to drive the serialization
                    // as a whole (what tags get output for criteria, criterion, etc)
                    schema: this
                };
                returnVal = ds.xmlSerialize(params, flags, null, "request");
            }
            

            // Don't leak the temp-dataSource objects
            ds.destroy();
            return returnVal;

        // "getParams" / "postParams": HTTP Parameters format            
        } else {            
            
            if (protocol != "getParams" && protocol != "postParams") {
                this.logWarn("RestDataSource operation:"+ dsRequest.operationID + ", of type " 
                             + dsRequest.operationType + " has dataProtocol specified as '" 
                             + protocol + "'. Supported protocols are 'postParams', 'getParams' "
                             + "and 'postMessage' only. Defaulting to 'getParams'.");
                dsRequest.dataProtocol = 'getParams';  
            }

            // All fields passed in as 'data' will be available directly as parameters
            // Also include any explicit parameters present on the dsRequest
            var params = isc.addProperties({}, dsRequest.data, dsRequest.params);

            // Map across the summary functions and group by data
            if (dsRequest.summaryFunctions != null) params.summaryFunctions = dsRequest.summaryFunctions;
            if (dsRequest.groupBy != null) params.groupBy = dsRequest.groupBy;

            // Attach meta data parameters to the transaction
            if (this.sendMetaData) {
                if (!this.parameterNameMap) {
                    var map = {};
                    
                    map[this.metaDataPrefix + "operationType"] = "operationType";
                    map[this.metaDataPrefix + "operationId"] = "operationId";
                    map[this.metaDataPrefix + "startRow"] = "startRow";
                    map[this.metaDataPrefix + "endRow"] = "endRow";
                    map[this.metaDataPrefix + "sortBy"] = "sortBy";
                    map[this.metaDataPrefix + "useStrictJSON"] = "useStrictJSON";
                    map[this.metaDataPrefix + "textMatchStyle"] = "textMatchStyle";
                    map[this.metaDataPrefix + "oldValues"] = "oldValues";
                    map[this.metaDataPrefix + "componentId"] = "componentId";
                    map[this.metaDataPrefix + "parentNode"] = "parentNode";

                    this.parameterNameMap = map;
                }
                
                // Meta data will be available as parameters with the metaDataPrefix applied
                for (var parameterName in this.parameterNameMap) {
                    var value = dsRequest[this.parameterNameMap[parameterName]];
                    if (value != null) {
                        if (parameterName == "_parentNode") {
                            params[parameterName] = isc.Tree.getCleanNodeData(value);
                        } else {
                            params[parameterName] = value;
                        }
                    }
                }
                params[this.metaDataPrefix + "dataSource"] = this.getID();
                params["isc_metaDataPrefix"] = this.metaDataPrefix;
            }
            // Set parameter specifying response data format
            params[this.dataFormatParamName] = this.dataFormat;

            return params;
        }
    },
    
    // getUpdatedData() overridden to use request.originalData if dataProtocol is "postMessage"
    // as in this case request.data will be a serialized string of data rather than a javascript
    // object.
    getUpdatedData : function (dsRequest, dsResponse, useDataFromRequest) {
        var data = dsResponse ? dsResponse.data : null;
        if (useDataFromRequest && 
            (!data || isc.isAn.emptyString(data) || 
             (isc.isA.Array(data) && data.length == 0)) &&
            dsResponse.status == 0 &&
            this.getDataProtocol(dsRequest) == "postMessage") 
        {
            this.logInfo("dsResponse for successful operation of type " + dsRequest.operationType +
                         " did not return updated record[s]. Using submitted request data to update"
                          + " ResultSet cache.", "ResultSet");
            // look at the originalData object - stored before transformRequest was called
            var updateData = {},
                requestData = dsRequest.originalData;
            
            if (requestData && isc.isAn.Object(requestData)) {
                    
                // if operationType is an update, request data will be sparse so need to combine 
                // with oldValues
                if (dsRequest.operationType == "update") {
                    updateData = isc.addProperties({}, dsRequest.oldValues);
            
                    // Assertion - we only update one record at a time, so if submitted data is an array
                    // it will contain one entry that matches the 'oldValues'
                    if (isc.isAn.Array(requestData)) {
                        updateData = isc.addProperties(updateData, requestData[0]);  
                    } else {
                        updateData = isc.addProperties(updateData, requestData);
                    }
                    updateData = [updateData];
                // for add or delete old values are irrelevant
                } else {      
                    if (!isc.isAn.Array(requestData)) requestData = [requestData];
                    updateData = [];
                    for (var i = 0; i < requestData.length; i++) {
                        updateData[i] = isc.addProperties({}, requestData[i]);
                    }
                }
                //>DEBUG
                if (this.logIsDebugEnabled("ResultSet")) {
                    this.logDebug("Submitted data to be integrated into the cache:"
                                  + this.echoAll(updateData), "ResultSet");
                }
                //<DEBUG
            }
            return updateData;
            
        } else {
            return this.Super("getUpdatedData", arguments);
        }
        
    },
    
    // Helper method to verify the status returned by the server is valid
    getValidStatus : function (status) {
        if (isc.isA.String(status)) {
            if (parseInt(status) == status) status = parseInt(status);
            else {
                status = isc.DSResponse[status];
                if (status == null) {
                    this.logWarn("Unable to map response code: " + status
                                  + " to a DSResponse code, setting status to DSResponse.STATUS_FAILURE.");
                    status = isc.DSResponse.STATUS_FAILURE;
                }
            }
        }
        if (status == null) status = isc.DSResponse.STATUS_SUCCESS;
        return status;
    },
    
    // Helper method to verify the value for invalidateCache returned by the server is valid.
    // As invalidateCache is not mandatory field, set value to FALSE if not exists
    getValidInvalidateCache : function (ic) {
    	if (ic == null) return false;
	    switch (ic.toLowerCase()) {
    		case "true": case "yes": case "1": return true;
	    	case "false": case "no": case "0": case null: return false;
		    default: return Boolean(ic);
    	}
    },
    
    //> @method RestDataSource.transformResponse()
    // RestDataSource implements transformResponse in order to extract data and meta-data
    // properties from server responses, as described in the 
    // +link{class:RestDataSource,RestDataSource overview}.
    // <P>
    // You can override <code>transformResponse()</code> in order to further modify the
    // response, but if you do so, call +link{class.Super,Super()} as shown below or you will
    // wipe out the built-in response processing behavior of RestDataSource.
    // <pre>
    // transformResponse : function (dsResponse, dsRequest, data) {        
    //     var dsResponse = this.Super("transformResponse", arguments);
    //     // ... do something to dsResponse ...
    //     return dsResponse;
    // }
    // </pre> 
    // @param dsResponse (DSResponse) default DSResponse derived from the response data
    // @param dsRequest (DSRequest) DSRequest object that initiated this request
    // @param data (XMLDocument or JSON) XML document or JSON objects returned by the web
    //                                   service
    // @return (DSResponse) response derived 
    // 
    // @visibility external
    //<
    transformResponse : function (dsResponse, dsRequest, data) {
        if (dsResponse.status < 0 || !data) {
            // If any request failed, queueStatus must be -1
            dsResponse.queueStatus = -1;
            return dsResponse;
        }

        if (dsRequest.dataFormat == "json") {
            if (isc.isAn.Array(data)) {
                var useSingleEntry = data.length == 1 && data[0] && data[0].response != null;
                this.logWarn(
                    "RestDataSource transformResponse(): JSON response text is " +
                    "incorrectly formatted as an Array rather than a simple response " +
                    "object."
                    + (useSingleEntry ? 
                        " Array contains a single entry which appears to be a validly " +
                        "formatted response object - using this."
                        : "")
                );
                if (useSingleEntry) data = data[0];
            // If we're passed something with no {response:{...}} block log a warning, and
            // continue.
            } else if (data.response == null) {
                this.logWarn("RestDataSouce transformResponse(): JSON response text does " +
                "not appear to be in standard response format.");
            }
            var rawResponse = data.response || {};
        
            dsResponse.status = this.getValidStatus(rawResponse.status);
    	    // As invalidateCache is not mandatory field, set a default value of FALSE
	        dsResponse.invalidateCache = rawResponse.invalidateCache == null ? false : rawResponse.invalidateCache;
            dsResponse.queueStatus = this.getValidStatus(rawResponse.queueStatus);

            // if the status is a validation error, convert the errors from XML
            if (dsResponse.status == isc.DSResponse.STATUS_VALIDATION_ERROR) {
                var errors = rawResponse.errors;
                // Handle being returned an array of errors (per row) or a single error object
                // for the modified row.
                if (isc.isAn.Array(errors)) {
                    if (errors.length > 1) {
                        this.logWarn("server returned an array of errors - ignoring all but the first one");
                    }
                    errors = errors[0];   
                }
                dsResponse.errors = errors;
                // if there's also a general error message grab that too
                if (rawResponse.data != null) dsResponse.data = rawResponse.data;
            // handle being passed a failure status with 'data' being an error string to display
            } else if (dsResponse.status < 0) {

                dsResponse.data = rawResponse.data;
                
                
                dsResponse.totalRows = dsResponse.startRow;
                dsResponse.endRow = dsResponse.startRow;
            }
            if (rawResponse.totalRows != null) dsResponse.totalRows = rawResponse.totalRows;
            if (rawResponse.startRow != null) dsResponse.startRow = rawResponse.startRow;
            if (rawResponse.endRow != null) dsResponse.endRow = rawResponse.endRow;
            
        } else {
            if (dsRequest.clientOnly) return dsResponse;
            dsResponse.status = this.getValidStatus(data.selectString("//status"));
    	    dsResponse.invalidateCache = this.getValidInvalidateCache(data.selectString("//invalidateCache"));
            dsResponse.queueStatus = this.getValidStatus(data.selectString("//queueStatus"));
            // if the status is a validation error, convert the errors from XML
            if (dsResponse.status == isc.DSResponse.STATUS_VALIDATION_ERROR) {
                var errors = data.selectNodes("//errors");
                errors = isc.xml.toJS(errors);
                if (errors.length > 1) {
                    this.logWarn("server returned an array of errors - ignoring all but the first one");
                }
                errors = errors[0];   
                dsResponse.errors = errors;
                // if there's also a general error message grab that too
                var errorMessage = data.selectString("//data");
                if (errorMessage) dsResponse.data = errorMessage;
            // handle being passed a raw response where 'data' is an error string to display                            
            } else if (dsResponse.status < 0) {
                dsResponse.data = data.selectString("//data");
            }
              
            var totalRows = data.selectNumber("//totalRows");
            if (totalRows != null) dsResponse.totalRows = totalRows;
            
            var startRow = data.selectNumber("//startRow");
            if (startRow != null) dsResponse.startRow = startRow;
            
            var endRow = data.selectNumber("//endRow");
            if (endRow != null) dsResponse.endRow = endRow;
        }
        return dsResponse;
    },
    
    shouldUseTestDataFetch : function () {
        return this.clientOnly == true && 
               this.cacheAllData != true && 
               this.testFileName != null;
    },

    hasTestData : function () {
        return this.testFileName != null;
    },
    
    getClientOnlyResponse : function (request, serverData) {
        if (request._unserializedData) request.data = request._unserializedData;
        var resp = this.Super("getClientOnlyResponse", arguments);
        if (!isc.isAn.Array(resp.data)) resp.data = [resp.data];
        return resp;
    }


});
