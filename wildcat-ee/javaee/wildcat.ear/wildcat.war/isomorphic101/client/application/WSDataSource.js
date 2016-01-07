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
isc.defineClass("WSDataSource", "DataSource");

//> @class WSDataSource
// A WSDataSource is a DataSource that is preconfigured to contact the WSDL web service built
// into the SDK (see isomorphic/system/schema/SmartClientOperations.wsdl).  This WSDL service
// can be easily implemented on Java and non-Java backends.  
// <P>
// WSDataSource supports all 4 DataSource operations (fetch, add, update, remove) and can be
// used with ListGrids, DynamicForms and other +link{DataBoundComponent}s just like other
// DataSources.
// <P>
// Note that WSDataSource is specifically designed for use with SmartClientOperations.wsdl.  If
// you are trying to connect to a pre-existing WSDL service, start with just +link{DataSource}, 
// not WSDataSource, and see the +link{group:wsdlBinding,WSDL Integration} chapter for an
// overview.
//
// @treeLocation Client Reference/Data Binding
// @visibility xmlBinding
//<

isc.WSDataSource.addMethods({
    serviceNamespace : "urn:operations.smartclient.com",
    operationBindings:[
       {operationType:"fetch", wsOperation:"fetch", recordXPath:"//data/*" },
       {operationType:"add", wsOperation:"add", recordXPath:"//data/*" },
       {operationType:"remove", wsOperation:"remove", recordXPath:"//data/*" },
       {operationType:"update", wsOperation:"update" , recordXPath:"//data/*" }
    ],
    transformRequest : function (dsRequest) {
        var data = {
            dataSource : dsRequest.dataSource,
            operationType : dsRequest.operationType,
            data : dsRequest.data
        };
        // send various metadata only if set
        if (dsRequest.startRow != null) {
            data.startRow = dsRequest.startRow;
            data.endRow = dsRequest.endRow;
        }
        if (dsRequest.textMatchStyle != null) data.textMatchStyle = dsRequest.textMatchStyle;
        if (dsRequest.operationId != null) data.operationId = dsRequest.operationId;
        if (dsRequest.sortBy != null) data.sortBy = dsRequest.sortBy;
        return data;
    },
    transformResponse : function (dsResponse, dsRequest, xmlData) {
        
        // Bail out early if xmlData is null or an unexpected type
        if (!xmlData || !xmlData.selectString) return;
        
        dsResponse.status = xmlData.selectString("//status");

        // convert status from a String to a numeric code
        if (isc.isA.String(dsResponse.status)) {
            var status = isc.DSResponse[dsResponse.status];
            if (dsResponse.status == null) {
                this.logWarn("Unable to map response code: " + status
                              + " to a DSResponse code, setting status to DSResponse.STATUS_FAILURE.");
                status = isc.DSResponse.STATUS_FAILURE;
                dsResponse.data = xmlData.selectString("//data");
            } else {
                dsResponse.status = status;
            }
        }

        // if the status is a validation error, conver the errors from XML
        if (dsResponse.status == isc.DSResponse.STATUS_VALIDATION_ERROR) {
            var errors = xmlData.selectNodes("//errors/*");
            dsResponse.errors = isc.xml.toJS(errors, null, this);
        }

        dsResponse.totalRows = xmlData.selectNumber("//totalRows");
        dsResponse.startRow = xmlData.selectNumber("//startRow");
        dsResponse.endRow = xmlData.selectNumber("//endRow");

    }
});


