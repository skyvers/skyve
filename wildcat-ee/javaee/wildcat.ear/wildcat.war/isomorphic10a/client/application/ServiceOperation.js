/*
 * Isomorphic SmartClient
 * Version v10.0p_2015-01-04 (2015-01-04)
 * Copyright(c) 1998 and beyond Isomorphic Software, Inc. All rights reserved.
 * "SmartClient" is a trademark of Isomorphic Software, Inc.
 *
 * licensing@smartclient.com
 *
 * http://smartclient.com/license
 */



isc.defineClass("ServiceOperation").addClassMethods({
    getServiceOperation : function (operationName, serviceName, serviceNamespace) {
        if (this._instances) return this._instances.find({
            operationName: operationName,
            serviceName: serviceName,
            serviceNamespace: serviceNamespace
        });
    }
});

isc.ServiceOperation.addMethods({
    init : function () {
	    // get a global ID so we can be called in the global scope
    	isc.ClassFactory.addGlobalID(this);
        this.Super("init", arguments);
        if (!isc.ServiceOperation._instances) isc.ServiceOperation._instances = [];
        isc.ServiceOperation._instances.add(this);
    },
    
    invoke : function () {
        var service = this.service = isc.WebService.getByName(this.serviceName, this.serviceNamespace);
        if (!service) {
            this.logWarn("Unable to find web service with serviceName '" + this.serviceName +
                         "' and serviceNamespace '" + this.serviceNamespace + "'. Has it been " +
                         "loaded?");
            return;
        }
        
        var data = this.inputVM.getValues();
        if (service.useSimplifiedInputs(this.operationName)) {
            data = data[isc.firstKey(data)];
        }
   
        var _this = this;
        service.callOperation(this.operationName, data, null, 
            function (data, xmlDoc, rpcResponse, wsRequest) {
                _this.invocationCallback(data, xmlDoc, rpcResponse, wsRequest);
            }
        );
    },
        
    invocationCallback : function (data, xmlDoc, rpcResponse, wsRequest) {
        if (!this.outputVM) return;

        // with document style SOAP there is no element representing the message as such - the
        // message parts appear as elements directly.  If there's only a single part there's no
        // representation in the returned data of that single part name (which is a good
        // simplification).  However the dataPaths are specified message-relative and so the
        // partName needs to be explicitly re-introduced
        if (this.service.getSoapStyle(this.operationName) == "document") {
            var fieldNames = this.outputVM.getDataSource().getFieldNames();
            if (fieldNames.length == 1) {
                var firstFieldName = fieldNames.first(),
                    fullData = {};
                fullData[firstFieldName] = data;
                data = fullData;
            }
        }

        this.outputVM.setValues(data);
 
        if (this.logIsInfoEnabled()) {
            this.logInfo("populating listeners on dataView: " + this.dataView + 
                         ", vm has values: " + this.echo(this.outputVM.getValues()));
        }

        // populate inputDataPath listeners via the DataView       
        if (this.dataView) this.dataView.populateListeners(this.outputVM);
    }
        
});

