(function(){    delete isc.definingFramework;    if(isc._currentModule){        var _1=isc._currentModule;        if(isc.Page){            isc._moduleEnd=isc["_"+_1+"_end"]=(isc.timestamp?isc.timestamp():new Date().getTime());            isc.Page.handleEvent(null,"moduleLoaded",{                moduleName:_1,                loadTime:(isc._moduleEnd-isc._moduleStart)
            })}
        isc._lastModule=_1;        delete isc._currentModule}})();
