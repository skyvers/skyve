(function(){    delete isc.definingFramework;    if(isc.$1342){        var _1=isc.$1342;        if(isc.Page){            isc.$1343=isc["_"+_1+"$1344"]=(isc.timestamp?isc.timestamp():new Date().getTime());            isc.Page.handleEvent(null,"moduleLoaded",{                moduleName:_1,                loadTime:(isc.$1343-isc.$141q)
            })}
        isc.$1345=_1;        delete isc.$1342}})();
