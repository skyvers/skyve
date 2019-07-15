// SKYVE name space definition
SKYVE = {};

SKYVE.Util = function() {
	// public methods
	return {
		customer: null,
		version: null,
		googleMapsV3ApiKey: null,
		ckEditorConfigFileUrl: null,
		
		loadJS: function(scriptPath, callback) {
		    var scriptNode = document.createElement('SCRIPT');
		    scriptNode.type = 'text/javascript';
		    scriptNode.src = scriptPath;

		    if (callback != null) {
			    if (scriptNode.readyState) { // IE, incl. IE9
			    	scriptNode.onreadystatechange = function() {
			    		if (scriptNode.readyState == "loaded" || scriptNode.readyState == "complete") {
			    			scriptNode.onreadystatechange = null;
			    			callback();
			    		}
			    	};
			    } 
			    else { // Other browsers
			    	scriptNode.onload = callback;
			    }
		    }
		    
		    var headNode = document.getElementsByTagName('HEAD');
		    if (headNode[0] != null) {
		        headNode[0].appendChild(scriptNode);
		    }
		}
	}
}();
