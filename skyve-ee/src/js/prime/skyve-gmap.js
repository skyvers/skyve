SKYVE.BizMap = function() {
	var displays = {};

	var refresh = function(fit) {
		var wkt = new Wkt.Wkt();
		var url = SKYVE.Util.CONTEXT_URL + 'map?';

	};
	
	// public
	return {
		create: function(options) {
			var mapOptions = {
				zoom: 4,
				center: new google.maps.LatLng(-26,133.5),
				mapTypeId: google.maps.MapTypeId.ROADMAP
			};

			var display = displays[options.elementId];
			if (display) {
				if (display.map) {
					mapOptions.zoom = display.map.getZoom();
					mapOptions.center = display.map.getCenter();
					mapOptions.mapTypeId = display.map.getMapTypeId();
				}
			}
			else {
				display = {};
				displays[options.elementId] = display;
			}
			display.infoWindow = new google.maps.InfoWindow({content: ''});

	/* TODO reinstate
				var control = document.createElement('DIV');
				control.id = this.ID + '_form';
				control.style.width = '300px';
	*/
			display.map = new google.maps.Map(document.getElementById(options.elementId), mapOptions);
	/* TODO reinstate
				this._map.controls[google.maps.ControlPosition.TOP].push(control);
	*/

			var url = SKYVE.Util.CONTEXT_URL + 'map?';
			if (options.modelName) {
				url += '_c=' + options._c + '&_m=' + options.modelName;
			}
			else if (options.queryName) {
				url += '_mod=' + options.moduleName + '&_q=' + options.queryName + '&_geo=' + options.geometryBinding;
			}

			refresh(true);
//				this.delayCall('_addForm', null, 1000);
			return display;
		},
		
		get: function(elementId) {
			return displays[elementId];
		}
	}
}();
