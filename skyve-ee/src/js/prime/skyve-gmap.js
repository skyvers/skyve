SKYVE.BizMap = function() {
	maps = {};

	// public
	return {
		initialise: function(elementId) {
			var mapOptions = {
				zoom: 4,
				center: new google.maps.LatLng(-26,133.5),
				mapTypeId: google.maps.MapTypeId.ROADMAP
			};

				if (this._map) {
					mapOptions.zoom = this._map.getZoom();
					mapOptions.center = this._map.getCenter();
					mapOptions.mapTypeId = this._map.getMapTypeId();
				}

				this._infoWindow = new google.maps.InfoWindow({content: ''});

	/* TODO reinstate
				var control = document.createElement('DIV');
				control.id = this.ID + '_form';
				control.style.width = '300px';
	*/
				this._map = new google.maps.Map(document.getElementById(elementId), mapOptions);
	/* TODO reinstate
				this._map.controls[google.maps.ControlPosition.TOP].push(control);
	*/
//				this._refresh(true, false);
//				this.delayCall('_addForm', null, 1000);
		}
	}
}();
