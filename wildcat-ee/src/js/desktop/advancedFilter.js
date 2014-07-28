isc.defineClass("AdvancedFilter", "VLayout");
isc.AdvancedFilter.addProperties({
	// VLayout properties
	width: "100%",
	height: 1,
	overflow: "visible",
	membersMargin: 2,
	margin:2,

	// The BizListGrid, BizTreeGrid or whatever that can show/hide a filter editor
	// and do a fetch with the filter builder's criteria
	filterableComponent: null,

	// The BizListGrid/BizTreeGrid config
	filterableComponentConfig: null,
	
	toggleButton: null,
	_filterBuilder: null,
	_filterButtonDefaults: null
});
isc.AdvancedFilter.addMethods({
	initWidget : function() {
		this._filterButtonDefaults = {
    		_constructor: isc.IButton,
    		autoFit: true,
    		title: "Filter",
    		icon: "../images/icons/filter_add.png"
    	};

		this.Super("initWidget", arguments);
    	
    	this.toggleButton = isc.ImgButton.create({
    	    size: 20,
    	    src: "../images/icons/filterButton.png",
    	    showRollOver: false,
    	    showFocused: false,
    	    actionType: "checkbox",
    	    selected: false,
    	    canHover: true,
    		getHoverHTML: function() {
    			if (this.selected) {
    				return "Use <b>simple</b> filtering";
    			}
    			else {
    				return "Use <b>advanced</b> filtering";
    			}
    		}
    	});
    	
    	var me = this;
    	this._styleForm = isc.DynamicForm.create({
    		numCols: 2,
    		width: 200,
    		items: [
    			{name: 'style',
    				title: 'Style',
    				type: 'radioGroup',
    				vertical: false,
    				required: true,
    				valueMap: {radio: 'Flat', bracket: 'Nested', inline: 'Inline'},
    				defaultValue: 'radio',
    				changed: function(form, item, value) {
						me.setDataSource(me._filterBuilder.getDataSource());
    				}
    			}
    		]
    	});
    	this.addMember(this._styleForm);
    	
    	this.getStyle = function() {
    		return this._styleForm.getValue('style');
    	};
    	
    	this.setStyle = function(style) {
    		this._styleForm.setValue('style', style);
			this.setDataSource(this._filterBuilder.getDataSource());
    	};
	},
	
    // Assign this to the toggle button click event in the client class
	toggleButtonClick: function() {
		if (this.toggleButton.selected) {
			// copy simple criteria to the advanced criteria object
			// TODO DOESNT WORK - WHY NOT??
//alert(this.filterableComponent.grid.getDataSource().convertCriteria); // DNE
//			var newCriteria = this.filterableComponent.grid.getDataSource().convertCriteria(this.filterableComponent.grid.getCriteria());
//			this._filterBuilder.setCriteria(newCriteria);

			this.filterableComponent.grid.setShowFilterEditor(false);
			// ensure we show and hide the whole panel 
			// otherwise mouse gestures don't work on well on the list filter editor in IE7
			this.show();
		}
		else {
			this.filterableComponent.grid.setShowFilterEditor(true);
			var params = {_summary: this.filterableComponent.summaryType};
			if (this.filterableComponentConfig && this.filterableComponentConfig.params) {
				var instance = this.filterableComponent._view.gather(false); // no validate
				for (var binding in this.filterableComponentConfig.params) {
					var expression = this.filterableComponentConfig.params[binding];
					var value = this.filterableComponent._view.toDisplay(expression, instance);
					params[binding] = value;
				}
			}
			
			this.filterableComponent.grid.clearCriteria(null, {params: params});
			// ensure we show and hide the whole panel 
			// otherwise mouse gestures don't work on well on the list filter editor in IE7
			this.hide();
		}
	},

	setDataSource: function(dataSource) { // the data source object
		if (this._filterBuilder) {
			this.removeMember(this._filterBuilder);
			this._filterBuilder.destroy();
			this._filterBuilder = null;
		}
		else { // initting
			this._filterButton = this.createAutoChild("_filterButton", {
				click: function() {
					this.creator.filterableComponent.grid.filterData(this.creator._filterBuilder.getCriteria(), 
																		null, 
																		{params: {_summary: this.creator.filterableComponent.summaryType}});
				}
			});
			this.addMember(this._filterButton);
		}
		var style = this._styleForm.getValue('style');
		if (style) {} else {
			style = 'radio';
		}

		this._filterBuilder = isc.FilterBuilder.create({
			dataSource: dataSource,
			topOperatorAppearance: style,
			allowEmpty: true
		});

		this.addMember(this._filterBuilder, 1);
		if (this.toggleButton.selected) {
			// ensure we show and hide the whole panel 
			// otherwise mouse gestures don't work on well on the list filter editor in IE7
			this.show();
		}
		else {
			// ensure we show and hide the whole panel 
			// otherwise mouse gestures don't work on well on the list filter editor in IE7
			this.hide();
		}
	},
	
	getCriteria: function() {
		if (this._filterBuilder) {
			return this._filterBuilder.getCriteria();
		}
		else {
			return null;
		}
	},
	
	clearCriteria: function() {
		if (this._filterBuilder) {
			this._filterBuilder.clearCriteria();
		}
	},
	
	setCriteria: function(criteria) {
		if (this._filterBuilder) {
			this._filterBuilder.setCriteria(criteria);
		}
	}
});
