isc.defineClass("ListView");
ListView.addClassProperties({
	// the heading HTML at the top of the list
	// the template for the list view headers - has {title} and {link} in it
	// this comes from the server renderer
	_heading: isc.HTMLFlow.create({showEdges:true}),

	// the contents (layout) of the entire list
	contents: isc.VLayout.create({
		width: "100%",
		height: "100%",
		overflow: "auto",
		membersMargin: 2,
		layoutMargin: 2,
		margin: 2,
		// rerender the opening view
		rerender: function() {
			if (ListView._grid && ListView._grid.isVisible()) {
				ListView._grid.refresh();
			}
			else if (ListView._calendar && ListView._calendar.isVisible()) {
				ListView._calendar.refresh();
			}
			else if (ListView._tree && ListView._tree.isVisible()) {
				ListView._tree.refresh();
			}
			else if (ListView._map && ListView._map.isVisible()) {
				ListView._map.rerender();
			}
		},
		// resume auto refresh
		resume: function() {
			if (ListView._grid && ListView._grid.isVisible()) {
//				ListView._grid.resume();
			}
			else if (ListView._calendar && ListView._calendar.isVisible()) {
//				ListView._calendar.resume();
			}
			else if (ListView._tree && ListView._tree.isVisible()) {
//				ListView._tree.resume();
			}
			else if (ListView._map && ListView._map.isVisible()) {
				ListView._map.resume();
			}
		}
	}),

	_grid: null,
	_calendar: null,
	_tree: null,
	_map: null,
	_portal: isc.PortalLayout.create({
		width: '100%',
		height: '100%',
	    getDropPortlet : function (dragTarget, colNum, rowNum, rowOffset) {
	        // You can use getDropPortlet to customise what happens when a component is dropped
	        if (dragTarget.isA('TreeGrid') && dragTarget.getID().endsWith('Tree')) { // this is a menu item
	        	var leaf = dragTarget.getDragData()[0];

	        	var portlet = isc.Portlet.create({title: leaf.desc, items: []});

	        	// derive the component required
	        	if (leaf.ref == 'edit') {
					BizUtil.getEditView(dragTarget.data.root.name, // module Name
											leaf.name, // document name
											function(view) { // the view
												portlet.addItem(view);
//												BizUtil._currentView = view;
												view.newInstance(null, null, null, null, function() {view.hideMember(view._heading);});
											});
	        	}
	        	else {
		        	if (leaf.ref == 'grid') {
		        		var grid = BizUtil.createListGrid();
		        		portlet.addItem(grid);
		        		grid.setDataSource(dragTarget.data.root.name + "_" + leaf.name);
					}
					else if (leaf.ref == 'cal') {
		        		var calendar = BizUtil.createCalendar();
		        		portlet.addItem(calendar);
		        		calendar.setDataSource(dragTarget.data.root.name + "_" + leaf.name);
					}
					else if (leaf.ref == 'tree') {
		        		var tree = BizUtil.createTreeGrid();
		        		portlet.addItem(tree);
		        		tree.setDataSource(dragTarget.data.root.name + "_" + leaf.name);
					}
					else if (leaf.ref == 'map') {
		        		var map = BizUtil.createMap();
		        		portlet.addItem(map);
		        		map.setDataSource(dragTarget.data.root.name + "_" + leaf.name);
					}
					else {
						alert('Menu ref of ' + leaf.ref + 'is unknown');
					}
	        	}
		        
	        	return portlet;
	        }
        	else {
	            // By default, the whole component is wrapped in a Portlet
	            return this.Super("getDropPortlet", arguments);
	        }
	    }
	}),
	
	_setHeading: function(title, icon) {
		var header = BizUtil.headerTemplate;
		header = header.replace('{icon}', icon).replace('{title}', title).replace('{link}', '');
		ListView._heading.setContents(header);
	},
	
	// set the data source for the list view grid
	setGridDataSource: function(ID) { // the ID of the data source
		if (ListView._grid) {} else {
			ListView._grid = BizUtil.createListGrid();
			ListView.contents.addMember(ListView._grid);
		}
		if (ListView._calendar) {
			ListView.contents.hideMember(ListView._calendar);
		}
		if (ListView._tree) {
			ListView.contents.hideMember(ListView._tree);
		}
		if (ListView._map) {
			ListView.contents.hideMember(ListView._map);
		}
		ListView.contents.hideMember(ListView._portal);
		ListView.contents.showMember(ListView._grid);
		ListView._setHeading(ListView._grid.setDataSource(ID), eval(ID + '.icon'));
	},
	
	// set the data source for the list view calendar
	setCalendarDataSource: function(ID) { // the ID of the data source
		if (ListView._calendar) {} else {
			ListView._calendar = BizUtil.createCalendar();
			ListView.contents.addMember(ListView._calendar);
		}
		if (ListView._grid) {
			ListView.contents.hideMember(ListView._grid);
		}
		if (ListView._tree) {
			ListView.contents.hideMember(ListView._tree);
		}
		if (ListView._map) {
			ListView.contents.hideMember(ListView._map);
		}
		ListView.contents.hideMember(ListView._portal);
		ListView.contents.showMember(ListView._calendar);

		ListView._calendar.setDataSource(ID);
		ListView._setHeading("NOT IMPLEMENTED", eval(ID + '.icon'));
	},
	
	// set the data source for the list view tree
	setTreeDataSource: function(ID) { // the ID of the data source
		if (ListView._tree) {} else {
			ListView._tree = BizUtil.createTreeGrid();
			ListView.contents.addMember(ListView._tree);
		}
		if (ListView._grid) {
			ListView.contents.hideMember(ListView._grid);
		}
		if (ListView._calendar) {
			ListView.contents.hideMember(ListView._calendar);			
		}
		if (ListView._map) {
			ListView.contents.hideMember(ListView._map);
		}
		ListView.contents.hideMember(ListView._portal);
		ListView.contents.showMember(ListView._tree);

		ListView._tree.setDataSource(ID);
		ListView._setHeading("NOT IMPLEMENTED", eval(ID + '.icon'));
	},

	// set the data source for the list view map
	setMapDataSource: function(ID) { // the ID of the data source
		if (ListView._map) {} else {
			ListView._map = BizUtil.createMap();
			ListView.contents.addMember(ListView._map);
		}
		if (ListView._grid) {
			ListView.contents.hideMember(ListView._grid);
		}
		if (ListView._calendar) {
			ListView.contents.hideMember(ListView._calendar);			
		}
		if (ListView._tree) {
			ListView.contents.hideMember(ListView._tree);
		}
		ListView.contents.hideMember(ListView._portal);
		ListView.contents.showMember(ListView._map);

		ListView._map.setDataSource(ID);
		ListView._setHeading("MAP", eval(ID + '.icon'));
	},
	
	showPortal: function() {
		if (ListView._grid) {
			ListView.contents.hideMember(ListView._grid);
		}
		if (ListView._calendar) {
			ListView.contents.hideMember(ListView._calendar);			
		}
		if (ListView._tree) {
			ListView.contents.hideMember(ListView._tree);
		}
		if (ListView._map) {
			ListView.contents.hideMember(ListView._map);
		}
		ListView.contents.showMember(ListView._portal);

		ListView._setHeading("DASHBOARD", "shared/icons/Home.png");
	}
});
ListView.contents.addMember(ListView._heading);
ListView.contents.addMember(ListView._portal);
ListView.contents.hideMember(ListView._portal);
