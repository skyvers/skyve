isc.defineClass("ListView");
isc.ListView.addClassProperties({
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
			if (isc.ListView._grid && isc.ListView._grid.isVisible()) {
				isc.ListView._grid.refresh();
			}
			else if (isc.ListView._calendar && isc.ListView._calendar.isVisible()) {
				isc.ListView._calendar.refresh();
			}
			else if (isc.ListView._tree && isc.ListView._tree.isVisible()) {
				isc.ListView._tree.refresh();
			}
			else if (isc.ListView._map && isc.ListView._map.isVisible()) {
				isc.ListView._map.rerender();
			}
		},
		// resume auto refresh
		resume: function() {
			if (isc.ListView._grid && isc.ListView._grid.isVisible()) {
//				isc.ListView._grid.resume();
			}
			else if (isc.ListView._calendar && isc.ListView._calendar.isVisible()) {
//				isc.ListView._calendar.resume();
			}
			else if (isc.ListView._tree && isc.ListView._tree.isVisible()) {
//				isc.ListView._tree.resume();
			}
			else if (isc.ListView._map && isc.ListView._map.isVisible()) {
				isc.ListView._map.resume();
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
					isc.BizUtil.getEditView(dragTarget.data.root.name, // module Name
												leaf.name, // document name
												function(view) { // the view
													portlet.addItem(view);
//													isc.BizUtil._currentView = view;
													view.newInstance(null, null, null, null, function() {view.hideMember(view._heading);});
												});
	        	}
	        	else {
		        	if (leaf.ref == 'grid') {
		        		var grid = isc.BizUtil.createListGrid();
		        		portlet.addItem(grid);
		        		grid.setDataSource(dragTarget.data.root.name + "_" + leaf.name, leaf.config);
					}
					else if (leaf.ref == 'cal') {
		        		var calendar = isc.BizUtil.createCalendar();
		        		portlet.addItem(calendar);
		        		calendar.setDataSource(dragTarget.data.root.name + "_" + leaf.name);
					}
					else if (leaf.ref == 'tree') {
		        		var tree = isc.BizUtil.createTreeGrid();
		        		portlet.addItem(tree);
		        		tree.setDataSource(dragTarget.data.root.name + "_" + leaf.name, leaf.config);
					}
					else if (leaf.ref == 'map') {
		        		var map = isc.BizUtil.createMap();
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
	
	_setHeading: function(title, icon, fontIcon, modoc) {
		var iconMarkup = '';
		if (icon) {
			iconMarkup = '<img style="width:32px;height:32px" src="resources?_doc=' + modoc + '&_n=' + icon + '&v=' + SKYVE.Util.v + '"/>';
		}
		else if (fontIcon) {
			iconMarkup = '<i style="padding-left:5px;font-size:28px;width:32px !important" class="titleBar bizhubFontIcon ' + fontIcon + '"></i>';
		}
		var header = isc.BizUtil.headerTemplate;
		header = header.replace('{icon}', iconMarkup).replace('{title}', title).replace('{link}', '');
		isc.ListView._heading.setContents(header);
	},
	
	// set the data source for the list view grid
	setGridDataSource: function(ID,  // the ID of the data source
								menuConfig) { // config from the menu item (optional parameter)
		if (isc.ListView._grid) {} else {
			isc.ListView._grid = isc.BizUtil.createListGrid();
			isc.ListView.contents.addMember(isc.ListView._grid);
		}
		if (isc.ListView._calendar) {
			isc.ListView.contents.hideMember(isc.ListView._calendar);
		}
		if (isc.ListView._tree) {
			isc.ListView.contents.hideMember(isc.ListView._tree);
		}
		if (isc.ListView._map) {
			isc.ListView.contents.hideMember(isc.ListView._map);
		}
		isc.ListView.contents.hideMember(isc.ListView._portal);
		isc.ListView.contents.showMember(isc.ListView._grid);
		var ds = eval(ID);
		var title = isc.ListView._grid.setDataSource(ds, menuConfig);
		isc.ListView._setHeading(title, ds.icon, ds.fontIcon, ds.modoc);
	},
	
	// set the data source for the list view calendar
	setCalendarDataSource: function(ID, // the ID of the data source
									menuConfig) { // config from the menu item (optional parameter)
		if (isc.ListView._calendar) {} else {
			isc.ListView._calendar = isc.BizUtil.createCalendar();
			isc.ListView.contents.addMember(isc.ListView._calendar);
		}
		if (isc.ListView._grid) {
			isc.ListView.contents.hideMember(isc.ListView._grid);
		}
		if (isc.ListView._tree) {
			isc.ListView.contents.hideMember(isc.ListView._tree);
		}
		if (isc.ListView._map) {
			isc.ListView.contents.hideMember(isc.ListView._map);
		}
		isc.ListView.contents.hideMember(isc.ListView._portal);
		isc.ListView.contents.showMember(isc.ListView._calendar);

		var ds = eval(ID);
		isc.ListView._calendar.setDataSource(ds);
		isc.ListView._setHeading("NOT IMPLEMENTED", ds.icon, ds.fontIcon, ds.modoc);
	},
	
	// set the data source for the list view tree
	setTreeDataSource: function(ID, // the ID of the data source
								menuConfig) { // config from the menu item (optional parameter)
		if (isc.ListView._tree) {} else {
			isc.ListView._tree = isc.BizUtil.createTreeGrid();
			isc.ListView.contents.addMember(isc.ListView._tree);
		}
		if (isc.ListView._grid) {
			isc.ListView.contents.hideMember(isc.ListView._grid);
		}
		if (isc.ListView._calendar) {
			isc.ListView.contents.hideMember(isc.ListView._calendar);			
		}
		if (isc.ListView._map) {
			isc.ListView.contents.hideMember(isc.ListView._map);
		}
		isc.ListView.contents.hideMember(isc.ListView._portal);
		isc.ListView.contents.showMember(isc.ListView._tree);

		var ds = eval(ID);
		var title = isc.ListView._tree.setDataSource(ds, menuConfig);
		isc.ListView._setHeading(title, ds.icon, ds.fontIcon, ds.modoc);
	},

	// set the data source for the list view map
	setMapDataSource: function(ID, // the ID of the data source
								menuConfig) { // config from the menu item (optional parameter)
		if (isc.ListView._map) {} else {
			isc.ListView._map = isc.BizUtil.createMap();
			isc.ListView.contents.addMember(isc.ListView._map);
		}
		if (isc.ListView._grid) {
			isc.ListView.contents.hideMember(isc.ListView._grid);
		}
		if (isc.ListView._calendar) {
			isc.ListView.contents.hideMember(isc.ListView._calendar);			
		}
		if (isc.ListView._tree) {
			isc.ListView.contents.hideMember(isc.ListView._tree);
		}
		isc.ListView.contents.hideMember(isc.ListView._portal);
		isc.ListView.contents.showMember(isc.ListView._map);

//		var ds = eval(ID);
//		isc.ListView._map.setDataSource(ds);
//		isc.ListView._setHeading("MAP", ds.icon, ds.fontIcon, ds.modoc);
		isc.ListView._map.setDataSource(ID);
		isc.ListView._setHeading('MAP', 'shared/icons/Home.png', 'fa fa-globe fa-2x', '');
	},
	
	showPortal: function() {
		if (isc.ListView._grid) {
			isc.ListView.contents.hideMember(isc.ListView._grid);
		}
		if (isc.ListView._calendar) {
			isc.ListView.contents.hideMember(isc.ListView._calendar);			
		}
		if (isc.ListView._tree) {
			isc.ListView.contents.hideMember(isc.ListView._tree);
		}
		if (isc.ListView._map) {
			isc.ListView.contents.hideMember(isc.ListView._map);
		}
		isc.ListView.contents.showMember(isc.ListView._portal);

		isc.ListView._setHeading("DASHBOARD", "shared/icons/Home.png", 'fa fa-home fa-2x', '');
	}
});
isc.ListView.contents.addMember(isc.ListView._heading);
isc.ListView.contents.addMember(isc.ListView._portal);
isc.ListView.contents.hideMember(isc.ListView._portal);
