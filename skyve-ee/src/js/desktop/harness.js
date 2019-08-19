isc.BizUtil.addClassProperties({
	headerTemplate: null,
	init: function(headerTemplate, // template HTML for view headers
					logoSrc, // src for logo image - most cases this is a url to the CustomerResourceServlet
					menuConfig, // a bunch of menu configurations
					dataSourceConfig) { // a bunch of datasource configurations
		isc.BizUtil.headerTemplate = headerTemplate;

		// this is the body HLayout - Menu on the left, Details on the right
		isc.HLayout.create({
			ID: "body", 
			autoDraw: true,
			width: "100%",
			height: "100%",
			// TODO should this be hard-coded??
			backgroundColor: "whitesmoke",
			padding: 10,
			members: [
				// LHS Menu Navigation panel
				isc.VLayout.create({
					ID: "navigation",
					// should this be hard coded and should it be a percentage anyway
					width: "30%",
					showResizeBar: true,
					backgroundColor: "white",
					border: "1px solid #c0c0c0",
					showShadow: true,
					shadowSoftness: 10,
					shadowOffset: 0,
					members: [
						// Logo image
						isc.Img.create({
						    imageType: "center",
						    src: logoSrc,
						    overflow: 'clip-h'
						}),
						// accordion pane of menus
						isc.SectionStack.create({
							ID: "adminStack",
							width: "100%",
							visibilityMode: "mutex",
							animateSections: false,
							overflow: "hidden",
							headerHeight: 30
						})
					]
				}),
				// RHS details panel
				isc.VLayout.create({
					ID: "details",
					width: "100%",
					height: "100%",
					backgroundColor: "whitesmoke",
					border: "1px solid #c0c0c0",
					showShadow: true,
					shadowSoftness: 10,
					shadowOffset: 0
				})
			]
		});
		// for each module in the menuConfig, make a menu treegrid and house it in a section
		for (var i = 0, l = menuConfig.length; i < l; i++) {
			var item = menuConfig[i];
			var menu = isc.TreeGrid.create({
			    ID: item.name + 'Tree',
			    showHeader: false,
			    showOpenIcons:false,
			    showDropIcons:false,
			    showConnectors:true,
			    showRollOver:false,
		        leaveScrollbarGap: false,
				nodeIcon:null,
				folderIcon:null,
			    closedIconSuffix: '',
			    canDragRecordsOut: true, // for portal dropping
			    dragDataAction: 'copy', // for portal dropping
			    data: isc.Tree.create({
			        modelType: 'children',
			        nameProperty: 'desc',
			        childrenProperty: 'sub',
			        root: item.root
			    }),
				// Called when a menu item is selected
			    leafClick: function(viewer, leaf, recordNum) {
					if (leaf.ref == 'link') {
						window.location = leaf.name;
					}
					else if (leaf.ref == 'edit') {
						// remove the old view
						if (isc.BizUtil._currentView == isc.ListView.contents) {
							details.hideMember(isc.ListView.contents);
						}
						else {
							if (isc.BizUtil._currentView) {
								if (details.hasMember(isc.BizUtil._currentView)) {
									details.hideMember(isc.BizUtil._currentView);
								}
								isc.BizUtil.relinquishEditView(isc.BizUtil._currentView);
							}
						}

						// get the new view and edit a new record
						isc.BizUtil.getEditView(viewer.data.root.name, 
													leaf.name,
													function(view) { // the view
														details.addMember(view);
														isc.BizUtil._currentView = view;
														view.newInstance();
													});
					}
					else {
						if (isc.BizUtil._currentView != isc.ListView.contents) {
							if (isc.BizUtil._currentView) {
								if (details.hasMember(isc.BizUtil._currentView)) {
									details.hideMember(isc.BizUtil._currentView);
								}
								isc.BizUtil.relinquishEditView(isc.BizUtil._currentView);
							}
							isc.BizUtil._currentView = isc.ListView.contents;
							details.showMember(isc.ListView.contents);
						}
						if (leaf.ref == 'grid') {
							isc.ListView.setGridDataSource(viewer.data.root.name + "_" + leaf.name, leaf.config);
						}
						else if (leaf.ref == 'cal') {
							isc.ListView.setCalendarDataSource(viewer.data.root.name + "_" + leaf.name, leaf.config);
						}
						else if (leaf.ref == 'tree') {
							isc.ListView.setTreeDataSource(viewer.data.root.name + "_" + leaf.name, leaf.config);
						}
						else if (leaf.ref == 'map') {
							isc.ListView.setMapDataSource(viewer.data.root.name + "_" + leaf.name, leaf.config);
						}
						else {
							alert('Menu ref of ' + leaf.ref + 'is unknown');
						}
					}
				}
			});
			// add the section to the accordion pane
			adminStack.addSection({
				title: item.title,
				expanded: item.open,
				items: [menu]
			});
			// open all menu groups in the tree
			menu.getData().openAll();
		}

		// for each module in the dataSourceConfig, make a data source
		for (var i = 0, l = dataSourceConfig.length; i < l; i++) {
			var item = dataSourceConfig[i];
			// for filtering
			item.fields.add({name: 'operator', type: 'text', hidden: true});
			item.fields.add({name: 'criteria', type: 'text', hidden: true});

			// standard for all rows
			item.fields.add({name: 'bizId', primaryKey: true, hidden: true});
			item.fields.add({name:'bizLock', hidden: true});
			item.fields.addAt({name: 'bizTagged', title:'Tag', type: 'boolean', validOperators: ['equals']}, 0);
			item.fields.addAt({name: 'bizFlagComment', title:'Flag'}, 1);//,length:1024}, 0); long length makes filter builder use a text area

			isc.RestDataSource.create({ID: item.ID,
										dataFormat: 'json',
										jsonPrefix: '',
										jsonSuffix: '',
										dataURL: "smartlist",
										operationBindings : [
											{operationType: "fetch", dataProtocol: "postParams"},
											{operationType: "update", dataProtocol: "postParams"},
											{operationType: "add", dataProtocol: "postParams"},
											{operationType: "remove", dataProtocol: "postParams"}
										],
										// ensure all filtering is server-side
										// this enables the summary row to always stay in sync
										criteriaPolicy: "dropOnChange",
										title: item.title,
										modoc: item.modoc,
										icon: item.icon,
										fontIcon: item.fontIcon,
										aggregate: item.aggregate,
										canCreate: item.canCreate,
										canUpdate: item.canUpdate,
										canDelete: item.canDelete,
										cellHeight: item.cellHeight,
// This used to work in SC 8.2 but now doesn't send down any fields if present
// NB - it is meant to ensure that only datasource defined fields goes down with the request
//										sendExtraFields: false,
										fields: item.fields});
		}
		
		// add the listview to the details
		isc.BizUtil._currentView = isc.ListView.contents;
		details.addMember(isc.ListView.contents);
		details.hideMember(isc.ListView.contents);
/*
// Calendar bullshit
var _today = new Date;
var _start = _today.getDate() - _today.getDay();
var _month = _today.getMonth();
var _year = _today.getFullYear();
var eventData = [
{
    eventId: 1, 
    name: "Meeting",
    description: "Shareholders meeting: monthly forecast report",
    startDate: new Date(_year, _month, _start + 2, 9),
    endDate: new Date(_year, _month, _start + 2, 14)
},
{
    eventId: 2,
    name: "Realtor",
    description: "Breakfast with realtor to discuss moving plans",
    startDate: new Date(_year, _month, _start + 3, 8 ),
    endDate: new Date(_year, _month, _start + 3, 10)
},
{
    eventId: 3,
    name: "Soccer",
    description: "Little league soccer finals",
    startDate: new Date(_year, _month, _start + 4, 13),
    endDate: new Date(_year, _month, _start + 4, 16)
},
{
    eventId: 4, 
    name: "Sleep",
    description: "Catch up on sleep",
    startDate: new Date(_year, _month, _start + 4, 5),
    endDate: new Date(_year, _month, _start + 4, 9)
},
{
    eventId: 6,
    name: "Airport run",
    description: "Pick James up from the airport",
    startDate: new Date(_year, _month, _start + 4, 1),
    endDate: new Date(_year, _month, _start + 4, 3)
},
{
    eventId: 7,
    name: "Dinner Party",
    description: "Prepare elaborate meal for friends",
    startDate: new Date(_year, _month, _start + 4, 17),
    endDate: new Date(_year, _month, _start + 4, 20)
},
{
    eventId: 8,
    name: "Poker",
    description: "Poker at Steve's house",
    startDate: new Date(_year, _month, _start + 4, 21),
    endDate: new Date(_year, _month, _start + 4, 23)
},
{
    eventId: 9,
    name: "Meeting",
    description: "Board of directors meeting: discussion of next months strategy",
    startDate: new Date(_year, _month, _start + 5, 11),
    endDate: new Date(_year, _month, _start + 5, 15)
}
];

		// the single calendar window
		isc.Window.create({
			ID: 'calendar',
			title: "Calendar",
			autoCenter: true,
			isModal: true,
			showModalMask: true,
   			canDragReposition: true,
			canDragResize: true,
			keepInParentRect: true,
			showShadow: true,
			shadowSoftness: 10,
			shadowOffset: 0,
			items: [
				isc.Calendar.create({
					width: '100%',
					height: '100%',
					scrollToWorkDay: true,
					data: eventData
				})
			]
		});
*/

		isc.RestDataSource.create({
			ID: 'textSearch', 
			dataFormat: 'json',
			jsonPrefix: '',
			jsonSuffix: '',
			dataURL: 'smartsearch', 
			// ensure all queries are performed server-side
			criteriaPolicy: "dropOnChange",
			fields: [
				{name: 'icon', title: 'Icon'},
				{name: 'doc', title: 'Document'},
				{name: 'bizKey', title: 'Desciption'},
// {name: 'lastPost', mapping: 'post_time', type: 'date', dateFormat: 'timestamp'},
				{name: 'excerpt', title: 'Excerpt'},
				{name: 'score', title: 'Score (%)', type: 'integer'},
				{name: 'data', type: 'link', title: 'Data', target: '_self'},
				{name: 'content', type: 'link', title: 'Content', target:'_blank'}
            ]
        });
		
		isc.DynamicForm.create({
			ID:'textSearchForm',
			margin: 2,
			width: 500,
			height: 40,
			numCols: 3,
			colWidths: [300, 100, 100],
			items: [
				{name: "query",
					title: "Type and press search or enter",
					type: "text",
					keyPress: function(item, form, keyName, characterValue) {
						if ((keyName == "Enter") && form.validate(false)) {
							item.form.doSearch();
						}
					},
					required: true,
					validators: [
			            {clientOnly: true, 
			            	type: 'lengthRange',
			            	min: 4,
			            	errorMessage: 'At least 4 characters are required for a search'
			            }
		            ]
				},
				{name: "searchBtn", 
					startRow: false,
					title: "Search", 
					type: "button", 
					accessKey: "Enter",
					click: function() {
						this.form.doSearch();
					}
				},
				{name: 'message', type: 'blurb', startRow: true, colSpan: 3}
			],
			doSearch: function() {
				if (this.validate()) {
					var message = this.getItem('message');
					message.setValue('');
					textSearchResults.filterData(textSearchForm.getValuesAsCriteria());
				}
			}
		});
		
		isc.ListGrid.create({
			ID: "textSearchResults",
			width:"100%", 
			height:"100%", 
			alternateRecordStyles: true,
			dataSource: 'textSearch',
			dataFetchMode: 'basic', // no paging
			wrapCells: true,
		    fixedRecordHeights: false,
		    canEdit: false,
		    canFreezeFields: false,
		    canGroupBy: false,
		    canPickFields: false,
		    canSort: false,
		    fields:[
				{name: 'icon', align: 'center', width: 30},
				{name: 'doc', width: '15%'},
				{name: 'bizKey', width: '30%'},
				{name: 'excerpt', width: '55%'},
				{name: 'score', width: 75},
				{name: 'data', width: 40, align: 'center', linkText: 'Data'},
				{name: 'content', width: 60, align: 'center', linkText: 'Content'}
	        ],
			dataProperties: {
				transformData: function(newData, dsResponse) {
					if (dsResponse.status >= 0) { // success test
						var summary = newData.pop();
						var message = textSearchForm.getItem('message');
						var response = 'Query took ' + summary.time + ' seconds.';
						if (summary.suggestion) {
							response += '  Did you mean <a href="#" onclick="var q = textSearchForm.getItem(\'query\'); q.setValue(\'' + 
											summary.suggestion + '\'); q.focusInItem(); return false;">' + summary.suggestion + '</a>';
						}
						message.setValue(response);
					}
				}
			}
		});
	},
	
	popupSearch: function() {
		isc.WindowStack.popup(null, 'Text Search', true, [textSearchForm, textSearchResults]);
		textSearchForm.focusInItem('query');
		textSearchForm.clearValues();
	},
	
	showPortal: function() {
		if (isc.BizUtil._currentView != isc.ListView.contents) {
			if (isc.BizUtil._currentView) {
				if (details.hasMember(isc.BizUtil._currentView)) {
					details.hideMember(isc.BizUtil._currentView);
				}
				isc.BizUtil.relinquishEditView(isc.BizUtil._currentView);
			}
			isc.BizUtil._currentView = isc.ListView.contents;
			details.showMember(isc.ListView.contents);
		}

		isc.ListView.showPortal();
	},
	
	showHelp: function(url) {
		if (url) {} else {
			if (isc.BizUtil._currentView == isc.ListView.contents) {
				url = 'https://skyvers.github.io/skyve-user-guide/';
			}
			else {
				url = 'https://skyvers.github.io/skyve-user-guide/';
			}
		}
		isc.BizUtil.popupFrame(url, "Skyve Help", 1024, 768);
	}
});
