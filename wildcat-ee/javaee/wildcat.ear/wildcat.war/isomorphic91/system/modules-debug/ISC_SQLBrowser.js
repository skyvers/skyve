
/*

  SmartClient Ajax RIA system
  Version v9.1p_2014-03-26/LGPL Deployment (2014-03-26)

  Copyright 2000 and beyond Isomorphic Software, Inc. All rights reserved.
  "SmartClient" is a trademark of Isomorphic Software, Inc.

  LICENSE NOTICE
     INSTALLATION OR USE OF THIS SOFTWARE INDICATES YOUR ACCEPTANCE OF
     ISOMORPHIC SOFTWARE LICENSE TERMS. If you have received this file
     without an accompanying Isomorphic Software license file, please
     contact licensing@isomorphic.com for details. Unauthorized copying and
     use of this software is a violation of international copyright law.

  DEVELOPMENT ONLY - DO NOT DEPLOY
     This software is provided for evaluation, training, and development
     purposes only. It may include supplementary components that are not
     licensed for deployment. The separate DEPLOY package for this release
     contains SmartClient components that are licensed for deployment.

  PROPRIETARY & PROTECTED MATERIAL
     This software contains proprietary materials that are protected by
     contract and intellectual property law. You are expressly prohibited
     from attempting to reverse engineer this software or modify this
     software for human readability.

  CONTACT ISOMORPHIC
     For more information regarding license rights and restrictions, or to
     report possible license violations, please contact Isomorphic Software
     by email (licensing@isomorphic.com) or web (www.isomorphic.com).

*/

if(window.isc&&window.isc.module_Core&&!window.isc.module_SQLBrowser){isc.module_SQLBrowser=1;isc._moduleStart=isc._SQLBrowser_start=(isc.timestamp?isc.timestamp():new Date().getTime());if(isc._moduleEnd&&(!isc.Log||(isc.Log && isc.Log.logIsDebugEnabled('loadTime')))){isc._pTM={ message:'SQLBrowser load/parse time: ' + (isc._moduleStart-isc._moduleEnd) + 'ms', category:'loadTime'};
if(isc.Log && isc.Log.logDebug)isc.Log.logDebug(isc._pTM.message,'loadTime');
else if(isc._preLog)isc._preLog[isc._preLog.length]=isc._pTM;
else isc._preLog=[isc._pTM]}isc.definingFramework=true;isc.defineClass("SQLEditor","VLayout");
isc.A=isc.SQLEditor.getPrototype();
isc.B=isc._allFuncs;
isc.C=isc.B._maxIndex;
isc.D=isc._funcClasses;
isc.D[isc.C]=isc.A.Class;
isc.A.sqlInputFormDefaults={
    _constructor:"DynamicForm",
    height:150,
    showResizeBar:true
};
isc.A.actionButtonsDefaults={
    _constructor:"HLayout",
    layoutMargin:5,
    membersMargin:5,
    height:20
};
isc.A.execSQLButtonDefaults={
    _constructor:"IButton",
    title:"Exec SQL",
    click:"this.creator.execSQL();",
    autoParent:"actionButtons"
};
isc.A.previewGridDefaults={
    _constructor:"ListGrid",
    dataProperties:{
        progressiveLoading:true
    },
    minFieldWidth:100,
    autoFetchData:false
};
isc.A.previewGridStripDefaults={
    _constructor:"GridToolStrip",
    width:"100%",
    generateDSButtonDefaults:{
        _constructor:"IAutoFitButton",
        title:"Show DataSource",
        layoutAlign:"center",
        click:"this.creator.creator.showDS()"
    },
    members:["autoChild:exportButton",
              "starSpacer",
              "autoChild:refreshButton","autoChild:totalRowsIndicator"
    ]
};
isc.B.push(isc.A.initWidget=function isc_SQLEditor_initWidget(){
    this.Super("initWidget",arguments);
    var sqlEditor=this;
    this.addAutoChild("sqlInputForm",{
        fields:[
            {name:"sql",showTitle:false,type:"textarea",
             width:"*",height:"*",colSpan:"*",
             keyPress:function(item,form,keyName){
                if(keyName=='Enter'&&isc.EH.ctrlKeyDown()){
                   if(isc.Browser.isSafari)item.setValue(item.getElementValue());
                   sqlEditor.execSQL();
                   if(isc.Browser.isSafari)return false;
                }
            }}
        ]
    });
    this.addAutoChildren(["actionButtons","execSQLButton"]);
}
,isc.A.execSQL=function isc_SQLEditor_execSQL(){
    var sql=this.sqlInputForm.getValue("sql");
    if(sql){
        sql=sql.trim().replace(/(.*);+/,"$1");
        var ds=isc.DataSource.get("DataSourceStore");
        ds.performCustomOperation("dsFromSQL",{dbName:this.config.name,sql:sql},this.getID()+".dsLoaded(data)");
    }
}
,isc.A.dsLoaded=function isc_SQLEditor_dsLoaded(data){
    var ds=data.ds;
    if(!this.previewGrid)this.addAutoChild("previewGrid",{dataSource:ds});
    else this.previewGrid.setDataSource(ds);
    this.previewGrid.fetchData();
    this.addAutoChild("previewGridStrip",{
        grid:this.previewGrid
    });
}
);
isc.B._maxIndex=isc.C+3;

isc.defineClass("SQLTableBrowser","VLayout");
isc.A=isc.SQLTableBrowser.getPrototype();
isc.B=isc._allFuncs;
isc.C=isc.B._maxIndex;
isc.D=isc._funcClasses;
isc.D[isc.C]=isc.A.Class;
isc.A.previewGridDefaults={
    _constructor:"ListGrid",
    canDragSelectText:true,
    autoFetchData:false,
    height:"*",
    minFieldWidth:100,
    showFilterEditor:true,
    canEdit:true,
    dataProperties:{
        progressiveLoading:true
    }
};
isc.A.previewGridStripDefaults={
    _constructor:"GridToolStrip",
    width:"100%",
    generateDSButtonDefaults:{
        _constructor:"IAutoFitButton",
        title:"Show DataSource",
        layoutAlign:"center",
        click:"this.creator.creator.showDS()"
    },
    members:["autoChild:removeButton","autoChild:addButton","autoChild:exportButton","autoChild:generateDSButton",
              "starSpacer",
              "autoChild:refreshButton","autoChild:totalRowsIndicator"
    ]
};
isc.B.push(isc.A.initWidget=function isc_SQLTableBrowser_initWidget(){
    this.Super("initWidget",arguments);
    var ds=isc.DataSource.get("DataSourceStore");
    ds.performCustomOperation("dsFromTable",{schema:this.schema,dbName:this.dbName,tableName:this.config.name},this.getID()+".dsLoaded(data)");
}
,isc.A.dsLoaded=function isc_SQLTableBrowser_dsLoaded(data){
    this.dataSource=data.ds;
    this.dataSourceXML=data.dsXML;
    this.addAutoChild("previewGrid",{
        dataSource:this.dataSource
    });
    this.addAutoChild("previewGridStrip",{
        grid:this.previewGrid
    });
    this.previewGrid.filterData();
}
,isc.A.showDS=function isc_SQLTableBrowser_showDS(){
    var dsXMLHeader="<DataSource ID=\""+this.config.name+"\" serverType=\"sql\" dbName=\""+this.dbName+"\" tableName=\""+this.config.name+"\"";
    if(this.schema)dsXMLHeader+=" schema=\""+this.schema+"\"";
    dsXMLHeader+=">";
    var dsXML=this.dataSourceXML;
    dsXML=dsXML.substring(dsXML.indexOf(">")+1);
    dsXML=dsXMLHeader+dsXML;
    isc.Window.create({
        title:"DataSource XML for table: "+this.config.name,
        autoDraw:true,
        autoSize:true,
        autoCenter:true,
        items:[
            isc.DynamicForm.create({
                numCols:1,
                width:600,
                height:600,
                autoFocus:true,
                selectOnFocus:true,
                fields:[
                    {name:"dsData",showTitle:false,type:"textArea",wrap:isc.TextAreaItem.OFF,defaultValue:dsXML,width:"*",height:"*"}
                ],
                closeClick:function(){
                     this.destroy();
                     return false;
                }
            })
        ]
    });
}
);
isc.B._maxIndex=isc.C+3;

isc.defineClass("DBPane","TabSet");
isc.A=isc.DBPane.getPrototype();
isc.B=isc._allFuncs;
isc.C=isc.B._maxIndex;
isc.D=isc._funcClasses;
isc.D[isc.C]=isc.A.Class;
isc.A.sqlEditorDefaults={
    _constructor:"SQLEditor"
};
isc.A.tablePaneDefaults={
    _constructor:"SQLTableBrowser"
};
isc.B.push(isc.A.initWidget=function isc_DBPane_initWidget(){
    this.Super("initWidget",arguments);
    this.sqlEditor=this.createAutoChild("sqlEditor",{config:this.config});
    this.addTab({title:"SQL Editor",pane:this.sqlEditor});
}
,isc.A.showTableBrowser=function isc_DBPane_showTableBrowser(table){
    var tabId=this.escapeForId(this.config.name+'_'+table.name);
    this.showPane({ID:tabId,title:table.name,paneClass:"tablePane"},table);
}
,isc.A.escapeForId=function isc_DBPane_escapeForId(s){
    return isc.isA.String(s)?s.replace(/(\/|\.)/g,'_'):s;
}
,isc.A.showPane=function isc_DBPane_showPane(props,childConfig){
    var tab=this.getTab(props.ID);
    if(tab){
        this.selectTab(tab);
        return;
    }
    tab={};
    isc.addProperties(tab,props,{canClose:true,pane:this.createAutoChild(props.paneClass,{config:childConfig,dbName:this.config.name})});
    this.addTab(tab);
    this.selectTab(tab);
    this.currentPane=tab.pane;
}
);
isc.B._maxIndex=isc.C+4;

isc.DataSource.create({
    allowAdvancedCriteria:true,
    ID:"DBSchema",
    operationBindings:[
        {
            operationType:"fetch"
        }
    ],
    fields:[
        {
            name:"name"
        },
        {
            name:"itemType"
        },
        {
            name:"type"
        },
        {
            name:"length",
            type:"integer"
        },
        {
            name:"primaryKey",
            type:"boolean"
        },
        {
            hidden:true,
            primaryKey:true,
            name:"path"
        },
        {
            hidden:true,
            name:"parentID",
            foreignKey:"DBSchema.path"
        }
    ]
})
isc.defineClass("DBSchemaTree","ListGrid");
isc.A=isc.DBSchemaTree.getPrototype();
isc.B=isc._allFuncs;
isc.C=isc.B._maxIndex;
isc.D=isc._funcClasses;
isc.D[isc.C]=isc.A.Class;
isc.A.showFilterEditor=true;
isc.A.filterOnKeypress=true;
isc.A.serverType="sql";
isc.A.emptyMessage="No tables defined";
isc.A.canExpandRecords=true;
isc.A.detailDefaults={
    _constructor:"ListGrid",
    autoFitData:"vertical",
    autoFitMaxRecords:8,
    showResizeBar:true
};
isc.B.push(isc.A.initWidget=function isc_DBSchemaTree_initWidget(){
    this.dataSource=isc.DataSource.create({
        ID:this.getID()+"_DB_DS",
        clientOnly:true,
        fields:[
            {name:"name",title:"Name"},
            {name:"type",title:"Type",width:60,valueMap:["table","view"]}
        ]
    });
    this.Super("initWidget",arguments);
}
,isc.A.selectionChanged=function isc_DBSchemaTree_selectionChanged(record,state){
    this.tableSelected(record.name);
}
,isc.A.tableSelected=function isc_DBSchemaTree_tableSelected(tableName){}
,isc.A.getExpansionComponent=function isc_DBSchemaTree_getExpansionComponent(record){
    var component=this.createAutoChild("detail",{
        sortField:"primaryKey",
        sortDirection:"descending",
        fields:[
            {name:"name",title:"Column",formatCellValue:function(value,record){
                if(record.primaryKey)return"<b>"+value+"</b>";
                return value;
            }},
            {name:"type",title:"Type",width:50},
            {name:"length",title:"Length",width:45},
            {name:"primaryKey",title:"PK",type:"boolean",showIf:"false",width:22}
        ]
    });
    isc.DMI.call("isc_builtin","com.isomorphic.tools.BuiltinRPC","getFieldsFromTable",
        record.name,this.schema,this.serverType,this.db.name,
        function(rpcResponse,data){
            component.setData(data);
        }
    );
    return component;
}
,isc.A.invalidateCache=function isc_DBSchemaTree_invalidateCache(){
    this.setData([]);
    this.loadSchema(this.db);
}
,isc.A.loadSchema=function isc_DBSchemaTree_loadSchema(db){
    this.db=db;
    isc.showPrompt("Loading schema for database: "+db.name);
    isc.DMI.call("isc_builtin","com.isomorphic.tools.BuiltinRPC","getTables",
                 this.serverType,db.name,true,true,this.catalog,this.schema,this.includeList,this.excludeList,
                 this.getID()+".loadSchemaReply(data)");
}
,isc.A.loadSchemaReply=function isc_DBSchemaTree_loadSchemaReply(data){
    isc.clearPrompt();
    for(var i=0;i<data.length;i++){
        data[i].name=data[i].TABLE_NAME;
        data[i].type=data[i].TABLE_TYPE.toLowerCase();
    }
    this.setData(isc.ResultSet.create({
        dataSource:this.dataSource,
        allRows:data
    }));
    this.sort("name");
    if(this.schemaLoaded)this.fireCallback("schemaLoaded");
}
);
isc.B._maxIndex=isc.C+7;

isc.DBSchemaTree.registerStringMethods({
    schemaLoaded:""
});isc.DataSource.create({
    allowAdvancedCriteria:true,
    ID:"DataSourceStore",
    operationBindings:[
        {
            operationType:"custom",
            operationId:"dsFromSQL"
        },
        {
            operationType:"custom",
            operationId:"dsFromTable"
        },
        {
            operationType:"custom",
            operationId:"dsFromConfig"
        }
    ],
    fields:[
        {
            primaryKey:true,
            name:"ID"
        },
        {
            name:"version"
        },
        {
            name:"dsXML",
            length:50000,
            type:"text"
        },
        {
            hidden:true,
            name:"config"
        },
        {
            hidden:true,
            name:"dbName"
        },
        {
            hidden:true,
            name:"tableName"
        },
        {
            hidden:true,
            name:"schema"
        },
        {
            hidden:true,
            name:"sql"
        },
        {
            hidden:true,
            name:"ds",
            type:"DataSource"
        }
    ]
})
isc.DataSource.create({
    allowAdvancedCriteria:true,
    dropExtraFields:false,
    ID:"DBListDS",
    operationBindings:[
        {
            operationType:"fetch"
        }
    ],
    fields:[
        {
            primaryKey:true,
            name:"name"
        },
        {
            name:"type"
        },
        {
            name:"version"
        },
        {
            name:"driverVersion"
        },
        {
            name:"status"
        }
    ]
})
isc.defineClass("DBCompactList","DynamicForm");
isc.A=isc.DBCompactList.getPrototype();
isc.B=isc._allFuncs;
isc.C=isc.B._maxIndex;
isc.D=isc._funcClasses;
isc.D[isc.C]=isc.A.Class;
isc.A.width=200;
isc.A.numCols=2;
isc.A.colWidths=[80,"*"];
isc.B.push(isc.A.initWidget=function isc_DBCompactList_initWidget(){
    this.fields=[{
        name:"dbName",title:"Database",type:"select",width:"*",type:"select",width:120,
        optionDataSource:"DBListDS",displayField:"name",valueField:"name",
        change:"if (this.form.databaseChanged) this.form.fireCallback('databaseChanged', 'dbName', [value])",valueMap:{}
    }];
    this.Super("initWidget",arguments);
}
,isc.A.getSelectedDB=function isc_DBCompactList_getSelectedDB(){
    return this.getValue("dbName");
}
,isc.A.setSelectedDB=function isc_DBCompactList_setSelectedDB(db){
    return this.setValue("dbName",db);
}
);
isc.B._maxIndex=isc.C+3;

isc.DBCompactList.registerStringMethods({
    databaseChanged:"dbName"
});isc.defineClass("DBList","ListGrid");
isc.A=isc.DBList.getPrototype();
isc.B=isc._allFuncs;
isc.C=isc.B._maxIndex;
isc.D=isc._funcClasses;
isc.D[isc.C]=isc.A.Class;
isc.A.dataSource="DBListDS";
isc.A.showFilterEditor=true;
isc.A.filterOnKeypress=true;
isc.A.sortField="name";
isc.B.push(isc.A.initWidget=function isc_DBList_initWidget(){
    this.Super("initWidget",arguments);
}
,isc.A.dataArrived=function isc_DBList_dataArrived(){
    this.Super("dataArrived",arguments);
    if(!this.initialCriteriaSet){
        var initialCriteria={status:"OK"};
        this.setFilterEditorCriteria(initialCriteria);
        this.initialCriteriaSet=true;
        this.filterData(initialCriteria);
    }
    this.initialCriteriaSet=false;
}
,isc.A.cellHoverHTML=function isc_DBList_cellHoverHTML(record){
    if(!this.hoverDV)this.hoverDV=isc.DetailViewer.create({dataSource:this.dataSource,width:200,autoDraw:false});
    this.hoverDV.setData(record);
    return this.hoverDV.getInnerHTML();
}
,isc.A.destroy=function isc_DBList_destroy(){
    if(this.hoverDV)this.hoverDV.destroy();
    this.Super("destroy",arguments);
}
);
isc.B._maxIndex=isc.C+4;
isc.defineClass("SQLBrowser","VLayout");
isc.A=isc.SQLBrowser;
isc.B=isc._allFuncs;
isc.C=isc.B._maxIndex;
isc.D=isc._funcClasses;
isc.D[isc.C]=isc.A.Class;
isc.B.push(isc.A.showWindow=function isc_c_SQLBrowser_showWindow(windowProps,sqlBrowserProps){
    isc.Window.create({
        title:"SQL Browser",
        width:"100%",
        height:"100%",
        canDragReposition:false,
        closeClick:function(){this.destroy();},
        items:[
            isc.SQLBrowser.create({autoDraw:false},sqlBrowserProps)
        ]
    },windowProps).show();
}
);
isc.B._maxIndex=isc.C+1;

isc.A=isc.SQLBrowser.getPrototype();
isc.B=isc._allFuncs;
isc.C=isc.B._maxIndex;
isc.D=isc._funcClasses;
isc.D[isc.C]=isc.A.Class;
isc.A.dbListDefaults={
    _constructor:"DBList",
    height:150,
    canDragSelectText:true,
    autoFetchData:true,
    canHover:true,
    defaultFields:[
        {name:"name"},
        {name:"status"}
    ]
};
isc.A.dbListRefreshButtonDefaults={
    _constructor:"Img",
    size:16,
    src:"[SKIN]/actions/refresh.png",
    click:"this.creator.dbList.invalidateCache()"
};
isc.A.dbSchemaTreeDefaults={
    _constructor:"DBSchemaTree",
    canDragSelectText:true,
    animateFolders:false,
    showConnectors:false,
    recordClick:function(viewer,record){
        this.creator.showTablePane(record);
    }
};
isc.A.dbSchemaRefreshButtonDefaults={
    _constructor:"Img",
    size:16,
    src:"[SKIN]/actions/refresh.png",
    click:"this.creator.dbSchemaTree.invalidateCache()"
};
isc.A.leftSectionDefaults={
    _constructor:"SectionStack",
    headerHeight:25,
    width:300,
    showResizeBar:true,
    animateSections:isc.Browser.isSafari,
    visibilityMode:"visible",
    autoParent:"mainLayout"
};
isc.A.mainLayoutDefaults={
    _constructor:"HLayout",
    height:"*"
};
isc.A.rightPaneDefaults={
    _constructor:"TabSet",
    tabs:[
        {name:"welcome",title:"Welcome",ID:"dsb_welcome_tab",canClose:true,
         pane:isc.Label.create({
             height:10,
             autoDraw:false,
             overflow:"visible",
             contents:"Select a database on the left..."
         })
        }
    ]
};
isc.A.autoChildren=["mainLayout"];
isc.A.dbPaneDefaults={
    _constructor:"DBPane"
};
isc.B.push(isc.A.initWidget=function isc_SQLBrowser_initWidget(){
    this.Super("initWidget",arguments);
    this.dbList=this.createAutoChild("dbList",{
        selectionChanged:"if (state) this.creator.databaseChanged(record)"
    });
    this.dbListRefreshButton=this.createAutoChild("dbListRefreshButton");
    this.dbSchemaTree=this.createAutoChild("dbSchemaTree",{
    });
    this.dbSchemaRefreshButton=this.createAutoChild("dbSchemaRefreshButton");
    this.leftSection=this.createAutoChild("leftSection",{
        sections:[
            {name:"databases",title:"Databases",expanded:true,controls:[this.dbListRefreshButton],items:[
                this.dbList
            ]},
            {name:"tables",title:"Tables & Views",expanded:true,controls:[this.dbSchemaRefreshButton],items:[
                this.dbSchemaTree
            ]}
        ]
    });
    this.addAutoChildren(this.autoChildren);
    this.mainLayout.addMember(this.leftSection);
    this.rightPane=this.createAutoChild("rightPane");
    this.mainLayout.addMember(this.rightPane);
}
,isc.A.showDBPane=function isc_SQLBrowser_showDBPane(){
    var db=this.db;
    this.showPane({ID:this.escapeForId("db_"+db.name),title:db.name,paneClass:"dbPane"},db);
}
,isc.A.databaseChanged=function isc_SQLBrowser_databaseChanged(db){
    if(db.status=="OK"){
        this.db=db;
        this.dbSchemaTree.loadSchema(db);
        this.showDBPane();
    }
}
,isc.A.showTablePane=function isc_SQLBrowser_showTablePane(record){
    this.showDBPane();
    this.currentPane.showTableBrowser(record);
}
,isc.A.escapeForId=function isc_SQLBrowser_escapeForId(s){
    return isc.isA.String(s)?s.replace(/(\/|\.)/g,'_'):s;
}
,isc.A.showPane=function isc_SQLBrowser_showPane(props,childConfig){
    var tab=this.rightPane.getTab(props.ID);
    if(tab){
        this.currentPane=tab.pane;
        this.rightPane.selectTab(tab);
        return;
    }
    tab={};
    isc.addProperties(tab,props,{canClose:true,pane:this.createAutoChild(props.paneClass,{config:childConfig})});
    var firstTab=this.rightPane.getTab(0);
    if(firstTab&&firstTab.name=="welcome")this.rightPane.removeTab(0);
    this.rightPane.addTab(tab);
    this.rightPane.selectTab(tab);
    this.currentPane=tab.pane;
}
);
isc.B._maxIndex=isc.C+6;
isc._debugModules = (isc._debugModules != null ? isc._debugModules : []);isc._debugModules.push('SQLBrowser');isc.checkForDebugAndNonDebugModules();isc._moduleEnd=isc._SQLBrowser_end=(isc.timestamp?isc.timestamp():new Date().getTime());if(isc.Log&&isc.Log.logIsInfoEnabled('loadTime'))isc.Log.logInfo('SQLBrowser module init time: ' + (isc._moduleEnd-isc._moduleStart) + 'ms','loadTime');delete isc.definingFramework;}else{if(window.isc && isc.Log && isc.Log.logWarn)isc.Log.logWarn("Duplicate load of module 'SQLBrowser'.");}

/*

  SmartClient Ajax RIA system
  Version v9.1p_2014-03-26/LGPL Deployment (2014-03-26)

  Copyright 2000 and beyond Isomorphic Software, Inc. All rights reserved.
  "SmartClient" is a trademark of Isomorphic Software, Inc.

  LICENSE NOTICE
     INSTALLATION OR USE OF THIS SOFTWARE INDICATES YOUR ACCEPTANCE OF
     ISOMORPHIC SOFTWARE LICENSE TERMS. If you have received this file
     without an accompanying Isomorphic Software license file, please
     contact licensing@isomorphic.com for details. Unauthorized copying and
     use of this software is a violation of international copyright law.

  DEVELOPMENT ONLY - DO NOT DEPLOY
     This software is provided for evaluation, training, and development
     purposes only. It may include supplementary components that are not
     licensed for deployment. The separate DEPLOY package for this release
     contains SmartClient components that are licensed for deployment.

  PROPRIETARY & PROTECTED MATERIAL
     This software contains proprietary materials that are protected by
     contract and intellectual property law. You are expressly prohibited
     from attempting to reverse engineer this software or modify this
     software for human readability.

  CONTACT ISOMORPHIC
     For more information regarding license rights and restrictions, or to
     report possible license violations, please contact Isomorphic Software
     by email (licensing@isomorphic.com) or web (www.isomorphic.com).

*/

