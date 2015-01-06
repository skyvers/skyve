
/*

  SmartClient Ajax RIA system
  Version v10.0p_2015-01-04/LGPL Deployment (2015-01-04)

  Copyright 2000 and beyond Isomorphic Software, Inc. All rights reserved.
  "SmartClient" is a trademark of Isomorphic Software, Inc.

  LICENSE NOTICE
     INSTALLATION OR USE OF THIS SOFTWARE INDICATES YOUR ACCEPTANCE OF THE
     SOFTWARE LICENSE AGREEMENT. If you have received this file without an 
     Isomorphic Software license file, please see:

         http://www.isomorphic.com/licenses/license-sisv.html

     You are not required to accept this agreement, however, nothing else
     grants you the right to copy or use this software. Unauthorized copying
     and use of this software is a violation of international copyright law.

  PROPRIETARY & PROTECTED MATERIAL
     This software contains proprietary materials that are protected by
     contract and intellectual property law. YOU ARE EXPRESSLY PROHIBITED
     FROM ATTEMPTING TO REVERSE ENGINEER THIS SOFTWARE OR MODIFY THIS
     SOFTWARE FOR HUMAN READABILITY.

  CONTACT ISOMORPHIC
     For more information regarding license rights and restrictions, or to
     report possible license violations, please contact Isomorphic Software
     by email (licensing@isomorphic.com) or web (www.isomorphic.com).

*/

if(window.isc&&window.isc.module_Core&&!window.isc.module_DSBrowser){isc.module_DSBrowser=1;isc._moduleStart=isc._DSBrowser_start=(isc.timestamp?isc.timestamp():new Date().getTime());if(isc._moduleEnd&&(!isc.Log||(isc.Log && isc.Log.logIsDebugEnabled('loadTime')))){isc._pTM={ message:'DSBrowser load/parse time: ' + (isc._moduleStart-isc._moduleEnd) + 'ms', category:'loadTime'};
if(isc.Log && isc.Log.logDebug)isc.Log.logDebug(isc._pTM.message,'loadTime');
else if(isc._preLog)isc._preLog[isc._preLog.length]=isc._pTM;
else isc._preLog=[isc._pTM]}isc.definingFramework=true;isc.defineClass("DSRegistryList","ListGrid");
isc.A=isc.DSRegistryList.getPrototype();
isc.B=isc._allFuncs;
isc.C=isc.B._maxIndex;
isc.D=isc._funcClasses;
isc.D[isc.C]=isc.A.Class;
isc.A.dataSource="RepoRegistry";
isc.B.push(isc.A.initWidget=function isc_DSRegistryList_initWidget(){
    this.Super("initWidget",arguments);
}
);
isc.B._maxIndex=isc.C+1;

isc.defineClass("DSList","ListGrid");
isc.A=isc.DSList.getPrototype();
isc.B=isc._allFuncs;
isc.C=isc.B._maxIndex;
isc.D=isc._funcClasses;
isc.D[isc.C]=isc.A.Class;
isc.B.push(isc.A.initWidget=function isc_DSList_initWidget(){
    this.Super("initWidget",arguments);
}
);
isc.B._maxIndex=isc.C+1;

isc.defineClass("DSBrowser","VLayout");
isc.A=isc.DSBrowser;
isc.B=isc._allFuncs;
isc.C=isc.B._maxIndex;
isc.D=isc._funcClasses;
isc.D[isc.C]=isc.A.Class;
isc.B.push(isc.A.showWindow=function isc_c_DSBrowser_showWindow(windowProps,componentProps){
    isc.Window.create({
        title:"DS Builder",
        width:"100%",
        height:"100%",
        canDragReposition:false,
        closeClick:function(){this.destroy();},
        items:[
            isc.DSBrowser.create({autoDraw:false},componentProps)
        ]
    },windowProps).show();
}
);
isc.B._maxIndex=isc.C+1;

isc.A=isc.DSBrowser.getPrototype();
isc.B=isc._allFuncs;
isc.C=isc.B._maxIndex;
isc.D=isc._funcClasses;
isc.D[isc.C]=isc.A.Class;
isc.A.dsRegistryListDefaults={
    _constructor:"DSRegistryList",
    height:150,
    autoFetchData:true,
    canHover:true,
    defaultFields:[
        {name:"ID",title:"Name"}
    ],
    recordClick:"this.creator.dsRegistryChanged(record)"
};
isc.A.dsRegistryListRefreshButtonDefaults={
    _constructor:"Img",
    size:16,
    src:"[SKIN]/actions/refresh.png",
    click:"this.creator.dsRegistryList.invalidateCache()"
};
isc.A.dsListDefaults={
    _constructor:"DSList",
    canHover:true,
    showFilterEditor:true,
    defaultFields:[
        {name:"ID",title:"Name"}
    ],
    recordClick:"this.creator.dsChanged(record)"
};
isc.A.dsListAddButtonDefaults={
    _constructor:"Img",
    size:16,
    src:"[SKIN]/actions/add.png",
    click:"this.creator.dsList.startEditingNew()"
};
isc.A.dsListRemoveButtonDefaults={
    _constructor:"Img",
    size:16,
    src:"[SKIN]/actions/remove.png",
    click:"this.creator.dsList.removeSelectedData()"
};
isc.A.dsListRefreshButtonDefaults={
    _constructor:"Img",
    size:16,
    src:"[SKIN]/actions/refresh.png",
    click:"this.creator.dsList.invalidateCache()"
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
             contents:"Select a datasource registry on the left..."
         })
        }
    ]
};
isc.A.autoChildren=["mainLayout"];
isc.A.dsRegistryPaneDefaults={
    _constructor:"DSRegistryPane"
};
isc.B.push(isc.A.initWidget=function isc_DSBrowser_initWidget(){
    this.Super("initWidget",arguments);
    this.dsRegistryList=this.createAutoChild("dsRegistryList");
    this.dsRegistryListRefreshButton=this.createAutoChild("dsRegistryListRefreshButton");
    this.dsList=this.createAutoChild("dsList");
    this.dsListAddButton=this.createAutoChild("dsListAddButton");
    this.dsListRemoveButton=this.createAutoChild("dsListRemoveButton");
    this.dsListRefreshButton=this.createAutoChild("dsListRefreshButton");
    this.leftSection=this.createAutoChild("leftSection",{
        sections:[
            {name:"registries",title:"DataSource Registries",expanded:true,controls:[this.dsRegistryListRefreshButton],items:[
                this.dsRegistryList
            ]},
            {name:"datasources",title:"DataSources",expanded:true,controls:[this.dsListAddButton,this.dsListRemoveButton,this.dsListRefreshButton],items:[
                this.dsList
            ]}
        ]
    });
    this.addAutoChildren(this.autoChildren);
    this.mainLayout.addMember(this.leftSection);
    this.rightPane=this.createAutoChild("rightPane");
    this.mainLayout.addMember(this.rightPane);
}
,isc.A.dsRegistryChanged=function isc_DSBrowser_dsRegistryChanged(record){
    this.currentRegistry=record;
    isc.DMI.call("isc_builtin","com.isomorphic.tools.BuiltinRPC","dsFromXML",
                 record.object,
                 this.getID()+".dsLoaded(data)");
}
,isc.A.dsLoaded=function isc_DSBrowser_dsLoaded(ds){
    this.currentDS=ds;
    this.showDSRegistryPane();
    this.dsList.setDataSource(ds);
    this.dsList.setFields([
        {name:"ID",title:"Name"}
    ]);
    this.dsList.filterData();
}
,isc.A.showDSRegistryPane=function isc_DSBrowser_showDSRegistryPane(){
    var registry=this.currentRegistry;
    this.showPane({ID:this.escapeForId("registryPane_"+registry.ID),title:registry.ID,paneClass:"dsRegistryPane"},registry);
}
,isc.A.dsChanged=function isc_DSBrowser_dsChanged(record){
    this.currentDS=record;
    this.showDSPane();
}
,isc.A.showDSPane=function isc_DSBrowser_showDSPane(){
    var ds=this.currentDS;
    this.showDSRegistryPane();
    var props={};
    isc.addProperties(props,ds,{registry:isc.clone(this.currentRegistry)});
    this.currentPane.showDSPane(props);
}
,isc.A.escapeForId=function isc_DSBrowser_escapeForId(s){
    return isc.isA.String(s)?s.replace(/(\/|\.)/g,'_'):s;
}
,isc.A.showPane=function isc_DSBrowser_showPane(props,childConfig){
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
isc.B._maxIndex=isc.C+8;

isc.defineClass("DSRegistryPane","TabSet");
isc.A=isc.DSRegistryPane.getPrototype();
isc.B=isc._allFuncs;
isc.C=isc.B._maxIndex;
isc.D=isc._funcClasses;
isc.D[isc.C]=isc.A.Class;
isc.A.dsPaneDefaults={
    _constructor:"DSEditor"
};
isc.B.push(isc.A.initWidget=function isc_DSRegistryPane_initWidget(){
    this.Super("initWidget",arguments);
}
,isc.A.showDSPane=function isc_DSRegistryPane_showDSPane(ds){
    var tabId=this.escapeForId("dsPane_"+this.config.ID+'_'+ds.ID);
    this.showPane({ID:tabId,title:ds.ID,paneClass:"dsPane"},ds);
}
,isc.A.escapeForId=function isc_DSRegistryPane_escapeForId(s){
    return isc.isA.String(s)?s.replace(/(\/|\.)/g,'_'):s;
}
,isc.A.showPane=function isc_DSRegistryPane_showPane(props,childConfig){
    var tab=this.getTab(props.ID);
    if(tab){
        this.selectTab(tab);
        return;
    }
    tab={};
    isc.addProperties(tab,props,{canClose:true,pane:this.createAutoChild(props.paneClass,{config:childConfig})});
    this.addTab(tab);
    this.selectTab(tab);
    this.currentPane=tab.pane;
}
);
isc.B._maxIndex=isc.C+4;

isc.defineClass("DSEditor","SectionStack");
isc.A=isc.DSEditor.getPrototype();
isc.B=isc._allFuncs;
isc.C=isc.B._maxIndex;
isc.D=isc._funcClasses;
isc.D[isc.C]=isc.A.Class;
isc.A.visibilityMode="visible";
isc.A.fieldGridDefaults={
    _constructor:"ListGrid",
    canReorderRecords:true,
    canDragRecordsOut:false,
    canEdit:true,
    autoSaveEdits:true
};
isc.A.fieldGridAddButtonDefaults={
    _constructor:"Img",
    size:16,
    src:"[SKIN]/actions/add.png",
    click:"this.creator.fieldGrid.startEditingNew()"
};
isc.A.fieldGridRemoveButtonDefaults={
    _constructor:"Img",
    size:16,
    src:"[SKIN]/actions/remove.png",
    click:"this.creator.fieldGrid.removeSelectedData()"
};
isc.A.fieldGridRefreshButtonDefaults={
    _constructor:"Img",
    size:16,
    src:"[SKIN]/actions/refresh.png",
    click:"this.creator.fieldGrid.invalidateCache()"
};
isc.A.deriveFormDefaults={
    _constructor:"DynamicForm"
};
isc.A.dbListDefaults={
    _constructor:"DBCompactList"
};
isc.A.showSQLBrowserButtonDefaults={
    _constructor:"IButton",
    title:"Show SQL Browser",
    width:150,
    click:function(){
       isc.SQLBrowser.showWindow({
           width:"95%",
           height:"95%",
           isModal:true,
           autoCenter:true
       });
    }
};
isc.A.fetchOperationFormDefaults={
    _constructor:"DynamicForm",
    fields:[
        {name:"selectClause",title:"SELECT",formItemType:"AutoFitTextAreaItem",height:10,width:"*",colSpan:"*",defaultValue:"*"},
        {name:"tableClause",title:"FROM",formItemType:"AutoFitTextAreaItem",height:10,width:"*",colSpan:"*",defaultValue:""},
        {name:"whereClause",title:"WHERE",formItemType:"AutoFitTextAreaItem",height:10,width:"*",colSpan:"*",defaultValue:"$defaultWhereClause"},
        {name:"groupClause",title:"GROUP BY",formItemType:"AutoFitTextAreaItem",height:10,width:"*",colSpan:"*",defaultValue:"$defaultGroupClause"},
        {name:"orderClause",title:"ORDER BY",formItemType:"AutoFitTextAreaItem",height:10,width:"*",colSpan:"*",defaultValue:"$defaultOrderClause"}
    ]
};
isc.A.actionBarDefaults={
    _constructor:"HLayout",
    height:20
};
isc.A.tryButtonDefaults={
    _constructor:"IButton",
    title:"Try it",
    click:"this.creator.tryIt()",
    autoParent:"actionBar"
};
isc.A.saveButtonDefaults={
    _constructor:"IButton",
    title:"Save",
    click:"this.creator.saveDS()",
    autoParent:"actionBar"
};
isc.A.previewGridDefaults={
    _constructor:"ListGrid",
    showFilterEditor:true
};
isc.B.push(isc.A.initWidget=function isc_DSEditor_initWidget(){
    this.Super("initWidget",arguments);
    this.fieldGrid=this.createAutoChild("fieldGrid",{
        fields:[
            {name:"title",title:"Title"},
            {name:"name",title:"Name"},
            {name:"width",title:"Width"},
            {name:"height",title:"Height"},
            {name:"operator",title:"Operator",valueMap:[
                "equals",
                "notEqual",
                "greaterThan",
                "lessThan",
                "greaterOrEqual",
                "lessOrEqual",
                "contains",
                "startsWith",
                "endsWith",
                "iContains",
                "iStartsWith",
                "iEndsWith",
                "notContains",
                "notStartsWith",
                "notEndsWith",
                "iNotContains",
                "iNotStartsWith",
                "iNotEndsWith",
                "regexp",
                "iregexp",
                "isNull",
                "notNull",
                "inSet",
                "notInSet",
                "equalsField",
                "notEqualField",
                "and",
                "not",
                "or",
                "between",
                "betweenInclusive"
            ]},
            {name:"formItemType",title:"Form Item Type"},
            {name:"tableName",title:"Table Name"},
            {name:"type",title:"Type"}
        ]
    });
    this.fieldGridAddButton=this.createAutoChild("fieldGridAddButton");
    this.fieldGridRemoveButton=this.createAutoChild("fieldGridRemoveButton");
    this.addSection({
        ID:"fields",title:"Fields",expanded:true,items:[this.fieldGrid],
        controls:[this.fieldGridAddButton,this.fieldGridRemoveButton]
    });
    var dsEditor=this;
    this.deriveForm=this.createAutoChild("deriveForm",{
        fields:[
            {name:"sql",showTitle:false,formItemType:"AutoFitTextAreaItem",
             width:"*",height:40,colSpan:"*",
             keyPress:function(item,form,keyName){
                if(keyName=='Enter'&&isc.EH.ctrlKeyDown()){
                   if(isc.Browser.isSafari)item.setValue(item.getElementValue());
                   dsEditor.execSQL();
                   if(isc.Browser.isSafari)return false;
                }
            }},
            {type:"button",title:"Execute",startRow:true,click:this.getID()+".execSQL()"}
        ]
    });
    this.dbList=this.createAutoChild("dbList");
    this.showSQLBrowserButton=this.createAutoChild("showSQLBrowserButton");
    this.addSection({
        ID:"derive",title:"Derive Fields From SQL",expanded:false,items:[this.deriveForm],
        controls:[this.dbList,this.showSQLBrowserButton]
    });
    this.fetchOperationForm=this.createAutoChild("fetchOperationForm");
    this.addSection({ID:"fetchOperation",title:"Fetch Operation",expanded:true,items:[this.fetchOperationForm]});
    this.actionBar=this.createAutoChild("actionBar");
    this.addSection({ID:"actionBar",showHeader:false,expanded:true,items:[this.actionBar]});
    this.addAutoChildren(["tryButton","saveButton"]);
    this.previewGrid=this.createAutoChild("previewGrid");
    this.addSection({ID:"preview",title:"Preview",items:[this.previewGrid]});
    this.loadDS(this.config);
}
,isc.A.execSQL=function isc_DSEditor_execSQL(){
    var sql=this.deriveForm.getValue("sql");
    if(sql){
        sql=sql.trim().replace(/(.*);+/,"$1");
        var ds=isc.DataSource.get("DataSourceStore");
        ds.performCustomOperation("dsFromSQL",{dbName:this.dbList.getSelectedDB(),sql:sql},this.getID()+".deriveDSLoaded(data)");
    }
}
,isc.A.deriveDSLoaded=function isc_DSEditor_deriveDSLoaded(data){
    var ds=data.ds;
    this.dsLoaded(data.ds);
}
,isc.A.loadDS=function isc_DSEditor_loadDS(record){
    this.currentRegistry=record;
    isc.DMI.call("isc_builtin","com.isomorphic.tools.BuiltinRPC","dsConfigFromXML",
                 record.object,
                 this.getID()+".dsLoaded(data)");
}
,isc.A.dsLoaded=function isc_DSEditor_dsLoaded(dsConfig){
    var ds=isc.DataSource.create(dsConfig);
    this.currentDS=ds;
    ds.repo=this.config.registry.ID;
    this.deriveFields(ds);
    this.previewGrid.setDataSource(ds);
    if(ds.dbName)this.dbList.setSelectedDB(ds.dbName);
    var ob=ds.operationBindings;
    if(ob&&ob.length>0){
        this.fetchOperationForm.setValues(ob[0]);
    }
}
,isc.A.deriveFields=function isc_DSEditor_deriveFields(ds){
    var fields=ds.getFieldNames();
    var newFields=[];
    for(var i=0;i<fields.length;i++){
        var fieldName=fields[i]
        var field={};
        var dsField=ds.getField(fieldName);
        for(var key in dsField){
            if(isc.isA.String(key)&&key.startsWith("_"))continue;
            field[key]=dsField[key];
        }
        newFields.add(field);
    }
    this.fieldGrid.setData(newFields);
}
,isc.A.tryIt=function isc_DSEditor_tryIt(){
    var dsConfig=this.buildDSConfig(this.config.ID+"_test");
    var ds=isc.DataSource.get("DataSourceStore");
    ds.performCustomOperation("dsFromConfig",{config:dsConfig},this.getID()+".tryItCallback(data)");
}
,isc.A.tryItCallback=function isc_DSEditor_tryItCallback(data){
    this.expandSection("preview");
    this.previewGrid.setDataSource(data.ds);
    this.previewGrid.filterData();
}
,isc.A.saveDS=function isc_DSEditor_saveDS(){
    var dsConfig=this.buildDSConfig(this.config.ID);
    var ds=isc.DataSource.get("DataSourceStore");
    ds.performCustomOperation("dsFromConfig",{config:dsConfig},this.getID()+".xmlLoaded(data)");
}
,isc.A.xmlLoaded=function isc_DSEditor_xmlLoaded(data){
    var repoDS=isc.DataSource.get(this.config.registry.ID);
    repoDS.updateData({pk:this.config.pk,object:data.dsXML});
}
,isc.A.buildDSConfig=function isc_DSEditor_buildDSConfig(ID){
    var dsConfig={
        ID:ID,
        serverType:"sql",
        dbName:this.dbList.getSelectedDB(),
        __autoConstruct:"DataSource",
        operationBindings:[
            isc.addProperties({operationType:"fetch",skipRowCount:"true",qualifyColumnNames:false},this.fetchOperationForm.getValues())
        ],
        fields:this.fieldGrid.data
    };
    return dsConfig;
}
);
isc.B._maxIndex=isc.C+11;
isc._debugModules = (isc._debugModules != null ? isc._debugModules : []);isc._debugModules.push('DSBrowser');isc.checkForDebugAndNonDebugModules();isc._moduleEnd=isc._DSBrowser_end=(isc.timestamp?isc.timestamp():new Date().getTime());if(isc.Log&&isc.Log.logIsInfoEnabled('loadTime'))isc.Log.logInfo('DSBrowser module init time: ' + (isc._moduleEnd-isc._moduleStart) + 'ms','loadTime');delete isc.definingFramework;if (isc.Page) isc.Page.handleEvent(null, "moduleLoaded", { moduleName: 'DSBrowser', loadTime: (isc._moduleEnd-isc._moduleStart)});}else{if(window.isc && isc.Log && isc.Log.logWarn)isc.Log.logWarn("Duplicate load of module 'DSBrowser'.");}

/*

  SmartClient Ajax RIA system
  Version v10.0p_2015-01-04/LGPL Deployment (2015-01-04)

  Copyright 2000 and beyond Isomorphic Software, Inc. All rights reserved.
  "SmartClient" is a trademark of Isomorphic Software, Inc.

  LICENSE NOTICE
     INSTALLATION OR USE OF THIS SOFTWARE INDICATES YOUR ACCEPTANCE OF THE
     SOFTWARE LICENSE AGREEMENT. If you have received this file without an 
     Isomorphic Software license file, please see:

         http://www.isomorphic.com/licenses/license-sisv.html

     You are not required to accept this agreement, however, nothing else
     grants you the right to copy or use this software. Unauthorized copying
     and use of this software is a violation of international copyright law.

  PROPRIETARY & PROTECTED MATERIAL
     This software contains proprietary materials that are protected by
     contract and intellectual property law. YOU ARE EXPRESSLY PROHIBITED
     FROM ATTEMPTING TO REVERSE ENGINEER THIS SOFTWARE OR MODIFY THIS
     SOFTWARE FOR HUMAN READABILITY.

  CONTACT ISOMORPHIC
     For more information regarding license rights and restrictions, or to
     report possible license violations, please contact Isomorphic Software
     by email (licensing@isomorphic.com) or web (www.isomorphic.com).

*/

