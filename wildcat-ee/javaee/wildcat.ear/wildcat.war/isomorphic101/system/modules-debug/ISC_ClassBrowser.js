
/*

  SmartClient Ajax RIA system
  Version SNAPSHOT_v10.1p_2015-12-10/LGPL Deployment (2015-12-10)

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

if(window.isc&&window.isc.module_Core&&!window.isc.module_ClassBrowser){isc.module_ClassBrowser=1;isc._moduleStart=isc._ClassBrowser_start=(isc.timestamp?isc.timestamp():new Date().getTime());if(isc._moduleEnd&&(!isc.Log||(isc.Log && isc.Log.logIsDebugEnabled('loadTime')))){isc._pTM={ message:'ClassBrowser load/parse time: ' + (isc._moduleStart-isc._moduleEnd) + 'ms', category:'loadTime'};
if(isc.Log && isc.Log.logDebug)isc.Log.logDebug(isc._pTM.message,'loadTime');
else if(isc._preLog)isc._preLog[isc._preLog.length]=isc._pTM;
else isc._preLog=[isc._pTM]}isc.definingFramework=true;isc.defineClass("JavaClassPane","VLayout");
isc.A=isc.JavaClassPane.getPrototype();
isc.B=isc._allFuncs;
isc.C=isc.B._maxIndex;
isc.D=isc._funcClasses;
isc.D[isc.C]=isc.A.Class;
isc.A.sourceViewDefaults={
    _constructor:"HTMLFlow",
    autoDraw:false,
    height:"*"
};
isc.B.push(isc.A.initWidget=function isc_JavaClassPane_initWidget(){
    this.Super("initWidget",arguments);
    this.sourceView=this.createAutoChild("sourceView",{
        contents:"Loading..."
    });
    this.addMember(this.sourceView);
    this.loadSource();
}
,isc.A.loadSource=function isc_JavaClassPane_loadSource(){
    isc.DMI.call("isc_builtin","com.isomorphic.tools.BuiltinRPC","getJavaSource",
                 this.config.path,
                 this.getID()+".loadSourceReply(data)");
}
,isc.A.loadSourceReply=function isc_JavaClassPane_loadSourceReply(data){
    var sh=isc.JSSyntaxHiliter.create();
    this.sourceView.setContents(sh.hilite(data));
}
);
isc.B._maxIndex=isc.C+3;
isc.DataSource.create({
    operationBindings:[
        {
            operationType:"fetch"
        }
    ],
    allowAdvancedCriteria:true,
    ID:"JVMClassTreeDS",
    fields:[
        {
            name:"name",
            validators:[
            ]
        },
        {
            name:"path",
            validators:[
            ],
            primaryKey:true
        },
        {
            name:"parentID",
            hidden:true,
            foreignKey:"JVMClassTreeDS.path",
            validators:[
            ]
        },
        {
            name:"isFolder",
            type:"boolean",
            validators:[
            ]
        }
    ]
})
isc.defineClass("JVMClassTree","TreeGrid");
isc.A=isc.JVMClassTree.getPrototype();
isc.A.dataSource="JVMClassTreeDS";
isc.A.animateFolders=false
;

isc.JVMClassTree.registerStringMethods({
});
isc.defineClass("ClassBrowser","VLayout");
isc.A=isc.ClassBrowser;
isc.B=isc._allFuncs;
isc.C=isc.B._maxIndex;
isc.D=isc._funcClasses;
isc.D[isc.C]=isc.A.Class;
isc.B.push(isc.A.showWindow=function isc_c_ClassBrowser_showWindow(windowProps,props){
    isc.Window.create({
        title:"Class Browser",
        width:"100%",
        height:"100%",
        canDragReposition:false,
        closeClick:function(){this.destroy();},
        items:[
            isc.ClassBrowser.create({autoDraw:false},props)
        ]
    },windowProps).show();
}
);
isc.B._maxIndex=isc.C+1;

isc.A=isc.ClassBrowser.getPrototype();
isc.B=isc._allFuncs;
isc.C=isc.B._maxIndex;
isc.D=isc._funcClasses;
isc.D[isc.C]=isc.A.Class;
isc.A.classTreeDefaults={
    _constructor:"JVMClassTree",
    autoDraw:false,
    autoFetchData:true,
    recordDoubleClick:function(viewer,record){
        if(this.data.isLeaf(record))this.creator.showClassPane(record);
    }
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
             contents:"Select a class on the left..."
         })
        }
    ]
};
isc.A.autoChildren=["mainLayout"];
isc.A.classPaneDefaults={
    _constructor:"JavaClassPane"
};
isc.B.push(isc.A.initWidget=function isc_ClassBrowser_initWidget(){
    this.Super("initWidget",arguments);
    this.classTree=this.createAutoChild("classTree",{
        selectionChanged:"if (state) this.creator.classChanged(record)"
    });
    this.leftSection=this.createAutoChild("leftSection",{
        sections:[
            {name:"classes",title:"Classes",expanded:true,controls:[],items:[
                this.classTree
            ]}
        ]
    });
    this.addAutoChildren(this.autoChildren);
    this.mainLayout.addMember(this.leftSection);
    this.rightPane=this.createAutoChild("rightPane");
    this.mainLayout.addMember(this.rightPane);
}
,isc.A.classChanged=function isc_ClassBrowser_classChanged(record){
    this.showClassPane(record);
}
,isc.A.showClassPane=function isc_ClassBrowser_showClassPane(record){
    var tabId="class_"+this.escapeForId(record.path);
    this.showPane({ID:tabId,title:"Class: "+record.name,paneClass:"classPane"},record);
}
,isc.A.escapeForId=function isc_ClassBrowser_escapeForId(s){
    return isc.isA.String(s)?s.replace(/(\/|\.)/g,'_'):s;
}
,isc.A.showPane=function isc_ClassBrowser_showPane(props,childConfig){
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
isc.B._maxIndex=isc.C+5;
isc._debugModules = (isc._debugModules != null ? isc._debugModules : []);isc._debugModules.push('ClassBrowser');isc.checkForDebugAndNonDebugModules();isc._moduleEnd=isc._ClassBrowser_end=(isc.timestamp?isc.timestamp():new Date().getTime());if(isc.Log&&isc.Log.logIsInfoEnabled('loadTime'))isc.Log.logInfo('ClassBrowser module init time: ' + (isc._moduleEnd-isc._moduleStart) + 'ms','loadTime');delete isc.definingFramework;if (isc.Page) isc.Page.handleEvent(null, "moduleLoaded", { moduleName: 'ClassBrowser', loadTime: (isc._moduleEnd-isc._moduleStart)});}else{if(window.isc && isc.Log && isc.Log.logWarn)isc.Log.logWarn("Duplicate load of module 'ClassBrowser'.");}

/*

  SmartClient Ajax RIA system
  Version SNAPSHOT_v10.1p_2015-12-10/LGPL Deployment (2015-12-10)

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

