
/*

  SmartClient Ajax RIA system
  Version v10.0p_2015-01-04/LGPL Development Only (2015-01-04)

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

if(window.isc&&window.isc.module_Core&&!window.isc.module_Kapow){isc.module_Kapow=1;isc._moduleStart=isc._Kapow_start=(isc.timestamp?isc.timestamp():new Date().getTime());if(isc._moduleEnd&&(!isc.Log||(isc.Log&&isc.Log.logIsDebugEnabled('loadTime')))){isc._pTM={message:'Kapow load/parse time: '+(isc._moduleStart-isc._moduleEnd)+'ms',category:'loadTime'};if(isc.Log&&isc.Log.logDebug)isc.Log.logDebug(isc._pTM.message,'loadTime');else if(isc._preLog)isc._preLog[isc._preLog.length]=isc._pTM;else isc._preLog=[isc._pTM]}isc.definingFramework=true;isc.defineClass("RobotServerPicker","Window");isc.A=isc.RobotServerPicker.getPrototype();isc.B=isc._allFuncs;isc.C=isc.B._maxIndex;isc.D=isc._funcClasses;isc.D[isc.C]=isc.A.Class;isc.A.autoCenter=true;isc.A.autoSize=true;isc.A.isModal=true;isc.A.title="Select Robot Server";isc.A.formConstructor="DynamicForm";isc.A.formDefaults={width:300,numCols:2,colWidths:[150,"*"],defaultItems:[{name:"robotServerURL",title:"Robot Server URL",defaultValue:"http://127.0.0.1:50080"},{name:"next",type:"button",title:"Next",click:"form.creator.nextClick()",startRow:true},{name:"cancel",type:"button",title:"Cancel",click:"form.creator.hide()",endRow:false,startRow:false}]};isc.A.myAutoChildren=["form"];isc.B.push(isc.A.initWidget=function isc_RobotServerPicker_initWidget(){this.Super("initWidget",arguments);this.form=this.createAutoChild("form");this.addItem(this.form)},isc.A.nextClick=function isc_RobotServerPicker_nextClick(){var _1=this.form.getValue("robotServerURL");window.robotServerURL=_1;this.hide();this.fireCallback("robotServerSelected","robotServerURL",[_1])});isc.B._maxIndex=isc.C+2;isc.RobotServerPicker.registerStringMethods({robotServerSelected:"robotServerURL"});isc._nonDebugModules=(isc._nonDebugModules!=null?isc._nonDebugModules:[]);isc._nonDebugModules.push('Kapow');isc.checkForDebugAndNonDebugModules();isc._moduleEnd=isc._Kapow_end=(isc.timestamp?isc.timestamp():new Date().getTime());if(isc.Log&&isc.Log.logIsInfoEnabled('loadTime'))isc.Log.logInfo('Kapow module init time: '+(isc._moduleEnd-isc._moduleStart)+'ms','loadTime');delete isc.definingFramework;if(isc.Page)isc.Page.handleEvent(null,"moduleLoaded",{moduleName:'Kapow',loadTime:(isc._moduleEnd-isc._moduleStart)});}else{if(window.isc&&isc.Log&&isc.Log.logWarn)isc.Log.logWarn("Duplicate load of module 'Kapow'.");}

/*

  SmartClient Ajax RIA system
  Version v10.0p_2015-01-04/LGPL Development Only (2015-01-04)

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

