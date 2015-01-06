
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

if(window.isc&&window.isc.module_Core&&!window.isc.module_Scheduler){isc.module_Scheduler=1;isc._moduleStart=isc._Scheduler_start=(isc.timestamp?isc.timestamp():new Date().getTime());if(isc._moduleEnd&&(!isc.Log||(isc.Log&&isc.Log.logIsDebugEnabled('loadTime')))){isc._pTM={message:'Scheduler load/parse time: '+(isc._moduleStart-isc._moduleEnd)+'ms',category:'loadTime'};if(isc.Log&&isc.Log.logDebug)isc.Log.logDebug(isc._pTM.message,'loadTime');else if(isc._preLog)isc._preLog[isc._preLog.length]=isc._pTM;else isc._preLog=[isc._pTM]}isc.definingFramework=true;isc.DataSource.create({allowAdvancedCriteria:true,ID:"QuartzScheduler",operationBindings:[{operationType:"custom",operationId:"start"},{operationType:"custom",operationId:"shutdown"},{operationType:"custom",operationId:"standby"},{operationType:"custom",operationId:"doit"}],fields:[{name:"name",type:"text",canEdit:false},{valueMap:{"0":"Shutdown","1":"Standby","2":"Started"},name:"state",type:"intEnum",canEdit:false}]})
isc.DataSource.create({allowAdvancedCriteria:true,ID:"QuartzJobs",fields:[{primaryKey:true,name:"group",type:"string",required:true},{primaryKey:true,name:"name",type:"string",required:true},{name:"description",type:"string"},{name:"className",type:"string",required:true},{name:"volatility",type:"boolean",defaultValue:"false"},{name:"durability",type:"boolean",defaultValue:"true"},{name:"recover",type:"boolean",defaultValue:"true"},{name:"dataMap",showIf:"false",type:"Object"}]})
isc.DataSource.create({allowAdvancedCriteria:true,ID:"QuartzTriggers",fields:[{name:"jobGroup",showIf:"false",type:"string",required:true},{name:"jobName",showIf:"false",type:"string",required:true},{primaryKey:true,name:"group",type:"string",required:true},{primaryKey:true,name:"name",type:"string",required:true},{name:"description",type:"string"},{name:"dataMap",showIf:"false",type:"Object"},{name:"startTime",type:"date"},{name:"endTime",type:"date"},{name:"cronExpression",type:"text",required:true},{name:"timeZone",type:"text"},{name:"volatility",type:"boolean",defaultValue:"false"},{valueMap:{"0":"MISFIRE_INSTRUCTION_SMART_POLICY","1":"MISFIRE_INSTRUCTION_FIRE_ONCE_NOW","2":"MISFIRE_INSTRUCTION_DO_NOTHING"},name:"misfireInstruction",type:"intEnum",defaultValue:"0"},{valueMap:{"0":"Normal","1":"Paused","2":"Complete","3":"Error","4":"Blocked","-1":"None"},name:"state",type:"intEnum",canEdit:false}]})
isc.defineClass("QuartzManager","SectionStack");isc.A=isc.QuartzManager.getPrototype();isc.B=isc._allFuncs;isc.C=isc.B._maxIndex;isc.D=isc._funcClasses;isc.D[isc.C]=isc.A.Class;isc.A.visibilityMode="multiple";isc.A.jobsPauseBtnDefaults={_constructor:"IButton",title:"Pause Job",prompt:"Suspends all triggers associated with selected job",click:function(){var _1=this.creator.jobsGrid;if(!_1.anySelected()){isc.say("Please select a job first");return}
var _2=_1.getSelectedRecord();var _3=this;QuartzJobs.performCustomOperation("pauseJob",{group:_2.group,name:_2.name},function(_4){_3.creator.triggersGrid.invalidateCache();isc.say('Job Paused')})}};isc.A.jobsResumeBtnDefaults={_constructor:"IButton",title:"Resume Job",prompt:"Resumes all triggers associated with selected job",click:function(){var _1=this.creator.jobsGrid;if(!_1.anySelected()){isc.say("Please select a job first");return}
var _2=_1.getSelectedRecord();var _3=this;QuartzJobs.performCustomOperation("resumeJob",{group:_2.group,name:_2.name},function(_4){_3.creator.triggersGrid.invalidateCache();isc.say('Job Resumed')})}};isc.A.jobsTriggerBtnDefaults={_constructor:"IButton",title:"Trigger Job",prompt:"Triggers selected job immediately",click:function(){var _1=this.creator.jobsGrid;if(!_1.anySelected()){isc.say("Please select a job first");return}
var _2=_1.getSelectedRecord();QuartzJobs.performCustomOperation("triggerJob",{group:_2.group,name:_2.name},function(_3){isc.say('Job Triggered')})}};isc.A.jobsRefreshBtnDefaults={_constructor:"ImgButton",showRollOver:false,size:16,src:"[SKIN]actions/refresh.png",prompt:"Refresh jobs",click:function(){this.creator.jobsGrid.invalidateCache();this.creator.triggersGrid.setData([])}};isc.A.jobsAddBtnDefaults={_constructor:"ImgButton",size:16,showRollOver:false,src:"[SKIN]actions/add.png",prompt:"Add job",click:"this.creator.jobsGrid.startEditingNew()"};isc.A.jobsRemoveBtnDefaults={_constructor:"ImgButton",size:16,showRollOver:false,src:"[SKIN]actions/remove.png",prompt:"Remove job",click:function(){var _1=this;isc.ask("Are you sure you wish to delete the selected job?  This will remove all triggers associated with this job.",function(_2){if(_2)_1.creator.jobsGrid.removeSelectedData(function(_3){_1.creator.triggersGrid.setData([])})})}};isc.A.jobsGridDefaults={_constructor:"ListGrid",autoDraw:false,height:"30%",dataSource:"QuartzJobs",useAllDataSourceFields:true,autoFetchData:true,selectionType:"single",recordDoubleClick:function(){isc.say("The Quartz APIs do not allow modification of job metadata without destroying all triggers attached to the job, so you must remove and re-create the job if that's your intention");return},selectionChanged:function(_1,_2){if(_2){this.creator.triggersGrid.filterData({jobGroup:_1.group,jobName:_1.name})}else{this.creator.triggersGrid.setData([])}},remove:function(){}};isc.A.triggersPauseBtnDefaults={_constructor:"IButton",title:"Pause Trigger",prompt:"Suspends selected trigger",click:function(){var _1=this.creator.triggersGrid;if(!_1.anySelected()){isc.say("Please select a trigger first");return}
var _2=_1.getSelectedRecord();QuartzTriggers.performCustomOperation("pauseTrigger",{group:_2.group,name:_2.name},function(_3){_1.invalidateCache();isc.say('Trigger Paused')})}};isc.A.triggersResumeBtnDefaults={_constructor:"IButton",title:"Resume Trigger",prompt:"Resumes selected trigger",click:function(){var _1=this.creator.triggersGrid;if(!_1.anySelected()){isc.say("Please select a trigger first");return}
var _2=_1.getSelectedRecord();QuartzTriggers.performCustomOperation("resumeTrigger",{group:_2.group,name:_2.name},function(_3){_1.invalidateCache();isc.say('Trigger Resumed')})}};isc.A.triggersRefreshBtnDefaults={_constructor:"ImgButton",showRollOver:false,size:16,src:"[SKIN]actions/refresh.png",prompt:"Refresh jobs",click:"this.creator.triggersGrid.invalidateCache()"};isc.A.triggersAddBtnDefaults={_constructor:"ImgButton",size:16,showRollOver:false,src:"[SKIN]actions/add.png",prompt:"Add trigger",click:function(){var _1=this.creator.jobsGrid;if(!_1.anySelected()){isc.say("Please select a job first");return}
var _2=_1.getSelectedRecord();this.creator.triggersGrid.startEditingNew({jobGroup:_2.group,jobName:_2.name})}};isc.A.triggersRemoveBtnDefaults={_constructor:"ImgButton",size:16,showRollOver:false,src:"[SKIN]actions/remove.png",prompt:"Remove job",click:function(){var _1=this;isc.ask("Are you sure you wish to remove the selected trigger?",function(_2){if(_2)_1.creator.jobsGrid.removeSelectedData(function(_3){_1.creator.triggersGrid.invalidateCache()})})}};isc.A.triggersGridDefaults={_constructor:"ListGrid",canEdit:true,autoDraw:false,dataSource:"QuartzTriggers",useAllDataSourceFields:true,selectionType:"single",remove:function(){}};isc.B.push(isc.A.initWidget=function isc_QuartzManager_initWidget(){this.Super("initWidget",arguments);this.jobsPauseBtn=this.createAutoChild("jobsPauseBtn");this.jobsResumeBtn=this.createAutoChild("jobsResumeBtn");this.jobsTriggerBtn=this.createAutoChild("jobsTriggerBtn");this.jobsRefreshBtn=this.createAutoChild("jobsRefreshBtn");this.jobsAddBtn=this.createAutoChild("jobsAddBtn");this.jobsRemoveBtn=this.createAutoChild("jobsRemoveBtn");this.jobsGrid=this.createAutoChild("jobsGrid");this.addSection({title:"Jobs",expanded:true,items:[this.jobsGrid],controls:[this.jobsPauseBtn,this.jobsResumeBtn,this.jobsTriggerBtn,this.jobsRefreshBtn,this.jobsAddBtn,this.jobsRemoveBtn]});;this.triggersPauseBtn=this.createAutoChild("triggersPauseBtn");this.triggersResumeBtn=this.createAutoChild("triggersResumeBtn");this.triggersRefreshBtn=this.createAutoChild("triggersRefreshBtn");this.triggersAddBtn=this.createAutoChild("triggersAddBtn");this.triggersRemoveBtn=this.createAutoChild("triggersRemoveBtn");this.triggersGrid=this.createAutoChild("triggersGrid");this.addSection({title:"Triggers",expanded:true,items:[this.triggersGrid],controls:[this.triggersPauseBtn,this.triggersResumeBtn,this.triggersRefreshBtn,this.triggersAddBtn,this.triggersRemoveBtn]});});isc.B._maxIndex=isc.C+1;isc._nonDebugModules=(isc._nonDebugModules!=null?isc._nonDebugModules:[]);isc._nonDebugModules.push('Scheduler');isc.checkForDebugAndNonDebugModules();isc._moduleEnd=isc._Scheduler_end=(isc.timestamp?isc.timestamp():new Date().getTime());if(isc.Log&&isc.Log.logIsInfoEnabled('loadTime'))isc.Log.logInfo('Scheduler module init time: '+(isc._moduleEnd-isc._moduleStart)+'ms','loadTime');delete isc.definingFramework;if(isc.Page)isc.Page.handleEvent(null,"moduleLoaded",{moduleName:'Scheduler',loadTime:(isc._moduleEnd-isc._moduleStart)});}else{if(window.isc&&isc.Log&&isc.Log.logWarn)isc.Log.logWarn("Duplicate load of module 'Scheduler'.");}

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

