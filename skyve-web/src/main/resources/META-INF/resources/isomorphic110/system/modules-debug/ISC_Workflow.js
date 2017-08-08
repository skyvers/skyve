
/*

  SmartClient Ajax RIA system
  Version v11.0p_2016-03-31/LGPL Deployment (2016-03-31)

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

if(window.isc&&window.isc.module_Core&&!window.isc.module_Workflow){isc.module_Workflow=1;isc._moduleStart=isc._Workflow_start=(isc.timestamp?isc.timestamp():new Date().getTime());if(isc._moduleEnd&&(!isc.Log||(isc.Log && isc.Log.logIsDebugEnabled('loadTime')))){isc._pTM={ message:'Workflow load/parse time: ' + (isc._moduleStart-isc._moduleEnd) + 'ms', category:'loadTime'};
if(isc.Log && isc.Log.logDebug)isc.Log.logDebug(isc._pTM.message,'loadTime');
else if(isc._preLog)isc._preLog[isc._preLog.length]=isc._pTM;
else isc._preLog=[isc._pTM]}isc.definingFramework=true;isc.defineClass("ProcessElement");
isc.ProcessElement.addProperties({
})
isc.defineClass("ProcessSequence","ProcessElement");
isc.ProcessSequence.addProperties({
})
isc.defineClass("Task","ProcessElement");
isc.Task.addProperties({
})
isc.defineClass("Process","Task");
isc.A=isc.Process;
isc.B=isc._allFuncs;
isc.C=isc.B._maxIndex;
isc.D=isc._funcClasses;
isc.D[isc.C]=isc.A.Class;
isc.A._cache={};
isc.B.push(isc.A.loadProcess=function isc_c_Process_loadProcess(processId,callback){
        var ds=isc.DataSource.get("WorkflowLoader");
        ds.fetchData({id:processId},function(response,data,request){
            var process=null;
            var content=data.content;
            if(content!=null){
                if(isc.isAn.Array(content)){
                    process=isc.Class.evaluate(content[0]);
                    process.ID=processId[0];
                    isc.Process._cache[processId[0]]=process;
                    for(var i=1;i<content.length;i++){
                        var p=isc.Class.evaluate(content[i]);
                        p.ID=processId[i];
                        isc.Process._cache[processId[i]]=p;
                    }
                }else{
                    process=isc.Class.evaluate(content);
                    process.ID=processId;
                    isc.Process._cache[processId]=process;
                }
            }else{
                isc.logWarn("File named \""+processId+"\".proc.xml could not "+
                    "be found in the search path specified by \"project.processes\".")
            }
            callback(process);
        });
    }
,isc.A.getProcess=function isc_c_Process_getProcess(processId){
        return isc.Process._cache[processId];
    }
);
isc.B._maxIndex=isc.C+2;

isc.A=isc.Process.getPrototype();
isc.B=isc._allFuncs;
isc.C=isc.B._maxIndex;
isc.D=isc._funcClasses;
isc.D[isc.C]=isc.A.Class;
isc.A.autoStart=false;
isc.B.push(isc.A.init=function isc_Process_init(){
        var res=this.Super("init",arguments);
        this._initStartElement=this.startElement;
        if(this.autoStart)this.start();
        return res;
    }
,isc.A.getElement=function isc_Process_getElement(ID){
        return this._searchElement(this,ID);
    }
,isc.A.setState=function isc_Process_setState(state){
        this.state=state;
    }
,isc.A._searchElement=function isc_Process__searchElement(sequence,ID){
        if(sequence.sequences){
            for(var i=0;i<sequence.sequences.length;i++){
                var s=sequence.sequences[i];
                if(s.ID==ID){
                    return s;
                }else if(s.sequences||s.elements){
                    var res=this._searchElement(s,ID);
                    if(res)return res;
                }
            }
        }
        if(sequence.elements){
            for(var i=0;i<sequence.elements.length;i++){
                var e=sequence.elements[i];
                if(e.ID==ID){
                    return e;
                }else if(e.sequences||e.elements){
                    var res=this._searchElement(e,ID);
                    if(res)return res;
                }
            }
        }
    }
,isc.A.start=function isc_Process_start(){
        if(this.executionStack==null){
            this.executionStack=[];
        }
        if(this.state==null)this.state={};
        while(this._next()){
            var currentTask=this._getFirstTask();
            if(currentTask){
                this._started=true;
                if(!currentTask.executeElement(this)){
                    return;
                }
            }
        }
        if(this.finished){
            this.startElement=this._initStartElement;
            this.finished(this.state);
        }
    }
,isc.A.reset=function isc_Process_reset(state){
        this.state=state;
        this.executionStack=null;
    }
,isc.A._next=function isc_Process__next(){
        var currEl=this.executionStack.last();
        if(currEl==null){
            if(this.startElement){
                var nextEl=this._gotoElement(this,this.startElement);
                if(nextEl==null){
                    isc.logWarn("unable to find task '"+this.startElement+"' - process will be finished");
                }
                return nextEl;
            }else if(this._started){
                return null;
            }else if(this.sequences&&this.sequences.length>0){
                this.executionStack.add({el:this,sIndex:0});
                return this.sequences[0];
            }else if(this.elements&&this.elements.length>0){
                this.executionStack.add({el:this,eIndex:0});
                return this.elements[0];
            }else{
                isc.logWarn("There are neither sequences or elements. Nothing to execute.");
            }
        }else{
            var el=null;
            if(currEl.sIndex!=null){
                el=currEl.el.sequences[currEl.sIndex];
            }else if(currEl.eIndex!=null){
                el=currEl.el.elements[currEl.eIndex];
            }
            if(el.nextElement){
                this.executionStack=[];
                var nextEl=this._gotoElement(this,el.nextElement);
                if(nextEl==null){
                    isc.logWarn("unable to find task '"+el.nextElement+"' - process will be finished");
                }
                return nextEl;
            }else{
                return this._findNextElement();
            }
        }
    }
,isc.A._gotoElement=function isc_Process__gotoElement(sequence,ID){
        var elData={el:sequence};
        this.executionStack.add(elData);
        if(sequence.sequences){
            for(var i=0;i<sequence.sequences.length;i++){
                var s=sequence.sequences[i];
                elData.sIndex=i;
                if(s.ID==ID){
                    return s;
                }else if(s.sequences||s.elements){
                    var res=this._gotoElement(s,ID);
                    if(res)return res;
                }
            }
        }
        delete elData.sIndex;
        if(sequence.elements){
            for(var i=0;i<sequence.elements.length;i++){
                var e=sequence.elements[i];
                elData.eIndex=i;
                if(e.ID==ID){
                    return e;
                }else if(e.sequences||e.elements){
                    var res=this._gotoElement(e,ID);
                    if(res)return res;
                }
            }
        }
        this.executionStack.removeAt(this.executionStack.length-1);
    }
,isc.A._findNextElement=function isc_Process__findNextElement(){
        var elData=this.executionStack.last();
        if(elData.eIndex!=null&&elData.el!=this){
            if(elData.eIndex==elData.el.elements.length-1){
                this.executionStack.removeAt(this.executionStack.length-1);
                if(elData.el==this){
                    return;
                }else{
                    return this._findNextElement();
                }
            }else{
                elData.eIndex++;
                return elData.el.elements[elData.eIndex];
            }
        }
    }
,isc.A._getFirstTask=function isc_Process__getFirstTask(){
        var lastElData=this.executionStack.last();
        var el=null;
        if(lastElData.sIndex!=null){
            el=lastElData.el.sequences[lastElData.sIndex];
        }else if(lastElData.eIndex!=null){
            el=lastElData.el.elements[lastElData.eIndex];
        }
        if(el.sequences==null&&el.elements==null){
            return el;
        }
        var elData={el:el};
        this.executionStack.add(elData);
        if(el.sequences){
            for(var i=0;i<el.sequences.length;i++){
                elData.sIndex=i
                var res=this._getFirstTask(el.sequences[i]);
                if(res)return res;
            }
        }
        if(el.elements){
            for(var i=0;i<el.elements.length;i++){
                elData.eIndex=i
                var res=this._getFirstTask(el.elements[i]);
                if(res)return res;
            }
        }
        this.executionStack.removeAt(this.executionStack.length-1);
    }
,isc.A.setNextElement=function isc_Process_setNextElement(nextElement){
        this.executionStack=[];
        this.startElement=nextElement;
    }
,isc.A.setStateVariable=function isc_Process_setStateVariable(stateVariablePath,value){
        if(stateVariablePath.indexOf(".")<0||this.state[stateVariablePath]){
            this.state[stateVariablePath]=value;
        }else{
            var segments=stateVariablePath.split(".");
            var obj=this.state;
            for(var i=0;i<segments.length-1;i++){
                var nextObj=obj[segments[i]];
                if(nextObj==null){
                    obj[segments[i]]={}
                    nextObj=obj[segments[i]];
                }
                obj=nextObj;
            }
            obj[segments[i]]=value;
        }
    }
,isc.A.getStateVariable=function isc_Process_getStateVariable(stateVariablePath){
        if(stateVariablePath.indexOf(".")<0||this.state[stateVariablePath]){
            return this.state[stateVariablePath];
        }else{
            var segments=stateVariablePath.split(".");
            var obj=this.state;
            for(var i=0;i<segments.length-1;i++){
                obj=obj[segments[i]];
                if(obj==null){
                    isc.logWarn("Unable to get state variable: "+stateVariablePath+" no such path")
                    return;
                }
            }
            return obj[segments[i]]
        }
    }
,isc.A.setState=function isc_Process_setState(newState){
        this.state=newState;
    }
);
isc.B._maxIndex=isc.C+14;

isc.Process.registerStringMethods({
    finished:"state"
});
isc.defineClass("ServiceTask","Task");
isc.A=isc.ServiceTask.getPrototype();
isc.B=isc._allFuncs;
isc.C=isc.B._maxIndex;
isc.D=isc._funcClasses;
isc.D[isc.C]=isc.A.Class;
isc.A.operationType="fetch";
isc.B.push(isc.A.executeElement=function isc_ServiceTask_executeElement(process){
        var ds=this.dataSource;
        if(ds.getClassName==null||ds.getClassName()!="DataSource"){
            ds=isc.DataSource.get(ds);
        }
        var inputData={};
        if(this.inputFieldList){
            for(var i=0;i<this.inputFieldList.length;i++){
                var key=this.inputFieldList[i];
                var ldi=key.lastIndexOf(".");
                if(ldi>0){
                    key=key.substring(ldi+1);
                }
                inputData[key]=process.getStateVariable(this.inputFieldList[i]);
            };
        }
        if(this.inputField){
            var key=this.inputField;
            var ldi=key.lastIndexOf(".");
            if(ldi>0){
                key=key.substring(ldi+1);
            }
            inputData[key]=process.getStateVariable(this.inputField.replace("$",""));
            if(this.inputField.startsWith("$")){
                inputData=inputData[key];
            }
        }
        var data=null;
        if(this.operationType=="fetch"){
            if(this.criteria){
                data=this.criteria;
                this._processCriteriaExpressions(data,inputData);
            }
            if(this.fixedCriteria){
                if(data==null&&inputData==null){
                    data=this.fixedCriteria
                }else{
                    var crit=isc.clone(this.fixedCriteria);
                    if(inputData){
                        crit=isc.DataSource.combineCriteria(inputData,crit);
                    }
                    if(data){
                        crit=isc.DataSource.combineCriteria(data,crit);
                    }
                    data=crit;
                }
            }
        }
        if(data==null){
            data=inputData;
        }
        if(this.operationType!="fetch"){
            if(this.values){
                for(var key in this.values){
                    data[key]=this.values[key];
                    if(isc.isA.String(data[key])){
                        if(data[key].startsWith("$input")){
                            var script="state."+data[key].replace("$input",this.inputField);
                            data[key]=isc.Class.evaluate(script,{state:inputData});
                        }else if(data[key].startsWith("$inputRecord")){
                            var script=data[key].replace("$inputRecord","state");
                            data[key]=isc.Class.evaluate(script,{state:inputData});
                        }
                    }
                }
            }
            if(this.fixedValues){
                for(var key in this.fixedValues){
                    data[key]=this.fixedValues[key];
                }
            }
        }
        var task=this;
        ds.performDSOperation(this.operationType,data,function(dsResponse,data){
            if(!isc.isAn.Array(data))data=[data];
            if(data.length>0){
                var fieldsToProceed=[];
                if(task.outputFieldList){
                    fieldsToProceed.addList(task.outputFieldList);
                }
                if(task.outputField)fieldsToProceed.add(task.outputField);
                for(var i=0;i<fieldsToProceed.length;i++){
                    var fieldName=fieldsToProceed[i];
                    if(fieldName.startsWith("$")){
                        var value=data.length==1?data[0]:data;
                        fieldName=fieldName.substring(1);
                        process.setStateVariable(fieldName,value);
                    }else{
                        var key=fieldName;
                        var ldi=key.lastIndexOf(".");
                        if(ldi>0){
                            key=key.substring(ldi+1);
                        }
                        var value=data[0][key];
                        if(typeof value!='undefined'){
                            if(data.length>1){
                                value=[value];
                                for(var i=1;i<data.length;i++){
                                  value.add(data[i][key])
                                }
                            }
                            process.setStateVariable(fieldName,value);
                        }
                    }
                };
            }
            process.start();
        });
        return false;
    }
,isc.A._processCriteriaExpressions=function isc_ServiceTask__processCriteriaExpressions(criteria,inputData){
        for(var name in criteria){
            if(isc.isAn.Array(criteria[name])){
                for(var i=0;i<criteria[name].length;i++){
                    this._processCriteriaExpressions(criteria[name][i],inputData);
                }
            }else if(name=="criteria"){
                this._processCriteriaExpressions(criteria.criteria,inputData);
            }else if(isc.isA.String(criteria[name])){
                if(criteria[name].startsWith("$input")){
                    var script="state."+criteria[name].replace("$input",this.inputField);
                    criteria[name]=isc.Class.evaluate(script,{state:inputData});
                }else if(criteria[name].startsWith("$inputRecord")){
                    var script=criteria[name].replace("$inputRecord","state");
                    criteria[name]=isc.Class.evaluate(script,{state:inputData});
                }
            }
        }
    }
);
isc.B._maxIndex=isc.C+2;

isc.defineClass("ScriptTask","Task");
isc.A=isc.ScriptTask.getPrototype();
isc.B=isc._allFuncs;
isc.C=isc.B._maxIndex;
isc.D=isc._funcClasses;
isc.D[isc.C]=isc.A.Class;
isc.A.isAsync=false;
isc.B.push(isc.A.getInputData=function isc_ScriptTask_getInputData(){
        return this.inputData;
    }
,isc.A.setOutputData=function isc_ScriptTask_setOutputData(taskOutput){
        this._finishTask(this.process,null,taskOutput);
    }
,isc.A.getInputRecord=function isc_ScriptTask_getInputRecord(){
        return this.inputRecord;
    }
,isc.A.setOutputRecord=function isc_ScriptTask_setOutputRecord(outputRecord){
        this._finishTask(this.process,outputRecord);
    }
,isc.A.executeElement=function isc_ScriptTask_executeElement(process){
        var inputData;
        var inputRecord;
        if(this.inputFieldList){
            inputRecord={};
            for(var i=0;i<this.inputFieldList.length;i++){
                inputRecord[this.inputFieldList[i]]=process.getStateVariable(this.inputFieldList[i]);
            };
        }
        if(this.inputField){
            inputData=process.getStateVariable(this.inputField);
            if(inputRecord){
                inputRecord[this.inputField]=inputData;
            }
        }
        this.inputData=inputData;
        this.inputRecord=inputRecord;
        this.process=process;
        try{
            var output=this.execute(inputData,inputRecord);
        }catch(e){
            isc.logWarn("Error while executing ScriptTask: "+e.toString());
        }
        if(this.isAsync){
            return false;
        }
        if(typeof output=='undefined'){
            return true;
        }
        this._processTaskOutput(process,output);
        return true;
    }
,isc.A._processTaskOutput=function isc_ScriptTask__processTaskOutput(process,output){
        if(this.outputFieldList){
            for(var i=0;i<this.outputFieldList.length;i++){
                var fieldName=this.outputFieldList[i];
                if(typeof output[fieldName]!='undefined'){
                    process.setStateVariable(fieldName,output[fieldName]);
                }
            };
        }
        if(this.outputField){
            if(this.outputFieldList==null){
                if(typeof output!='undefined'){
                    process.setStateVariable(this.outputField,output);
                }
            }else{
                if(typeof output[this.outputField]!='undefined'){
                    process.setStateVariable(this.outputField,output[this.outputField]);
                }
            }
        }
    }
,isc.A._finishTask=function isc_ScriptTask__finishTask(process,outputRecord,outputData){
        if(outputRecord==null){
            this._processTaskOutput(process,outputData);
        }else{
            if(outputData){
                outputRecord[this.outputField]=outputData;
            }
            this._processTaskOutput(process,outputRecord);
        }
        if(this.isAsync){
            process.start();
        }
    }
);
isc.B._maxIndex=isc.C+7;

isc.ScriptTask.registerStringMethods({
    execute:"input,inputRecord"
});
isc.defineClass("XORGateway","ProcessElement");
isc.A=isc.XORGateway;
isc.B=isc._allFuncs;
isc.C=isc.B._maxIndex;
isc.D=isc._funcClasses;
isc.D[isc.C]=isc.A.Class;
isc.B.push(isc.A._processFieldsRecursivelyValuesOnly=function isc_c_XORGateway__processFieldsRecursivelyValuesOnly(criteria){
        var dsFields=[];
        if(criteria.fieldName){
            if(!dsFields.contains(criteria.fieldName)){
                dsFields.add(criteria.fieldName);
            }
        }else if(criteria.criteria){
            for(var i=0;i<criteria.criteria.length;i++){
                var fs=this._processFieldsRecursivelyValuesOnly(criteria.criteria[i]);
                for(var j=0;j<fs.length;j++){
                    if(!dsFields.contains(fs[j])){
                        dsFields.add(fs[j]);
                    }
                }
            }
        }else{
            for(var key in criteria){
                if(!dsFields.contains(key)){
                    dsFields.add(key);
                }
            }
        }
        return dsFields
    }
,isc.A._processFieldsRecursively=function isc_c_XORGateway__processFieldsRecursively(criteria){
        var res=[];
        var dsFields=isc.XORGateway._processFieldsRecursivelyValuesOnly(criteria);
        for(var i=0;i<dsFields.length;i++){
            res.add({name:dsFields[i]});
        }
        return res;
    }
);
isc.B._maxIndex=isc.C+2;

isc.A=isc.XORGateway.getPrototype();
isc.B=isc._allFuncs;
isc.C=isc.B._maxIndex;
isc.D=isc._funcClasses;
isc.D[isc.C]=isc.A.Class;
isc.B.push(isc.A.executeElement=function isc_XORGateway_executeElement(process){
        var dsFields=isc.XORGateway._processFieldsRecursively(this.criteria);
        var ds=isc.DataSource.create({
            fields:dsFields
        });
        if(ds.applyFilter([process.state],this.criteria).length==1){
            if(this.nextElement)process.setNextElement(this.nextElement);
        }else{
            if(this.failureElement)process.setNextElement(this.failureElement);
        }
        return true;
    }
);
isc.B._maxIndex=isc.C+1;

isc.defineClass("DecisionGateway","ProcessElement");
isc.A=isc.DecisionGateway.getPrototype();
isc.B=isc._allFuncs;
isc.C=isc.B._maxIndex;
isc.D=isc._funcClasses;
isc.D[isc.C]=isc.A.Class;
isc.B.push(isc.A.executeElement=function isc_DecisionGateway_executeElement(process){
        for(var key in this.criteriaMap){
            var dsFields=isc.XORGateway._processFieldsRecursively(this.criteriaMap[key]);
            var ds=isc.DataSource.create({
                fields:dsFields
            });
            if(ds.applyFilter([process.state],this.criteriaMap[key]).length==1){
                process.setNextElement(key);
                return true;
            }
        }
        if(this.defaultElement)process.setNextElement(this.defaultElement);
        return true;
    }
);
isc.B._maxIndex=isc.C+1;

isc.defineClass("UserTask","Task");
isc.A=isc.UserTask.getPrototype();
isc.B=isc._allFuncs;
isc.C=isc.B._maxIndex;
isc.D=isc._funcClasses;
isc.D[isc.C]=isc.A.Class;
isc.B.push(isc.A.goToPrevious=function isc_UserTask_goToPrevious(){
        if(this.previousElement==null){
            isc.logWarn("PreviousElement is not set - unable to accomplish goToPrevious method.");
            return;
        }
        this.process.setNextElement(this.previousElement);
        this.completeEditing();
    }
,isc.A.cancelEditing=function isc_UserTask_cancelEditing(){
        if(this.process){
            if(this.wizard||this.process.wizard){
                if(this.targetFormValue){
                    this.targetFormValue.hide();
                }
            }
            var process=this.process
            delete this.process;
            process.setNextElement(this.cancelElement);
            process.start();
        }
    }
,isc.A.completeEditing=function isc_UserTask_completeEditing(){
        if(this.process){
            if(this.wizard||this.process.wizard){
                if(this.targetFormValue){
                    this.targetFormValue.hide();
                }
            }
            var values;
            if(this.targetVMValue){
                values=this.targetVMValue.getValues();
            }else if(this.targetFormValue){
                values=this.targetFormValue.getValues();
            }
            var process=this.process;
            delete this.process;
            if(this.outputField){
                process.setStateVariable(this.outputField,values);
            }else if(this.outputFieldList){
                for(var i=0;i<this.outputFieldList.length;i++){
                    var key=this.outputFieldList[i];
                    var ldi=key.lastIndexOf(".");
                    if(ldi>0){
                        key=key.substring(ldi+1);
                    }
                    var value=values[key];
                    if(value)process.setStateVariable(this.outputFieldList[i],value);
                }
            }else{
                process.setStateVariable(this.inputField,values);
            }
            process.start();
        }
    }
,isc.A.executeElement=function isc_UserTask_executeElement(process){
        this.process=process;
        if(this.targetView&&isc.isA.String(this.targetView)){
            if(process.getStateVariable(this.targetView)){
                this.targetViewValue=process.getStateVariable(this.targetView);
            }else{
                this.targetViewValue=window[this.targetView];
                if(this.targetViewValue==null&&process.views){
                    for(var i=0;i<process.views.length;i++){
                        if(process.views[i].ID==this.targetView){
                            this.targetViewValue=isc[process.views[i]._constructor].create(process.views[i]);
                            if(this.process.containerId){
                                window[this.process.containerId].addMember(this.targetViewValue);
                            }
                            break;
                        }
                    }
                }
                if(this.targetViewValue==null){
                    this.targetViewValue=this.addAutoChild(this.targetView);
                }
                if(this.targetViewValue==null){
                    isc.logWarn("TargetView "+this.targetView+" was not found.");
                }
            }
        }else{
            if(this.targetView){
                this.targetViewValue=this.targetView;
            }else if(this.inlineView){
                this.targetViewValue=isc[this.inlineView._constructor].create(this.inlineView);
                if(this.process.containerId){
                    window[this.process.containerId].addMember(this.targetViewValue);
                }
            }
        }
        if(this.targetVM&&isc.isA.String(this.targetVM)){
            if(process.state[this.targetVM]){
                this.targetVMValue=process.getStateVariable(this.targetVM);
            }else{
                this.targetVMValue=window[this.targetVM];
                if(this.targetVMValue==null){
                    isc.logWarn("TargetVM "+this.targetVM+" was not found.");
                }
            }
        }else{
            this.targetVMValue=this.targetVM;
        }
        if(this.targetForm&&isc.isA.String(this.targetForm)){
            if(process.state[this.targetForm]){
                this.targetFormValue=process.getStateVariable(this.targetForm);
            }else{
                this.targetFormValue=window[this.targetForm];
                if(this.targetFormValue==null){
                    isc.logWarn("TargetForm "+this.targetForm+" was not found.");
                }
            }
        }else{
            this.targetFormValue=this.targetForm;
        }
        if(this.targetViewValue==null){
            isc.logWarn("TargetView should be set for UserTask");
            return true;
        }
        if(this.targetFormValue==null){
            if(this.targetViewValue.getClassName()=="DynamicForm"){
                this.targetFormValue=this.targetViewValue;
            }
        }
        if(this.targetFormValue==null&&this.targetVMValue==null){
            isc.logWarn("Rather targetForm or targetVM should be set for UserTask or "+
                "targetView should be a DynamicForm");
            return true;
        }
        this.targetViewValue.showRecursively();
        var values=null;
        if(this.inputField){
            values=isc.clone(process.getStateVariable(this.inputField));
        }else if(this.inputFieldList){
            values={};
            for(var i=0;i<this.inputFieldList.length;i++){
                var key=this.inputFieldList[i];
                values[key]=isc.clone(process.getStateVariable(key));
            }
        }
        if(this.targetVMValue){
            if(values)this.targetVMValue.setValues(values);
            this.targetVMValue.userTask=this;
        }
        if(this.targetFormValue){
            if(values)this.targetFormValue.setValues(values);
            this.targetFormValue.saveToServer=(this.saveToServer==true);
            this.targetFormValue.userTask=this;
        }
        return false;
    }
);
isc.B._maxIndex=isc.C+4;

isc.defineClass("StateTask","Task");
isc.A=isc.StateTask.getPrototype();
isc.B=isc._allFuncs;
isc.C=isc.B._maxIndex;
isc.D=isc._funcClasses;
isc.D[isc.C]=isc.A.Class;
isc.B.push(isc.A.executeElement=function isc_StateTask_executeElement(process){
        if(this.value==null&&this.inputField==null&&this.inputFieldList==null){
            isc.logWarn("StateTask: value, inputField or inputFieldList should be set.");
            return true;
        }
        if(this.value==null&&this.inputField==null){
            if(this.outputFieldList==null||this.outputFieldList.length!=this.inputFieldList.length){
                isc.logWarn("StateTask: outputFieldList should have same number of parameters as inputFieldList.");
                return;
            }
            if(this.type){
                isc.logWarn("StateTask: type cannot be used with multiple outputFields");
            }
            for(var i=0;i<this.inputFieldList.lenght;i++){
                var value=process.getStateVariable(this.inputFieldList[i]);
                process.setStateVariable(this.outputFieldList[i],value);
            }
            return true;
        }
        var value=this.value||process.getStateVariable(this.inputField);
        value=this._executePair(value,this.type,process);
        process.setStateVariable(this.outputField,value);
        return true;
    }
,isc.A._executePair=function isc_StateTask__executePair(value,type,process){
        if(value==null){
            isc.logWarn("StateTask: value is null. Unable to convert to "+type);
            if(this.failureElement==null){
                isc.logWarn("There is no failureElement in stateTask");
            }else{
                process.setNextElement(this.failureElement);
            }
            return null;
        }
        if("string"==type){
            return value.toString();
        }else if("boolean"==type){
            if("true"==value)return true;
            if("false"==value)return false;
            if(isc.isA.String(value))return value.length!=0;
            if(isc.isA.Number(value))return value!=0;
            return value!=null;
        }else if("decimal"==type){
            var v=parseFloat(value.toString());
            if(isNaN(v)){
                if(this.failureElement==null){
                    isc.logWarn("There is no failureElement in stateTask");
                }else{
                    process.setNextElement(this.failureElement);
                }
                return null;
            }
            return v;
        }else if("integer"==type){
            var v=parseInt(value.toString());
            if(isNaN(v)){
                if(this.failureElement==null){
                    isc.logWarn("There is no failureElement in stateTask");
                }else{
                    process.setNextElement(this.failureElement);
                }
                return null;
            }
            return v;
        }else if("record"==type){
            if(isc.isAn.Object(value)&&!isc.isAn.Array(value)&&
                    !isc.isAn.RegularExpression(value)&&!isc.isAn.Date(value)){
                return value;
            }
            return null;
        }else if("array"==type){
            if(isc.isAn.Array(value))return value;
            return[value];
        }else{
            return value;
        }
    }
);
isc.B._maxIndex=isc.C+2;

isc._debugModules = (isc._debugModules != null ? isc._debugModules : []);isc._debugModules.push('Workflow');isc.checkForDebugAndNonDebugModules();isc._moduleEnd=isc._Workflow_end=(isc.timestamp?isc.timestamp():new Date().getTime());if(isc.Log&&isc.Log.logIsInfoEnabled('loadTime'))isc.Log.logInfo('Workflow module init time: ' + (isc._moduleEnd-isc._moduleStart) + 'ms','loadTime');delete isc.definingFramework;if (isc.Page) isc.Page.handleEvent(null, "moduleLoaded", { moduleName: 'Workflow', loadTime: (isc._moduleEnd-isc._moduleStart)});}else{if(window.isc && isc.Log && isc.Log.logWarn)isc.Log.logWarn("Duplicate load of module 'Workflow'.");}

/*

  SmartClient Ajax RIA system
  Version v11.0p_2016-03-31/LGPL Deployment (2016-03-31)

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

