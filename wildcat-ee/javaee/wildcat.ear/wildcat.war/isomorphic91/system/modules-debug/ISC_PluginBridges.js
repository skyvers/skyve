
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

if(window.isc&&window.isc.module_Core&&!window.isc.module_PluginBridges){isc.module_PluginBridges=1;isc._moduleStart=isc._PluginBridges_start=(isc.timestamp?isc.timestamp():new Date().getTime());if(isc._moduleEnd&&(!isc.Log||(isc.Log && isc.Log.logIsDebugEnabled('loadTime')))){isc._pTM={ message:'PluginBridges load/parse time: ' + (isc._moduleStart-isc._moduleEnd) + 'ms', category:'loadTime'};
if(isc.Log && isc.Log.logDebug)isc.Log.logDebug(isc._pTM.message,'loadTime');
else if(isc._preLog)isc._preLog[isc._preLog.length]=isc._pTM;
else isc._preLog=[isc._pTM]}isc.definingFramework=true;

if (window.isc && isc.version != "v9.1p_2014-03-26/LGPL Deployment") {
    isc.logWarn("SmartClient module version mismatch detected: This application is loading the core module from "
        + "SmartClient version '" + isc.version + "' and additional modules from 'v9.1p_2014-03-26/LGPL Deployment'. Mixing resources from different "
        + "SmartClient packages is not supported and may lead to unpredictable behavior. If you are deploying resources "
        + "from a single package you may need to clear your browser cache, or restart your browser."
        + (isc.Browser.isSGWT ? " SmartGWT developers may also need to clear the gwt-unitCache and run a GWT Compile." : ""));
}
isc.ClassFactory.defineClass("BrowserPlugin","Canvas");
isc.A=isc.BrowserPlugin;
isc.B=isc._allFuncs;
isc.C=isc.B._maxIndex;
isc.D=isc._funcClasses;
isc.D[isc.C]=isc.A.Class;
isc.A.instances=[];
isc.B.push(isc.A.handleDragMoveNotify=function isc_c_BrowserPlugin_handleDragMoveNotify(){}
);
isc.B._maxIndex=isc.C+1;

isc.A=isc.BrowserPlugin.getPrototype();
isc.B=isc._allFuncs;
isc.C=isc.B._maxIndex;
isc.D=isc._funcClasses;
isc.D[isc.C]=isc.A.Class;
isc.A.src="";
isc.A.extraHTML="";
isc.A.installPlugin=true;
isc.A.redrawOnResize=false;
isc.A._redrawWithMaster=false;
isc.A._redrawWithParent=false;
isc.A.useDragMask=true;
isc.A.dragMaskType="hidePlugin";
isc.A.usePlaceholderDragMask=!isc.Browser.isMoz;
isc.B.push(isc.A.initWidget=function isc_BrowserPlugin_initWidget(){
    isc.BrowserPlugin.instances.add(this);
}
,isc.A.destroy=function isc_BrowserPlugin_destroy(){
    isc.BrowserPlugin.instances.remove(this);
    this.Super("destroy",arguments);
}
,isc.A.draw=function isc_BrowserPlugin_draw(){
    this.Super("draw",arguments);
    if(this.backMaskCausesBurnThrough){
        var applet=this;
        this.getParentElements().map(function(ancestor){
            if(ancestor.useBackMask){
                applet.logInfo("Suppressing backmask of ancestor: "+ancestor.getID());
                if(ancestor._backMask){
                    ancestor._backMask.suppressed=true;
                    ancestor._backMask.hide();
                }else{
                    if(!ancestor._deferredBackMaskProps)ancestor._deferredBackMaskProps={};
                    ancestor._deferredBackMaskProps.suppressed=true;
                }
            }
        });
    }
}
);
isc.B._maxIndex=isc.C+3;

isc.ClassFactory.defineClass("Applet","BrowserPlugin");
isc.A=isc.Applet;
isc.A.appletScanInterval=500
;

isc.A=isc.Applet;
isc.B=isc._allFuncs;
isc.C=isc.B._maxIndex;
isc.D=isc._funcClasses;
isc.D[isc.C]=isc.A.Class;
isc.B.push(isc.A.initComplete=function isc_c_Applet_initComplete(version){
    this.jvmVersionString=version;
    this.jvmVersion=parseFloat(version);
    this.logInfo("ISCEventProxy init complete - jvmVersion: "+version+" - derived version: "+this.jvmVersion);
}
,isc.A.idForName=function isc_c_Applet_idForName(name){
    if(name&&name.endsWith("_applet"))return name.substring(0,name.length-7);
}
,isc.A.startJavaEventProxy=function isc_c_Applet_startJavaEventProxy(){
    if(this.eventProxyApplet)return;
    this.eventProxyApplet=isc.Applet.create({
        top:-1000,
        width:10,
        height:10,
        autoDraw:false,
        useJavaEventProxy:false,
        useDragMask:true,
        params:{
            debug:this.debug,
            useEventMasks:this.useEventMasks,
            appletScanInterval:this.appletScanInterval
        },
        _showDragMask:function(){
            var handle=this.getPluginHandle();
            if(handle)handle.showDragMask();
        },
        _hideDragMask:function(){
            var handle=this.getPluginHandle();
            if(handle)handle.hideDragMask();
        },
        ID:"isc_eventProxyApplet",
        archive:isc.Page.getURL("[HELPERS]isomorphic_applets.jar"),
        code:"com/isomorphic/applets/ISCEventProxy.class"
    });
    this.eventProxyApplet.draw();
}
);
isc.B._maxIndex=isc.C+3;

isc.A=isc.Applet.getPrototype();
isc.B=isc._allFuncs;
isc.C=isc.B._maxIndex;
isc.D=isc._funcClasses;
isc.D[isc.C]=isc.A.Class;
isc.A.mayScript=true;
isc.A.scriptable=true;
isc.A.classID="clsid:8AD9C840-044E-11D1-B3E9-00805F499D93";
isc.A.objectCodeBase="http://java.sun.com/products/plugin/1.3/jinstall-13-win32.cab#Version=1,3,0,0";
isc.A.useTag="applet";
isc.A.useClipDiv=false;
isc.A.useJavaEventProxy=isc.Browser.isIE;
isc.A.useDragMask=!isc.Browser.isIE;
isc.A.usePlaceholderDragMask=false;
isc.A.backMaskCausesBurnThrough=isc.Browser.isMoz;
isc.A.pluginID=null;
isc.B.push(isc.A.draw=function isc_Applet_draw(){
    if(this.useJavaEventProxy)isc.Applet.startJavaEventProxy();
    this.Super("draw",arguments);
}
,isc.A.getInnerHTML=function isc_Applet_getInnerHTML(){
    var accum=isc.StringBuffer.newInstance();
    if(this.code==null&&this.src!=null)this.code=this.src;
    if(this.useTag=="applet"){
        accum.append("<applet name='",this.getPluginID(),
                     "' width='100%' height='100%'",
                     " iscCanvasID='",this.getID(),"'");
        if(this.mayScript)accum.append(" mayScript");
        if(this.scriptable)accum.append(" scriptable");
        if(this.code)accum.append(" code='",this.code,"'");
        if(this.codeBase)accum.append(" codeBase='",this.codeBase,"'");
        if(this.archive)accum.append(" archive='",this.archive,"'");
        if(this.alt)accum.append(" alt='",this.alt,"'");
        if(this.extraHTML)accum.append(" ",this.extraHTML);
        accum.append(">");
        if(this.params){
            for(var key in this.params){
                accum.append("<param name='",key,"' value='",this.params[key],"'>");
            }
        }
        if(this.altHTML)accum.append(this.altHTML);
        accum.append("</applet>");
    }else if(this.useTag=="object"){
        accum.append("<object classid='",this.classID,"' codebase='",this.objectCodeBase,
                     "' width='100%' height='100%'");
        if(this.extraHTML)accum.append(" ",this.extraHTML);
        accum.append(">");
        accum.append("<param name='name' value='",this.getPluginID(),"'>");
        accum.append("<param name='iscCanvasID' value='",this.getID(),"'>");
        if(this.mayScript)accum.append("<param name='mayscript' value='true'>");
        if(this.scriptable)accum.append("<param name='scriptable' value='true'>");
        if(this.code)accum.append("<param name='code' value='",this.code,"'>");
        if(this.codeBase)accum.append("<param name='codeBase' value='",this.codeBase,"'>");
        if(this.archive)accum.append("<param name='archive' value='",this.archive,"'>");
        if(this.alt)accum.append("<param name='alt' value='",this.alt,"'>");
        if(this.params){
            for(var key in this.params){
                accum.append("<param name='",key,"' value='",this.params[key],"'>");
            }
        }
        accum.append("</object>");
    }
    return accum.toString();
}
,isc.A.getPluginID=function isc_Applet_getPluginID(){
    if(!this.pluginID){
        if(!this.name)this.name=this.getID()+"_applet";
        return this.name;
    }else{
        return this.pluginID;
    }
}
,isc.A.setPluginID=function isc_Applet_setPluginID(pluginID){
    this.pluginID=pluginID;
}
,isc.A.getPluginHandle=function isc_Applet_getPluginHandle(){
    return document[this.getPluginID()];
}
,isc.A.repaint=function isc_Applet_repaint(){
    var handle=this.getPluginHandle();
    if(handle)handle.repaint();
}
,isc.A.repaintOnDragStop=function isc_Applet_repaintOnDragStop(){
    return this.useJavaEventProxy&&isc.Applet.jvmVersion<1.4;
}
,isc.A._hideDragMask=function isc_Applet__hideDragMask(){
    this.Super("_hideDragMask",arguments);
    if(this.repaintOnDragStop())this.repaint();
}
);
isc.B._maxIndex=isc.C+8;

isc.ClassFactory.defineClass("Flashlet","BrowserPlugin");
isc.A=isc.Flashlet;
isc.B=isc._allFuncs;
isc.C=isc.B._maxIndex;
isc.D=isc._funcClasses;
isc.D[isc.C]=isc.A.Class;
isc.B.push(isc.A.flashAvailable=function isc_c_Flashlet_flashAvailable(){
        if(this.flashSupported!=null)return this.flashSupported;
        isc.Flashlet.flashVersion=0;
        isc.Flashlet.flashSupported=false;
        if(isc.Browser.isIE){
            if(window.ActiveXObject){
                try{
                    var flashControl=new ActiveXObject("ShockwaveFlash.ShockwaveFlash");
                    if(flashControl){
                        var version=flashControl.GetVariable("$version").
                            replace(/[^0-9]+[\s]+([0-9]+)[,.][\s]*([0-9]+).*/,"$1.$2");
                        isc.Flashlet.flashSupported=true;
                        isc.Flashlet.flashVersion=parseFloat(version);
                    }
                }catch(e){
                    this.logInfo("Unable to create sample flash ActiveX object: "+e);
                }
            }
        }else{
            var flashPlugin=navigator.plugins["Shockwave Flash"];
            if(flashPlugin==null)flashPlugin=navigator.plugins["Shockwave Flash 2.0"];
            if(flashPlugin!=null){
                this.flashSupported=true;
                var versionString=flashPlugin.description.substring(16),
                    versionNum=parseFloat(versionString.split(" ")[0]);
                this._fullFlashVersion=versionString;
                this.flashVersion=versionNum;
            }else{
                this.flashSupported=false;
            }
        }
        return this.flashSupported;
    }
,isc.A.getFlashVersion=function isc_c_Flashlet_getFlashVersion(){
        if(this.flashAvailable())return this.flashVersion;
    }
);
isc.B._maxIndex=isc.C+2;

isc.A=isc.Flashlet.getPrototype();
isc.A.useClipDiv=false;
isc.A.useDragMask=false;
isc.A.classID="clsid:D27CDB6E-AE6D-11cf-96B8-444553540000";
isc.A.codeBase="http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=5,0,0,0";
isc.A.pluginsPage="http://www.macromedia.com/shockwave/download/index.cgi?P1_Prod_Version=ShockwaveFlash";
isc.A.type="application/x-shockwave-flash"
;

isc.A=isc.Flashlet.getPrototype();
isc.B=isc._allFuncs;
isc.C=isc.B._maxIndex;
isc.D=isc._funcClasses;
isc.D[isc.C]=isc.A.Class;
isc.B.push(isc.A.setSrc=function isc_Flashlet_setSrc(src){
    this.src=src;
    this.markForRedraw();
}
,isc.A.getInnerHTML=function isc_Flashlet_getInnerHTML(){
    var accum=isc.SB.create();
    if(this.name==null)this.name=this.getPluginID();
    var protocol=window.location.protocol,
        codeBase=this.codeBase
    ;
    if(protocol&&protocol.startsWith("https")&&codeBase&&codeBase.startsWith("http://")){
        codeBase=codeBase.replace("http://","https://");
    }
    accum.append("<object classid='",this.classID,"' codebase='",codeBase,
                 "' width='100%' height='100%' ID='",this.name,"'");
    if(this.extraObjectHTML)accum.append(" ",this.extraObjectHTML);
    accum.append(">");
    var params={};
    isc.addProperties(params,this.params);
    if(!params.movie)params.movie=this.src||this.movie;
    if(!params.wmode)params.wmode="opaque"
    for(var key in params)
        accum.append("<param name='",key,"' value='",params[key],"'>");
    accum.append("<embed width='100%' height='100%' name='",this.name,"' src=\"",this.src,
                 "\" pluginspage=\"",this.pluginsPage,"\" type='",this.type,"'");
    for(var key in params)
        accum.append(" ",key,"='",params[key],"'");
    if(this.extraEmbedHTML)accum.append(" ",this.extraEmbedHTML);
    accum.append(">");
    accum.append("</embed>");
    accum.append("</object>");
    return accum.toString();
}
,isc.A.getPluginID=function isc_Flashlet_getPluginID(){
    return this.getID()+"_flash";
}
,isc.A.getPluginHandle=function isc_Flashlet_getPluginHandle(){
    if(this.name==null)return null;
    if(isc.Browser.isIE)return window[this.name];
    return document[this.name];
}
);
isc.B._maxIndex=isc.C+4;

isc.ClassFactory.defineClass("SVG","BrowserPlugin");
isc.A=isc.SVG.getPrototype();
isc.A.useNativeMask=true;
isc.A.pluginsPage="http://www.adobe.com/svg/viewer/install/";
isc.A.src=isc.Page.getHelperDir()+"svgCanvas.svg"
;

isc.A=isc.SVG;
isc.B=isc._allFuncs;
isc.C=isc.B._maxIndex;
isc.D=isc._funcClasses;
isc.D[isc.C]=isc.A.Class;
isc.B.push(isc.A.register=function isc_c_SVG_register(evt){
    var svgElement=evt.getTarget();
    var svgDocument=svgElement.getOwnerDocument();
    var svgInstance=this.getSVGCanvas(svgDocument);
    svgInstance.addProperties({
        svgElement:svgElement,
        svgDocument:svgDocument,
        svgDoc:svgDocument,
        svgRoot:svgDocument.getRootElement()
    });
    var embedHandle=svgInstance.getPluginHandle();
    if(embedHandle["window"])embedHandle["window"].svgCanvas=svgInstance;
    if(isc.isA.Function(svgInstance.initSVG))svgInstance.initSVG();
    else if(isc.isA.Function(svgInstance.initsvg))svgInstance.initsvg();
    if(svgInstance.useNativeMask)svgInstance._makeSVGEventMask();
    if(svgInstance._deferShowNativeMask)svgInstance.showNativeMask();
}
,isc.A.getSVGCanvas=function isc_c_SVG_getSVGCanvas(svgDocument){
    var url=svgDocument.getURL();
    if(url.indexOf("#")==-1){
        this.logError("Can't locate svgCanvas for svgDocument. Use SVG.create() to render SVGs");
        return null;
    }
    var svgID=url.substring(url.indexOf("#")+1,url.length);
    var svgInstance=window[svgID];
    if(!svgInstance){
        this.logError("Can't locate svg instance for id: "+svgID
                      +" did you call SVG.register(evt)?");
        return null;
    }
    return svgInstance;
}
);
isc.B._maxIndex=isc.C+2;

isc.A=isc.SVG.getPrototype();
isc.B=isc._allFuncs;
isc.C=isc.B._maxIndex;
isc.D=isc._funcClasses;
isc.D[isc.C]=isc.A.Class;
isc.B.push(isc.A.draw=function isc_SVG_draw(){
    this.Super("draw",arguments);
    if(isc.Browser.isIE)isc.EH.registerMaskableItem(this,true);
}
,isc.A.mouseOut=function isc_SVG_mouseOut(){
    if(this.useNativeMask){
        this.hideNativeMask();
        this.Super("_hideDragMask");
    }
}
,isc.A._showDragMask=function isc_SVG__showDragMask(){
    if(this.useNativeMask)this.showNativeMask();
    else this.Super("_showDragMask");
}
,isc.A._hideDragMask=function isc_SVG__hideDragMask(){
    if(this.useNativeMask)this.hideNativeMask();
    else this.Super("_hideDragMask");
}
,isc.A.handleSVGEvent=function isc_SVG_handleSVGEvent(evt){
    if(evt.type=="mousemove"){
        if(this.lastMouseMoveX==evt.clientX&&this.lastMouseMoveY==evt.clientY)return;
        this.lastMouseMoveX=evt.clientX;
        this.lastMouseMoveY=evt.clientY;
    }
    var event={
        type:evt.type,
        target:this,
        clientX:evt.clientX,
        clientY:evt.clientY
    };
    isc.EventHandler.handleSyntheticEvent(event);
}
,isc.A._makeSVGEventMask=function isc_SVG__makeSVGEventMask(){
    this._svgMask=this.svgDoc.createElement("rect");
    this._svgEventMaskID=this.getID()+"_SVGEventMask";
    var maskAttributes={
        id:this._svgEventMaskID,
        x:"-1073741823",
        y:"-1073741823",
        width:"2147483647",
        height:"2147483647",
        opacity:"0.0",
        visibility:"hidden",
        onmousemove:"svgCanvas.handleSVGEvent(evt)",
        onmouseup:"svgCanvas.handleSVGEvent(evt)",
        onmouseout:"svgCanvas.handleSVGEvent(evt)",
        onclick:"svgCanvas.handleSVGEvent(evt)",
        oncontextmenu:"svgCanvas.handleSVGEvent(evt)"
    };
    for(var key in maskAttributes)this._svgMask.setAttribute(key,maskAttributes[key]);
    this.svgRoot.appendChild(this._svgMask);
}
,isc.A.setZIndex=function isc_SVG_setZIndex(){
}
,isc.A.showNativeMask=function isc_SVG_showNativeMask(){
    if(!this.svgDoc){
        this.logWarn("showNativeMask called before SVG.register() - deferring until SVG.register()");
        this._deferShowNativeMask=true;
        return;
    }
    if(this._svgMask){
        this.svgRoot.removeChild(this._svgMask);
        this.svgRoot.appendChild(this._svgMask);
    }else{
        this._makeSVGEventMask();
    }
    this._svgMask.setAttribute("visibility","visible");
}
,isc.A.hideNativeMask=function isc_SVG_hideNativeMask(){
    if(this._svgMask)this._svgMask.setAttribute("visibility","hidden");
}
,isc.A.getInnerHTML=function isc_SVG_getInnerHTML(){
    if(isc.Browser.isIE){
        return"<embed name='"+this.getPluginID()+"' src=\""+isc.Page.getURL(this.src)
               +"#"+this.getID()+"\" width='100%' height='100%'"
               +(this.installPlugin?"pluginspage='"+this.pluginsPage+"'":"")
               +" type='image/svg+xml' "
               +this.extraHTML+" >";
    }
    return this.Super("getInnerHTML",arguments);
}
,isc.A.destroy=function isc_SVG_destroy(){
    if(this._svgMask)delete this._svgMask;
    var embedHandle=this.getPluginHandle();
    if(embedHandle&&embedHandle["window"])delete embedHandle["window"].svgCanvas;
    this.Super("destroy",arguments);
}
,isc.A.setNodeAttributes=function isc_SVG_setNodeAttributes(obj,attrs){
    for(var key in attrs)obj.setAttribute(key,attrs[key]);
}
);
isc.B._maxIndex=isc.C+12;

isc.ClassFactory.defineClass("ActiveXControl","BrowserPlugin");
isc.A=isc.ActiveXControl.getPrototype();
isc.B=isc._allFuncs;
isc.C=isc.B._maxIndex;
isc.D=isc._funcClasses;
isc.D[isc.C]=isc.A.Class;
isc.B.push(isc.A.getInnerHTML=function isc_ActiveXControl_getInnerHTML(){
    var accum=isc.StringBuffer.newInstance();
    var classID=this.classID?this.classID:"clsid:"+this.uuid;
    accum.append("<object classid='",classID,"' codebase='",this.codeBase,
                "' id='"+this.getPluginID(),"' width='100%' height='100%'");
    if(this.extraHTML)accum.append(" ",this.extraHTML);
    accum.append(">");
    accum.append("<param name='iscCanvasID' value='",this.getID(),"'>");
    if(this.params){
        for(var key in this.params){
            accum.append("<param name='",key,"' value='",this.params[key],"'>");
        }
    }
    accum.append("</object>");
    return accum.toString();
}
,isc.A.getPluginID=function isc_ActiveXControl_getPluginID(){
    if(!this.id)this.id=this.getID()+"_activeXControl";
    return this.id;
}
,isc.A.getPluginHandle=function isc_ActiveXControl_getPluginHandle(){
    return window[this.getPluginID()];
}
);
isc.B._maxIndex=isc.C+3;
isc._debugModules = (isc._debugModules != null ? isc._debugModules : []);isc._debugModules.push('PluginBridges');isc.checkForDebugAndNonDebugModules();isc._moduleEnd=isc._PluginBridges_end=(isc.timestamp?isc.timestamp():new Date().getTime());if(isc.Log&&isc.Log.logIsInfoEnabled('loadTime'))isc.Log.logInfo('PluginBridges module init time: ' + (isc._moduleEnd-isc._moduleStart) + 'ms','loadTime');delete isc.definingFramework;}else{if(window.isc && isc.Log && isc.Log.logWarn)isc.Log.logWarn("Duplicate load of module 'PluginBridges'.");}

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

