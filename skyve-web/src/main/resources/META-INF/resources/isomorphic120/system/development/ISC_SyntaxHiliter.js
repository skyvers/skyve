/*

  SmartClient Ajax RIA system
  Version v12.0p_2019-08-11/LGPL Development Only (2019-08-11)

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

if(window.isc&&window.isc.module_Core&&!window.isc.module_SyntaxHiliter){isc.module_SyntaxHiliter=1;isc._moduleStart=isc._SyntaxHiliter_start=(isc.timestamp?isc.timestamp():new Date().getTime());if(isc._moduleEnd&&(!isc.Log||(isc.Log&&isc.Log.logIsDebugEnabled('loadTime')))){isc._pTM={message:'SyntaxHiliter load/parse time: '+(isc._moduleStart-isc._moduleEnd)+'ms',category:'loadTime'};if(isc.Log&&isc.Log.logDebug)isc.Log.logDebug(isc._pTM.message,'loadTime');else if(isc._preLog)isc._preLog[isc._preLog.length]=isc._pTM;else isc._preLog=[isc._pTM]}isc.definingFramework=true;isc.defineClass("SyntaxHiliter").addProperties({spanStart:"<span style='",spanStartClose:"'>",spanEnd:"</span>",defaultStyle:"hiliter_default"});isc.A=isc.SyntaxHiliter.getPrototype();isc.B=isc._allFuncs;isc.C=isc.B._maxIndex;isc.D=isc._funcClasses;isc.D[isc.C]=isc.A.Class;isc.B.push(isc.A.init=function isc_SyntaxHiliter_init(){this.fixedSpanLengths=this.spanStart.length+this.spanStartClose.length+this.spanEnd.length;this.spanEndLength=this.spanEnd.length},isc.A.hilite=function isc_SyntaxHiliter_hilite(_1,_2,_3,_4){var _5=this.regexps;if(!_5)return _1;var _6=[_1];for(var _7=0;_7<_5.length;_7++){var _8=_5[_7];var _9=_8.regexp;var _10=_8.cssStyles;if(!isc.isAn.Array(_10))_10=[_10];var _11=0;while(_11<_6.length){var _12=_6[_11];if(_12==null){_22+=this.fixedSpanLengths+_6[_11+2].length+_6[_11+4].length;_11+=6;continue}
var _13=_9.exec(_12);if(_13==null){_11++;_22+=_12.length;continue}
if(_13.length-1!=_10.length){this.logWarn("regexp: "+_9+" matched "+(_13.length-1)+" groups, but only "+_10.length+" cssStyles are defined - skipping this regexp.");_11=_6.length;continue}
var _14=_13[0];var _15=_13.index;_22+=_15;var _16=_12.substring(0,_15);var _17=_12.substring(_15+_14.length);if(_16.length>0){_6.splice(_11++,1,_16,_17)}else{_6[_11]=_17}
for(var _18=1;_18<_13.length;_18++){var _19=_13[_18]||isc.emptyString;var _20=_10[_18-1];_20=(_20||this.defaultStyle)+(this.darkMode?"$191p":"");if(_20==null){_6.splice(_11++,0,_19);continue}
var _21=isc.Element.getStyleText(_20)||isc.emptyString;_6.splice(_11,0,null,this.spanStart,_21,this.spanStartClose,_19,this.spanEnd);_11+=6}}}
var _22=0;var _23=_3!=null;for(var i=0;i<_6.length;i++){var _12=_6[i];var _25;if(_12==null){_12=_6[i+4];_25=_12.asHTML(!this.autoWrap);if(_23&&_3>_22){if(_3<(_22+_12.length)){var _26=_3-_22;_27=_12.slice(0,_26);_3+=_27.asHTML(!this.autoWrap).length-_27.length;_3-=this.spanEndLength}else{_3+=_25.length-_12.length}
_3+=this.fixedSpanLengths+_6[i+2].length;_22+=_25.length+this.fixedSpanLengths+_6[i+2].length}else{_23=false}
_6[i+4]=_25;i+=5;continue}
_25=_12.asHTML(!this.autoWrap);if(_23&&_3>_22){if(_3<(_22+_12.length)){var _26=_3-_22;var _27=_12.slice(0,_26);_3+=_27.asHTML(!this.autoWrap).length-_27.length}else{_3+=_25.length-_12.length}
_22+=_25.length}else{_23=false}
_6[i]=_25}
var _28=_6.join(isc.emptyString);if(_3!=null){_28=_28.slice(0,_3)+_4+_28.slice(_3,_28.length)}
if(_2){var _29=[];var _30;while((_30=_28.indexOf("<BR>"))!=-1){var _31=_28.slice(0,_30+4);_28=_28.substring(_30+4);_29[_29.length]=_31}
if(_28.length)_29[_29.length]=_28;return _29}else{return _28}},isc.A.containsMultilineToken=function isc_SyntaxHiliter_containsMultilineToken(_1){if(this.multilineTokens){for(var i=0;i<this.multilineTokens.length;i++)
if(_1.match(this.multilineTokens[i]))return true}
return false});isc.B._maxIndex=isc.C+3;isc.defineClass("JSSyntaxHiliter","SyntaxHiliter");isc.A=isc.JSSyntaxHiliter.getPrototype();isc.A.regexps=[{regexp:/(\/\*.*?\*\/)/,cssStyles:"js_multilineComment"},{regexp:/(\/\/.*)/,cssStyles:"js_lineComment"},{regexp:/("(?:[^"\n]|\\")*")/,cssStyles:"js_doubleQuotedString"},{regexp:/('(?:[^'\n]|\\')*')/,cssStyles:"js_singleQuotedString"},{regexp:/(\/(?:\\\/|[^\/\n])+\/)/,cssStyles:"js_regex"},{regexp:/([^a-zA-Z0-9_$:]|^)(abstract|boolean|break|byte|case|char|class|const|continue|default|delete|do|dougle|else|enum|extends|final|finally|float|for|fun[c]tion|goto|if|implements|import|in|instanceof|int|interface|long|native|new|package|private|protected|public|return|short|static|super|switch|synchronized|this|throw|throws|transient|try|typeof|var|void|while|with)([^a-zA-Z0-9_$:]|$)/,cssStyles:[null,"js_reservedWord",null]},{regexp:/([^a-zA-Z0-9_$:]|^)(true|false|null)([^a-zA-Z0-9_$:]|$)/,cssStyles:[null,"js_nativeValue",null]},{regexp:/([a-zA-Z][a-zA-Z0-9_$]*)(\s*:)/,cssStyles:["js_label",null]}];isc.A.multilineTokens=[/(\/\*)|(\*\/)/];isc.defineClass("CSSSyntaxHiliter","SyntaxHiliter");isc.A=isc.CSSSyntaxHiliter.getPrototype();isc.A.regexps=[{regexp:/(\/\*.*?\*\/)/,cssStyles:"css_multilineComment"},{regexp:/(\/\/.*)/,cssStyles:"css_lineComment"},{regexp:/([a-zA-Z][a-zA-Z0-9_$\-]*)(\s*:)/,cssStyles:["css_label",null]}];isc.A.multilineTokens=[/(\/\*)|(\*\/)/];isc.defineClass("XMLSyntaxHiliter","SyntaxHiliter");isc.A=isc.XMLSyntaxHiliter.getPrototype();isc.A.regexps=[{regexp:/(<!\[CDATA\[.*?\]\]>)/,cssStyles:"xml_cdataBlock"},{regexp:/(<!--.*?-->)/,cssStyles:"xml_comment"},{regexp:/(\w+)(=)("(?:[^"]|\\")*")/,cssStyles:["xml_attributeName",null,"xml_attributeValue"]},{regexp:/(<\/?)([_:A-Za-z][A-Za-z0-9_.:\-]*)/,cssStyles:[null,"xml_tagName"]}];isc.A.multilineTokens=[/(<!--)|(-->)/,/(<!\[CDATA\[)|(\]\]>)/];isc._nonDebugModules=(isc._nonDebugModules!=null?isc._nonDebugModules:[]);isc._nonDebugModules.push('SyntaxHiliter');isc.checkForDebugAndNonDebugModules();isc._moduleEnd=isc._SyntaxHiliter_end=(isc.timestamp?isc.timestamp():new Date().getTime());if(isc.Log&&isc.Log.logIsInfoEnabled('loadTime'))isc.Log.logInfo('SyntaxHiliter module init time: '+(isc._moduleEnd-isc._moduleStart)+'ms','loadTime');delete isc.definingFramework;if(isc.Page)isc.Page.handleEvent(null,"moduleLoaded",{moduleName:'SyntaxHiliter',loadTime:(isc._moduleEnd-isc._moduleStart)});}else{if(window.isc&&isc.Log&&isc.Log.logWarn)isc.Log.logWarn("Duplicate load of module 'SyntaxHiliter'.");}
