/*
 * Isomorphic SmartClient
 * Version v9.1p_2014-03-26 (2014-03-26)
 * Copyright(c) 1998 and beyond Isomorphic Software, Inc. All rights reserved.
 * "SmartClient" is a trademark of Isomorphic Software, Inc.
 *
 * licensing@smartclient.com
 *
 * http://smartclient.com/license
 */

 




// Node.contains() was introduced in Gecko 9.
// https://developer.mozilla.org/en-US/docs/Web/API/Node.contains
if (!("contains" in document.documentElement) && window.Node) {
    Node.prototype.contains = function (otherNode) {
        for (; otherNode != null; otherNode = otherNode.parentNode) {
            if (this == otherNode) return true;
        }
        return false;
    };
}

//>	@class	Element
//
// Helper class containing methods for direct DOM interaction. Note that even if 
// +link{isc, isc_useSimpleNames} is true, this class is not available in the global scope
// as <code>window.Element</code> - to access it developers must always use 
// <code>isc.Element</code>
//
//  @treeLocation Client Reference/Foundation
//  @visibility internal
//<
// Currently has no exposed methods - Developers typically will only need to interact with 
// higher level canvas methods
isc.ClassFactory.defineClass("Element", null, null, true);

isc.Element.addClassProperties({

// The browser's prefix for experimental/unspec'd CSS properties, as it would appear in CSS text.
vendorCSSPrefix: (isc.Browser.isMoz ? "-moz-" :
                  isc.Browser.isChrome ? "-webkit-" :
                  isc.Browser.isSafari ? "-webkit-" :
                  isc.Browser.isOpera ? "-o-" :
                  isc.Browser.isIE ? "-ms-" :
                  ""),

// The browser's prefix for CSSStyleDeclaration properties corresponding to experimental/unspec'd
// CSS properties.
// https://github.com/Modernizr/Modernizr/blob/master/src/omPrefixes.js
vendorStylePrefix: (isc.Browser.isMoz ? "Moz" :
                    isc.Browser.isChrome ? "Webkit" :
                    isc.Browser.isSafari ? "Webkit" :
                    isc.Browser.isOpera ? "O" :
                    isc.Browser.isIE ? "ms" :
                    "")

});

isc.Element.addClassMethods({


// --------------------------------------------------------------------------------------------
// DOM Access / Manipulation

//>	@classMethod	Element.get()
//		Like the DOM method document.getElementById(), but works in all supported browsers.
//<
get : function (id, doc) {
    doc = doc || this.getDocument();
    if (isc.Browser.isDOM) return doc.getElementById(id);
},


// _getElementFromSelection()
// Determine which DOM element contains the current selection.
// 'doc' param allows caller to pass in a pointer to the document element - (may be document
// element from some frame/iframe - if not specified the main page document is used).

_getElementFromSelection : function (doc) {

    

    if (!doc) doc = document;

    if (isc.Browser._hasDOMRanges) {
        var selection = doc.getSelection();
        if (!selection.rangeCount) return null;
        
        var commonAncestorContainer = selection.getRangeAt(0).commonAncestorContainer;
        while (commonAncestorContainer != null && commonAncestorContainer.nodeType != 1) {
            commonAncestorContainer = commonAncestorContainer.parentNode;
        }
        return commonAncestorContainer;
    } else if (isc.Browser.isIE) {
        var selection = doc.selection;
        // In some cases, Internet Explorer can throw an exception when on
        // attempting to access the selection.type attribute (though the
        // attribute does appear to be defined).
        
        try {
            var type = selection.type.toLowerCase();
        } catch (e) {
            return null;
        }
        var isText = (type == "text" || type == "none");

        if (!selection) return null;

        // If it's a text range use the 'parentElement()' method to determine what element
        // contains the text.
        // NOTE: an empty selection will be reported as type "None", but can be used to create
        // a zero char text range, so we treat it like a "Text" selection.
        if (isText) {
            var range;
            
            try {
                range = selection.createRange();
            } catch (e) {
                
            }
            return range ? range.parentElement() : null;

        // If it's a control range, we can get at the elements in the control range
        // by index.    Iterate through the elements and find the common ancester.
        
        } else {  
            // If this is a control range 
            // We're interested in the first common ancestor of the elements
            var range = selection.createRange(),
                commonParent;
            for (var i = 0; i < range.length; i++) {

                if (!commonParent) {
                    commonParent = range(i).parentElement;                

                } else {
                    // To determine whether the element is contained by the common parent, 
                    // we're creating a textRange from both elements and using the inRange
                    // method.
                    while (!commonParent.contains(range(i))) {
                        commonParent = commonParent.parentElement;
                    }
                } 
                
            }
            return commonParent;
        }
    }
},

// From SmartGWT.mobile's `com.smartgwt.mobile.client.internal.util.ElementUtil'
hasClassName : function (element, className) {
    if (!className) return false;
    var str = element.className,
        pos = str.indexOf(className);
    while (pos != -1) {
        if (pos == 0 || str[pos - 1] == ' ') {
            pos += className.length;
            if (pos == str.length || str[pos] == ' ') {
                return true;
            }
        } else pos += className.length;
        if (pos >= str.length) break;
        pos = str.indexOf(className, pos);
    }
    return false;
},

// Calls `elementCallback' on each descendant element of `element' having the given CSS class.
// https://developer.mozilla.org/en-US/docs/Web/API/element.querySelectorAll
forEachDescendantHavingClass : function (element, className, elementCallback) {
    if (element == null || !className) return;

    if (element.querySelectorAll) {
        var matches = element.querySelectorAll("." + className),
            len = matches.length;
        for (var ri = len; ri > 0; --ri) {
            elementCallback(matches[ri - 1]);
        }
    } else {
        var children = element.childNodes,
            len = children.length;
        for (var ri = len; ri > 0; --ri) {
            var child = children[ri - 1];

            // Skip the child node if it is not an element.
            if (child.nodeType != 1) continue;

            // Recurse over children of the child.
            this.forEachDescendantHavingClass(child, className, elementCallback);

            if (this.hasClassName(child, className)) elementCallback(child);
        }
    }
},

// Given an element attribute, iterate recursively through child nodes till you find a match.
// May be slow for deep hierarchies
findAttribute : function (element, attribute, value) {
    if (!element) return null;    
    if (element[attribute] == value || 
    (element.getAttribute && element.getAttribute(attribute) == value)) {   
        return element;
    }
    var children = element.childNodes;
    for (var i = 0; i < children.length; i++) {
        var subElement = this.findAttribute(children[i], attribute, value);
        if (subElement) return subElement;
    }
    return null;
    
},


// helpers for createAbsoluteElement
_insertAfterBodyStart : window.isc_insertAfterBodyStart,
_globalInsertionMarker : "isc_global_insertion_marker",
getInsertionMarkerHTML : function () {
    return "<span id='"+this._globalInsertionMarker+"' style='display:none'></span>";
},
getInsertionMarker : function () {
    return document.getElementById(this._globalInsertionMarker);
},
// create a new, absolutely positioned element, after page load
_$afterBegin : "afterBegin",
_$afterEnd : "afterEnd",
_$beforeBegin: "beforeBegin",
_$beforeEnd : "beforeEnd",
createAbsoluteElement : function (html, targetWindow) {
    var wd = targetWindow || this.getWindow(),
         
        body = this.getDocumentBody(true);

    
    
    //>DEBUG
    // if there's no body tag, we bail
    if (body == null && !isc.Element.noBodyTagMessageShown) {
        isc.Element.noBodyTagMessageShown = true;
        var msg ="Error: Attempt to write content into a page outside the BODY tag.  Isomorphic " +
            "SmartClient requires this tag be present and all widgets be written out inside " +
            "it.\r" +
            "Please ensure your file has a BODY tag and any code to draw SmartClient widgets " +
            "is enclosed in this tag."
            ;
        //alert(msg);
        this.logError(msg);
        return;
    }
    //<DEBUG

    

    // safety valve - use a global var to switch back to our previous rendering mode - set
    // window.isc_insertAfterBodyStart to enable this rendering mode.
    if (this._insertAfterBodyStart) {
        return isc.Element.insertAdjacentHTML(body, this._$afterBegin, html, true);
    }

    if (isc.Browser.isIE) {
        if (!this._insertionMarker) {
            if (body.childNodes.length < 2) {
                // empty body or body with just one element, in either case insert afterBegin
                // of the body on the assumption that the one element may not be closed.
                isc.Element.insertAdjacentHTML(body, this._$afterBegin, this.getInsertionMarkerHTML());
            } else {
                // insert right before the last element
                //
                // If the last element is a text node, spin back through the siblings until we
                // find one that isn't a text node because calling
                // textNode.insertAdjacentHTML() results in a "no such method" in IE
                //
                // One natural way in which such text elements get created is by the use of an
                // <INPUT> element with no enclosing <FORM> tag, right at the end of the page.
                var node = body.lastChild;
                while (node && node.nodeType == 3) node = node.previousSibling;
                if (node != null) {
                    isc.Element.insertAdjacentHTML(node, this._$beforeBegin, 
                                                  this.getInsertionMarkerHTML());
                } else {
                    // all nodes of the body are text nodes, insert after body begin
                    isc.Element.insertAdjacentHTML(body, this._$afterBegin, this.getInsertionMarkerHTML());
                }
            }
            this._insertionMarker = this.getInsertionMarker();
        }
        return isc.Element.insertAdjacentHTML(this._insertionMarker, this._$afterEnd, html, true);            
    } else {
        return isc.Element.insertAdjacentHTML(body, this._$beforeEnd, html, true);
    }
},

// emulate IE's insertAdjacentHTML on any fully DOM-compliant browser
insertAdjacentHTML : function (element, where, html, singleElement) {
    where = where.toLowerCase(); // case-insensitive match

    // handle string element IDs
    if (isc.isA.String(element)) element = isc.Element.get(element);

    //>DEBUG
    if (!element) this.logWarn("insertAdjacentHTML: element is null for where: '" + where + 
                                "' with html: " + html);
    //<DEBUG

    // Use insertAdjacentHTML() if it is available.
    
    if (isc.Browser.useInsertAdjacentHTML) {
        try {
            element.insertAdjacentHTML(where, html);

            if (singleElement) {
                
                switch (where) {
                    case "beforebegin":
                        return element.previousSibling;
                    case "afterbegin":
                        return element.firstChild;
                    case "beforeend":
                        return element.lastChild;
                    case "afterend":
                        return element.nextSibling;
                }
            }
            return;
        } catch (e) {
            // In IE 6, 7, 8, and 9, insertAdjacentHTML() does not work on some elements,
            // notably TABLE, THEAD, TBODY, TFOOT, and TR elements. Attempting to call
            // insertAdjacentHTML() on one of those elements results in an exception
            // "Invalid target element for this operation.", error number -2146827688.
            //
            // If that happens, fall through to the alternative implementation.
        }
    }

    //this.logWarn("inserting at element: " + this.echoLeaf(element) + 
    //             " at position " + where + ", html: " + this.echoLeaf(html));


    
    var newElement,
        doc = element.ownerDocument;
    if (!singleElement && isc.Browser.useCreateContextualFragment) {
        // create a document fragment from the HTML via Range.createContextualFragment()
        var range = doc.createRange();
        range.setStartBefore(element);
        newElement = range.createContextualFragment(html);
    } else {
        var wrapper = doc.createElement("DIV");
        
        if (element.tagName == "TR" && isc.Browser.isIE) {
            wrapper.innerHTML = "<table><tbody><tr>" + html + "</tr></tbody></table>";
            wrapper = wrapper.firstChild.firstChild.firstChild;
        } else {
            wrapper.innerHTML = html;
        }
        if (singleElement || wrapper.firstChild == wrapper.lastChild) newElement = wrapper.firstChild;
        else {
            newElement = doc.createDocumentFragment();
            var child;
            while (child = wrapper.firstChild) {
                newElement.appendChild(child);
            }
        }
    }

    // insert it into the given parent
    switch (where){
    case "beforebegin":
        element.parentNode.insertBefore(newElement, element);
		break;
    case "afterbegin":
		element.insertBefore(newElement, element.firstChild);
		break;
    case "beforeend":
		element.appendChild(newElement);
		break;
    case "afterend":
		if (element.nextSibling) element.parentNode.insertBefore(newElement, element.nextSibling);
		else element.parentNode.appendChild(newElement);
		break;
	}
    if (singleElement) return newElement;
},

// clear the element passed in (removing it's HTML from the DOM)
clear : function (element, useRemoveChild) {
    if (element == null) return;
 
    
    if (!useRemoveChild && isc.Page.isLoaded() && isc.Browser.isIE) {
        
        element.outerHTML = isc.emptyString;
        return;
    }

    if (element.parentNode) {
        element.parentNode.removeChild(element);
    } else {
        
        //>DEBUG
        isc.Log.logWarn("element parentNode null"); //<DEBUG
        element.innerHTML = "";
    }
},	

// ----------------------------------------------------------------------------------------
// Deriving sizing/positioning + margins etc information from HTML elements
// ----------------------------------------------------------------------------------------
// As with Canvii, these methods will work with the size of the HTML element, INCLUDING any
// margins wherever appropriate.
// This means:
// - getOffsetLeft(element) / getOffsetTop(element) return the offset top / left of the element's
//   margin, rather than of the element itself
// - getVisibleWidth(element) / getVisibleHeight(element) return the height / width of the element
//   including top and bottom margins.


// helper: Does this element adhere to the border-box model or the content-box model for sizing?
isBorderBox : function (element) {
    if (!element) return;
    if (!isc.Browser.isMoz) return isc.Browser.isBorderBox;
    
    return (element.style.MozBoxSizing == "border-box");
},

// Return the scrollHeight (scrollable height) for the element.
getScrollHeight : function (element) {

    if (element == null) return 0;

    
    var height = ((element.scrollHeight!= null && element.scrollHeight != "undefined") 
                                                ? element.scrollHeight : element.offsetHeight);

    
    var largestBottom = this._getPositionedChildrenBottom(element);

    return largestBottom > height ? largestBottom : height;
},

// get the largest bottom coordinate for any explicitly positioned DOM children of this element
_getPositionedChildrenBottom : function (element) {
    if (element.childNodes == null) return 0;

    var largest = 0,
        // constants for determining whether a DOM node is an element.  
        
        elementType = document.ELEMENT_NODE || 1,
        debug = this.logIsDebugEnabled("sizing");

    for (var i = 0; i < element.childNodes.length; i++) {
        var child = element.childNodes.item(i);
        
        // ignore anything that isn't an element (only elements report any size information)
        if (child.nodeType != elementType) continue;
        
        var childPosition = isc.Element.getComputedStyleAttribute(child, "position");
        
        // get the top coordinate of the child
        var childTop = 0;
        if (childPosition == isc.Canvas.ABSOLUTE || childPosition == isc.Canvas.RELATIVE) {
            childTop += isc.Element.getOffsetTop(child);
        } else {
            // inline content ("position" property unset).  We don't inspect this because the
            // scrollWidth reported by the element includes inline content
            continue;
        }

        
        var canvas = child.getAttribute("eventProxy"),
            childVisibleHeight;
        if (canvas != null && 
            !isc.isAn.emptyString(canvas) && 
            !window[canvas]._retrievingScrollHeight &&
            isc.isA.Function(window[canvas].getVisibleHeight)) 
        {
            childVisibleHeight = window[canvas].getVisibleHeight();
        } else {
            // For regular DOM elements call isc.Element.getVisibleHeight(element) instead
            childVisibleHeight = isc.Element.getVisibleHeight(child);
        }
        
        var childBottom = childTop + childVisibleHeight;
        // Notes:
        // - the 'visibleHeight' is the height of this child, including any margins.
        //   if this (parent) is scrollable, and the child is absolutely positioned, we 
        //   natively can't scroll to the right/bottom margin, so deduct this from the reported
        //   parent scroll-width.
        // - This child will be drawn over the top of any padding applied to the element, so we
        //   don't need to add that to the childBottom value.  Only inline elements will force
        //   the parents' padding to show up below them.
        if (childPosition == isc.Canvas.ABSOLUTE && 
            (element.style.overflow == isc.Canvas.SCROLL ||
             element.style.overflow == isc.Canvas.AUTO ||
             element.style.overflow == isc.Canvas.HIDDEN))
             childBottom -= isc.Element.getBottomMargin(child);
        
        
        
        if (childBottom > largest) largest = childBottom;

        
    }
    return largest;
},

// isc.Element.getScrollWidth(element) - See comments for getScrollHeight
getScrollWidth : function (element) {
    if (element == null) return 0;
    
    
    var width = ((element.scrollWidth != null && element.scrollWidth != "undefined") ? 
                     element.scrollWidth : element.offsetWidth);
    
    // if we have any position:absolute or position:relative children, find the right-most one
    var largestRight = this._getPositionedChildrenRight(element);

    return largestRight > width ? largestRight : width;
},

// get the largest right coordinate for any explicitly positioned DOM children of this element
_getPositionedChildrenRight : function (element) {
    if (element.childNodes == null) return 0;

    var largest = 0,
        // constants for determining whether a DOM node is an element.  
        
        elementType = document.ELEMENT_NODE || 1,
        debug = this.logIsDebugEnabled("sizing");

    for (var i = 0; i < element.childNodes.length; i++) {
        var child = element.childNodes.item(i);
        
        if (child.nodeType != elementType) continue;
        
        var childStyle = isc.Element.getComputedStyle(child, ["position", "display", "left"]);
        
        var childLeft = 0;
        if (childStyle.position == isc.Canvas.ABSOLUTE || 
            childStyle.position == isc.Canvas.RELATIVE) 
        {
            childLeft = isc.Element.getOffsetLeft(child);
        } else {
            // inline content ("position" property unset).  We don't inspect this because the
            // scrollWidth reported by the element includes inline content
            continue;
        }
 
        
        var canvas = child.getAttribute("eventProxy"),
            childVisibleWidth;
        if (canvas != null && 
            !isc.isAn.emptyString(canvas) && 
            !window[canvas]._retrievingScrollWidth &&
            isc.isA.Function(window[canvas].getVisibleWidth)) 
        {
            childVisibleWidth = window[canvas].getVisibleWidth();
        } else {
            // For regular DOM elements call isc.Element.getVisibleWidth(element) instead
            childVisibleWidth = isc.Element.getVisibleWidth(child);
        }
        
        var childRight = childLeft + childVisibleWidth;
        if (element.style.overflow == isc.Canvas.SCROLL || 
            element.style.overflow == isc.Canvas.HIDDEN ||
            element.style.overflow == isc.Canvas.AUTO) {
                childRight -= isc.Element.getRightMargin(child);
       }
        
        if (childRight > largest) largest = childRight;
 
        //>DEBUG
        if (debug) {
            this.logInfo("getChildNodesRight: child node " + i + " of " + 
                         element.childNodes.length + " (" + this.echoLeaf(child) + ")" +
                         " left:" + childLeft + ", width: " + childVisibleWidth + 
                         ", right:" + childRight, "sizing");
        }
        //<DEBUG
    }
    return largest;
},

getClientWidth : function (element) {
    
    if (isc.Browser.isIE && (isc.Browser.isIE9 || isc.Browser.version >= 10)) {
        // IE 9 Strict Mode and IE 10 have getComputedStyle().
        var cssStyleDecl = window.getComputedStyle(element, null),
            
            clientWidth = parseFloat(cssStyleDecl.width) + parseFloat(cssStyleDecl.paddingRight) +
                          parseFloat(cssStyleDecl.paddingLeft);
        return Math.ceil(clientWidth);
    } else {
        return element.clientWidth;
    }
},

getElementRect : function (element) {
    var body = this.getDocumentBody(),
        left = this.getLeftOffset(element, body), 
        top = this.getTopOffset(element, body);

    var width = 0, height = 0;
    if (element.style && element.style.overflow == "visible") {
        width = this.getScrollWidth(element);
        height = this.getScrollHeight(element);
    }
    
    width = Math.max(element.offsetWidth, element.clientWidth, width);
    height = Math.max(element.offsetHeight, element.clientHeight, height);
    return [ left, top, width, height ];
},

// get the inner width of an arbitrary dom element
// Note: we use this for widgets with htmlElement and matchElement set.
// Implementation doesn't cache results - don't use this in critical path code as it may be
// somewhat slow
getInnerWidth : function (element) {
    // assume content-box sizing (the default)
    // If specified style.width will be the available 'inner' width, excluding padding, 
    // margin, border
    // (ignore element overflow for now)
    var styleWidth = element.style.width;
    if (styleWidth != null && !isc.isAn.emptyString(styleWidth)) {
        styleWidth = parseInt(styleWidth);
        if (isc.isA.Number(styleWidth)) return styleWidth;
    }
    
    // If width is unspecified - measure how large the element rendered out.
    //
    // element.clientWidth will be the width of the element excluding border and margin,
    // but including padding. Delete the padding thickness to get the widget we want.
    var clientWidth = element.clientWidth,
        paddingLeft = parseInt(this.getComputedStyleAttribute("paddingLeft")),
        paddingRight = parseInt(this.getComputedStyleAttribute("paddingRight")),
        padding = paddingLeft + paddingRight;
    if (isc.isA.Number(padding)) clientWidth -= padding;
    return clientWidth;
},
getInnerHeight : function (element) {
    // assume content-box sizing (the default)
    // If specified style.width will be the available 'inner' width, excluding padding, 
    // margin, border
    // (ignore element overflow for now)
    var styleHeight = element.style.height;
    if (styleHeight != null && !isc.isAn.emptyString(styleHeight)) {
        styleHeight = parseInt(styleHeight);
        if (isc.isA.Number(styleHeight)) return styleHeight;
    }
    
    // If height is unspecified - measure how large the element rendered out.
    var clientHeight = element.clientHeight,
        paddingTop = parseInt(this.getComputedStyleAttribute("paddingTop")),
        paddingBottom = parseInt(this.getComputedStyleAttribute("paddingBottom")),
        padding = paddingTop + paddingBottom;
    if (isc.isA.Number(padding)) clientHeight -= padding;
    return clientHeight;
},


getNativeInnerWidth : function (element) {
    if (isc.Browser.isMoz) return this.getInnerWidth(element);
    var width = element.offsetWidth;
    // 0 or null
    if (!width) width = this.getInnerWidth(element);
    return width;
},
getNativeInnerHeight : function (element) {
    if (isc.Browser.isMoz) return this.getInnerHeight(element);
    var height = element.offsetHeight;
    // 0 or null
    if (!height) height = this.getInnerHeight(element);
    return height;
},




// Methods to get the margin sizes for an element

getTopMargin : function (element) {
    if (element != null) {
        var topMargin;
        if (element.style != null) topMargin = parseInt(element.style.marginTop);
        if (isc.isA.Number(topMargin)) return topMargin;
        if (element.className != null) return isc.Element._getTopMargin(element.className);
    }
    return 0;
},
getBottomMargin : function (element) {
    if (element != null) {
        var bottomMargin;
        if (element.style != null) bottomMargin = parseInt(element.style.marginBottom);
        if (isc.isA.Number(bottomMargin)) return bottomMargin;
        if (element.className != null) return isc.Element._getBottomMargin(element.className);
    }
    return 0;
},
getLeftMargin : function (element) {
    if (element != null) {
        var leftMargin;
        if (element.style != null) leftMargin = parseInt(element.style.marginLeft);
        if (isc.isA.Number(leftMargin)) return leftMargin;
        if (element.className != null) return isc.Element._getLeftMargin(element.className);
    }
    return 0;
},
getRightMargin : function (element) {
    if (element != null) {
        var rightMargin;
        if (element.style != null) rightMargin = parseInt(element.style.marginRight);
        if (isc.isA.Number(rightMargin)) return rightMargin;
        if (element.className != null) return isc.Element._getRightMargin(element.className);
    }
    return 0;
},

getHMarginSize : function (element) {
    return isc.Element.getLeftMargin(element) + isc.Element.getRightMargin(element);
},
getVMarginSize : function (element) {
    return isc.Element.getTopMargin(element) + isc.Element.getBottomMargin(element);
},

// element.currentStyle gives us the computed style of an element in IE / Opera
// Not available in IE9 [rendering in IE9 rendering mode]
_useCurrentStyle:(isc.Browser.isIE && !isc.Browser.isIE9) || isc.Browser.isOpera, 

getTopBorderSize : function (element) {
    if (element == null) return 0;
    if (isc.Browser.isOpera && element.currentStyle.borderTopStyle == this._$none) return 0;
    var borderSize = (this._useCurrentStyle
                        ? parseInt(element.currentStyle.borderTopWidth)
                        : parseInt(isc.Element.getComputedStyleAttribute(element, "borderTopWidth"))
                      );
                      
    return isNaN(borderSize) ? 0 : borderSize;
},

getBottomBorderSize : function (element) {
    if (element == null) return 0;
    if (isc.Browser.isOpera && element.currentStyle.borderBottomStyle == this._$none) return 0;    
    var borderSize = (this._useCurrentStyle
                        ? parseInt(element.currentStyle.borderBottomWidth)
                        : parseInt(isc.Element.getComputedStyleAttribute(element, "borderBottomWidth"))
                     );
    return isNaN(borderSize) ? 0 : borderSize;
},

getLeftBorderSize : function (element) {
    if (element == null) return 0;
    if (isc.Browser.isOpera && element.currentStyle.borderLeftStyle == this._$none) return 0;    
    var borderSize = (this._useCurrentStyle
                        ? parseInt(element.currentStyle.borderLeftWidth)
                        : parseInt(isc.Element.getComputedStyleAttribute(element, "borderLeftWidth"))
                     );
    return isNaN(borderSize) ? 0 : borderSize;
},

getRightBorderSize : function (element) {
    if (element == null) return 0;
    if (isc.Browser.isOpera && element.currentStyle.borderRightStyle == this._$none) return 0;
    var borderSize = (this._useCurrentStyle
                        ? parseInt(element.currentStyle.borderRightWidth)
                        : parseInt(isc.Element.getComputedStyleAttribute(element, "borderRightWidth"))
                      );
    return isNaN(borderSize) ? 0 : borderSize;
},

getBorderSizes: function (element) {
    var result = {
        top: isc.Element.getTopBorderSize(element),
        bottom: isc.Element.getBottomBorderSize(element),
        left: isc.Element.getLeftBorderSize(element),
        right: isc.Element.getRightBorderSize(element)
    };
    result.Top = result.top;
    result.Bottom = result.bottom;
    result.Left = result.left;
    result.Right = result.right;
    return result;
},

getVBorderSize : function (element) {
    return isc.Element.getTopBorderSize(element) + isc.Element.getBottomBorderSize(element);
},
getHBorderSize : function (element) {
    return isc.Element.getLeftBorderSize(element) + isc.Element.getRightBorderSize(element);
},

getTopPaddingSize : function (element) {
    if (element == null) return 0;
    var paddingSize = (this._useCurrentStyle
                        ? parseInt(element.currentStyle.paddingTop)
                        : parseInt(isc.Element.getComputedStyleAttribute(element, "paddingTop"))
                      );
    return isNaN(paddingSize) ? 0 : paddingSize;
},

getBottomPaddingSize : function (element) {
    if (element == null) return 0;
    var paddingSize = (this._useCurrentStyle
                        ? parseInt(element.currentStyle.paddingBottom)
                        : parseInt(isc.Element.getComputedStyleAttribute(element, "paddingBottom"))
                      );
    return isNaN(paddingSize) ? 0 : paddingSize;
},

getLeftPaddingSize : function (element) {
    if (element == null) return 0;
    var paddingSize = (this._useCurrentStyle
                        ? parseInt(element.currentStyle.paddingLeft)
                        : parseInt(isc.Element.getComputedStyleAttribute(element, "paddingLeft"))
                      );
    return isNaN(paddingSize) ? 0 : paddingSize;
},

getRightPaddingSize : function (element) {
    if (element == null) return 0;
    var paddingSize = (this._useCurrentStyle
                        ? parseInt(element.currentStyle.paddingRight)
                        : parseInt(isc.Element.getComputedStyleAttribute(element, "paddingRight"))
                      );
    return isNaN(paddingSize) ? 0 : paddingSize;
},

getVPaddingSize : function (element) {
    return isc.Element.getTopPaddingSize(element) + isc.Element.getBottomPaddingSize(element);
},
getHPaddingSize : function (element) {
    return isc.Element.getLeftPaddingSize(element) + isc.Element.getRightPaddingSize(element);
},

// getVisibleWidth / getVisibleHeight:
// when passed a DOM element, return the drawn size of the element, including any overflow, 
// border, margin or padding.
getVisibleWidth : function (element) {
    if (element == null) return 0;
    
    var overflow = isc.Element.getComputedStyleAttribute(element, "overflow"),
        width;
    if (overflow == isc.Canvas.VISIBLE || !isc.isA.Number(parseInt(element.style.width))) {
        width = isc.Element.getScrollWidth(element) + 
                isc.Element.getHBorderSize(element);

    } else {
        // use the specified width
        width = parseInt(element.style.width);
    }
    return width + isc.Element.getHMarginSize(element);
},

getVisibleHeight : function (element) {
    if (element == null) return 0;

    var overflow = isc.Element.getComputedStyleAttribute(element, "overflow"),
        height;
    if (overflow == isc.Canvas.VISIBLE || !isc.isA.Number(parseInt(element.style.height))) {
        height = isc.Element.getScrollHeight(element) + isc.Element.getVBorderSize(element);
    } else {
        // use the specified height
        height = parseInt(element.style.height);
    }
    return height + isc.Element.getVMarginSize(element);
},


// Element.getOffsetLeft()
//  Takes 'element'
//  element should be a pointer to a DOM element or the ID for a DOM element
//  (To get the offsetLeft for a widget use use widget.getOffsetLeft() instead)
//  Returns the true offsetLeft - the absolute left coordinate with respect to whatever is
//  reported by the DOM as the offsetParent of the element.
getOffsetLeft : function (element) {
 
    // Note: This method is used by the canvas instance 'getOffsetLeft()' method to calculate
    // the offset position.
    // We work with coordinates / sizes relative to the outside of any margins around our 
    // widgets - do the same with this method.
    
    if (element == null) {
        this.logWarn("getOffsetLeft: passed null element");
        return 0;
    }

    // IE and Moz both return somewhat unreliable values for element.offsetLeft by default.
    // Paper over these bugs and differences.
    var left = element.offsetLeft;
    // --- caching code:
    // If we've already calculated a value (based on a reported offsetLeft value), and
    // the reported value has not changed, return the previously calculated value.
    // This caching is safe except for cases where an indirect parent's styling changes in a
    // way that would affect this element's true offsetLeft.
    if (element._cachedReportedOffsetLeft == left) {
        return element._cachedCalculatedOffsetLeft;
    } else {
        // debug message for sanity checking coordinate caching
        //this.logWarn(element.getAttribute("eventProxy") + ": new DOM value for offsetLeft");
    }

    // always subtract the left margin (if there is one) to get the position OUTSIDE the
    // margins.
    // Note: for a negative margin, the reported offsetLeft does not need to be adjusted by the
    // specified margin size - it represents the position of the element - and in this case there
    // is no margin outside the element (rather the specified margin shifts the element to the 
    // left / up)
    var leftMargin = parseInt(isc.Element.getComputedStyleAttribute(element, "marginLeft"));
    if (isc.isA.Number(leftMargin) && leftMargin > 0) {
        left -= leftMargin;
    }
    
    
    var documentBody = this.getDocumentBody(),
        parentStyle,
        px = "px",
        // determine whether the element is absolutely / relatively / etc. positioned    
        elementPosition = element.style.position;

    // Workarounds for Moz        
    if (isc.Browser.isMoz) {
        // In moz we get some unexpected results
        
        if (element.offsetParent == null) return left;
        
        if (element.offsetParent != documentBody) {

            parentStyle = 
                this.ns.Element.getComputedStyle(element.offsetParent, ["borderLeftWidth", "overflow"]);       

            // The behavior changes with different releses of Moz / Firefox
            var geckoVersion = isc.Browser.geckoVersion,
            
                
                scrollingAdjustment = geckoVersion < 20100101 &&
                                     ((parentStyle.overflow != "visible") &&
                                      (geckoVersion >= 20051111 || 
                                      (elementPosition == isc.Canvas.ABSOLUTE && parentStyle.overflow != "hidden"))),
                
                accountForBorderBox = (geckoVersion > 20020826 &&
                                        (element.offsetParent.style.MozBoxSizing == "border-box"));

            
            if (isc.Browser.version < 8 && accountForBorderBox != scrollingAdjustment) {

                
                if (accountForBorderBox) {
                    left -= (isc.isA.Number(parseInt(parentStyle.borderLeftWidth)) ?
                                            parseInt(parentStyle.borderLeftWidth) : 0);
                                            
                } 
                
                if (scrollingAdjustment) {
                    left += (isc.isA.Number(parseInt(parentStyle.borderLeftWidth)) ?
                                            parseInt(parentStyle.borderLeftWidth) : 0);
                                            
                }
            }
                  
        }
    }
    
    // Workarounds for IE
     
    
    if (isc.Browser.isIE && !isc.Browser.isIE8Strict && !isc.Browser.isIE9) {
    
        

        var currentParent = element.offsetParent,
            parentStyle;
        if (parentStyle != documentBody) parentStyle = currentParent.currentStyle; 
        
        
        var hasSpecifiedSize = (element.currentStyle.height != isc.Canvas.AUTO ||
                                element.currentStyle.width != isc.Canvas.AUTO);

        
        var continueDeductingBorders = true;                                
        
        // iterate up the offsetParents till we reach the doc. body
        while (currentParent != documentBody) {

            
            
            
            if (parentStyle.position == isc.Canvas.ABSOLUTE) continueDeductingBorders = false;
            
            
            if (parentStyle.width == isc.Canvas.AUTO && 
                parentStyle.height == isc.Canvas.AUTO &&
                parentStyle.position == isc.Canvas.RELATIVE) {
                
                
                if (continueDeductingBorders &&
                    isc.isA.String(parentStyle.borderLeftWidth) && 
                    parentStyle.borderLeftWidth.contains(px)        ) {
                        left -= parseInt(parentStyle.borderLeftWidth);
                }    
                
                
                if (hasSpecifiedSize) {
                
                    if (isc.isA.String(parentStyle.marginLeft) && 
                        parentStyle.marginLeft.contains(px)) 
                    {
                        var parentMarginLeft = parseInt(parentStyle.marginLeft);
                        if (parentMarginLeft > 0) left -= parentMarginLeft;
                    }                           
                
                    
                    if (currentParent.offsetParent != documentBody) {
                        
                        var superPadding = currentParent.offsetParent.currentStyle.padding;
                        if (isc.isA.String(superPadding) && superPadding.contains(px)) {
                            left -= parseInt(superPadding);
                        }
                    } else {    
                        
                        left -= (documentBody.leftMargin ? parseInt(documentBody.leftMargin) : 0);                
                    }
                } 
                
            } // end of if

            
            elementPosition = currentParent.style.position;
            currentParent = currentParent.offsetParent;
            if (currentParent != document.body) {
                parentStyle = currentParent.currentStyle;
            }
            
        }   // End of while loop
        
    }        
    
    // Workarounds for Safari
    if (isc.Browser.isSafari && isc.Browser.safariVersion < 525.271) {
        // In some versions of Safari, if the offsetParent has a border, the offsetLeft / top
        // reported is relative to the outside of that border, rather than the inside, so deduct
        // that value
        // No longer the case in Safari 3.2.1 (525.27.1)
        if (element.offsetParent != null && element.offsetParent != documentBody) {
            var parentBorder = 
                this.ns.Element.getComputedStyle(element.offsetParent, ["borderLeftWidth"]).borderLeftWidth;
            if (parentBorder != null) parentBorder = parseInt(parentBorder);
            if (isc.isA.Number(parentBorder)) left -= parentBorder;
        }

    } else if (isc.Browser.isOpera && isc.Browser.version >= 12 && element.offsetParent) {
        if (element.offsetParent.id.endsWith("_clipDiv")) {
            var parentBCR = element.offsetParent.getBoundingClientRect(),
                bcr = element.getBoundingClientRect();
            left = bcr.left - parentBCR.left;
        }
    }

    // --- cacheing code:
    // Cache the calculated and reported value, by saving it as attributes on the DOM element
    element._cachedReportedOffsetLeft = element.offsetLeft;
    element._cachedCalculatedOffsetLeft = left;
    
    return left;
},

// Element.getOffsetTop()
//  Takes 'element' 
//  element should be a pointer to a DOM element or the ID for a DOM element (doesn't 
//  handle getting a widget ID - in that case use widget.getOffsetTop() instead)
//  Returns the true offsetTop - the absolute top coordinate with respect to (the inside of any
//  border of) whatever is reported by the DOM as the offsetParent of the element.

getOffsetTop : function (element) {
    // In theory the value element.offsetTop should be what we want here. However it is
    // unreliable in a number of ways.
    if (element == null) {
        this.logWarn("getOffsetTop: passed null element");
        return 0;
    }
    
    // IE and Moz both return somewhat unreliable values for element.offsetTop by default.
    // Paper over these bugs and differences.
    var top = element.offsetTop;  // This is what we'd return if the browsers worked correctly!

    
    if (isc.Browser.isFirefox && isc.Browser.isStrict && top < 0) {
        var parent = element.offsetParent;
        if (parent != null) {
            if (parent.getBoundingClientRect().top ==
                element.getBoundingClientRect().top) top = 0;
        }
    }

    // --- caching code:
    // If we've already calculated a value (based on a reported offsetTop value), and
    // the reported value has not changed, return the previously calculated value.
    if (element._cachedReportedOffsetTop == top) {
       return element._cachedCalculatedOffsetTop;
    } else {
        // debug message for sanity checking coordinate caching
        //this.logWarn(element.getAttribute("eventProxy") + ": new DOM value for offsetTop");
    }

    // The reported offsetTop is the offset from the element, INSIDE of margins to the
    // offsetParent - if we have a top margin we should subtract it to get the position OUTSIDE
    // the margins.
    // Exception: If the margin is negative, we don't need to adjust for it. In this case the
    // reported offset is still to the outside of the element, even though the element is 
    // essentially shifted above where it would normally appear.
    
    var topMargin = parseInt(isc.Element.getComputedStyleAttribute(element, "marginTop"));
    if (isc.isA.Number(topMargin) && topMargin > 0) {
        top -= topMargin;
    }
    
    var documentBody = this.getDocumentBody(),
        parentStyle,
        px = "px",
        elementPosition = element.style.position;

    // Workarounds for Moz        
    if (isc.Browser.isMoz) {
        
        
        
        if (element.offsetParent == null) return top;
        if (element.offsetParent != documentBody) {

            // get the offsetParent's style info
            parentStyle = this.ns.Element.getComputedStyle(element.offsetParent, ["overflow", "borderTopWidth"]);
            
            var geckoVersion = isc.Browser.geckoVersion;

            var scrollingAdjustment = geckoVersion < 20100101 &&
                                     ((parentStyle.overflow != "visible") &&
                                      (geckoVersion >= 20051111 || 
                                      (elementPosition == isc.Canvas.ABSOLUTE && parentStyle.overflow != "hidden"))),
                accountForBorderBox = (isc.Browser.geckoVersion > 20020826 && 
                                        element.offsetParent.style.MozBoxSizing == "border-box");
            
            if (isc.Browser.version < 8 && accountForBorderBox != scrollingAdjustment) {
                if (accountForBorderBox) {
                    top -= (isc.isA.Number(parseInt(parentStyle.borderTopWidth)) ?
                                            parseInt(parentStyle.borderTopWidth) : 0);

                } 
                if (scrollingAdjustment) {
                    top += (isc.isA.Number(parseInt(parentStyle.borderTopWidth)) ?
                                            parseInt(parentStyle.borderTopWidth) : 0);
                }
            }
        }
    }
    
    // Workarounds for IE
    
    if (isc.Browser.isIE && !isc.Browser.isIE9) {
        
        if (element.offsetParent && element.offsetParent != documentBody) {

            parentStyle = element.offsetParent.currentStyle;  
            
            
            if (    parentStyle.position == isc.Canvas.RELATIVE &&  
                    parentStyle.height == isc.Canvas.AUTO && 
                    parentStyle.width == isc.Canvas.AUTO &&  
                    isc.isA.String(parentStyle.borderTopWidth) && 
                    parentStyle.borderTopWidth.contains(px)         ) {
                        top -= parseInt(parentStyle.borderTopWidth);
            }
        }
    }    
    
    // Workarounds for Safari
    if (isc.Browser.isSafari && isc.Browser.safariVersion < 525.271) {
        // As noted in 'getOffsetLeft()', in Safari the width of the parent's border is included
        // in the offsetLeft/top value reported.
        if (element.offsetParent && element.offsetParent != documentBody) {
            var parentBorder = 
                this.ns.Element.getComputedStyle(element.offsetParent, 
                                                ["borderTopWidth"]).borderTopWidth;
            
            if (parentBorder != null) parentBorder = parseInt(parentBorder);
            if (isc.isA.Number(parentBorder)) top -= parentBorder;
        }

    } else if (isc.Browser.isOpera && isc.Browser.version >= 12 && element.offsetParent) {
        if (element.offsetParent.id.endsWith("_clipDiv")) {
            var parentBCR = element.offsetParent.getBoundingClientRect(),
                bcr = element.getBoundingClientRect();
            top = bcr.top - parentBCR.top;
        }
    }

    // --- cacheing code:
    // Cache the calculated and reported value, by saving it as attributes on the DOM element
    element._cachedReportedOffsetTop = element.offsetTop;
    element._cachedCalculatedOffsetTop = top;
   
    return top;

},

// getLeftOffset(element, targetElement, rtl)
//
// DOM Only method to return the absolute (offset) position for some element within some other 
// DOM parent element.  We will return this value from the outside of any border / margin on
// the child to the inside of the ancestor element.
//

getLeftOffset : function (element, targetElement, rtl, canvasArgs) {
    return this.getOffset(isc.Canvas.LEFT, element, targetElement, rtl, canvasArgs);
},

// Return the absolute position of an element within a DOM parent element.
// If no target parent element is passed, we return page level position.
getTopOffset : function (element, targetElement, canvasArgs) {
    return this.getOffset(isc.Canvas.TOP, element, targetElement, null, canvasArgs);
},


_$borderLeftWidth: "borderLeftWidth",
_$borderTopWidth: "borderTopWidth",
_$marginLeft: "marginLeft",
_$marginTop: "marginTop",
_$none:"none",



_$leftCoords: "_$leftCoords",
_$topCoords: "_$topCoords",

_$BODY: "BODY",
_$HTML: "HTML",
_isDocElemBCROkay: !isc.Browser.isIE && !isc.Browser.isOpera,
getBoundingClientRect : function (element) {
    
    var isDocElemBCROkay = this._isDocElemBCROkay,
        isIEQuirks = isc.Browser.isIE && !isc.Browser.isStrict;
    if (element.tagName == this._$BODY ||
        (!isDocElemBCROkay && element.tagName == this._$HTML))
    {
        var doc = element.ownerDocument,
            docElem = doc.documentElement,
            win = doc.defaultView || window;
        if (isDocElemBCROkay) {
            return docElem.getBoundingClientRect();
        } else if (isIEQuirks && isc.Browser.version == 6) {
            
            var bcr = element.getBoundingClientRect(),
                left = -doc.body.scrollLeft + (docElem.clientLeft || 0),
                top = -doc.body.scrollTop + (docElem.clientTop || 0),
                width = Math.max(docElem.offsetWidth, bcr.right),
                height = Math.max(docElem.offsetHeight, bcr.bottom);
            return {
                left: left,
                top: top,
                right: left + width,
                bottom: top + height,
                width: width,
                height: height
            };
        } else {
            var bcr = element.getBoundingClientRect(),
                
                width = Math.max(docElem.clientWidth, bcr.right),
                height = Math.max(docElem.clientHeight, bcr.bottom),
                left = docElem.clientLeft || 0,
                top = docElem.clientTop || 0;
            if (isIEQuirks) {
                left -= doc.body.scrollLeft;
                top -= doc.body.scrollTop;
            } else {
                left -= win.pageXOffset || docElem.scrollLeft;
                top -= win.pageYOffset || docElem.scrollTop;
            }
            return {
                left: left,
                top: top,
                right: left + width,
                bottom: top + height,
                width: width,
                height: height
            };
        }
    } else if (isIEQuirks && isc.Browser.version == 6) {
        
        var bcr = element.getBoundingClientRect();
        return {
            left: bcr.left,
            top: bcr.top,
            right: bcr.right,
            bottom: bcr.bottom,
            width: bcr.right - bcr.left,
            height: bcr.bottom - bcr.top
        };
    } else {
        return element.getBoundingClientRect();
    }
},

//> @object ElementOffsets
//<
//> @attr elementOffsets.left (double : : R)
//<
//> @attr elementOffsets.top (double : : R)
//<

// cacheOffestCoordinates: If set we will cache calculated offsets between SmartClient
// ancestor/descendent components.
// This improves performance on repeated lookups during (EG) drag/drop, etc

cacheOffsetCoords:true,

//> @classMethod element.getOffsets() [A]
// @param (DOMElement or Canvas) sourceElement
// @param (DOMElement or Canvas) origTargetElement
// @param (Boolean) rtl
// @return (ElementOffsets)
//<
getOffsets : function (sourceElement, origTargetElement, rtl, canvasArgs) {
    var sourceIsCanvas = canvasArgs || isc.isA.Canvas(sourceElement),
        targetIsCanvas = canvasArgs || origTargetElement == null || isc.isA.Canvas(origTargetElement);

    var cacheCoords = sourceIsCanvas && targetIsCanvas && this.cacheOffsetCoords && 
                      (sourceElement.cacheOffsetCoords != false),
        targetID = origTargetElement ? origTargetElement.ID : this._$none;
    if (cacheCoords && sourceElement[this._$leftCoords] != null &&
        sourceElement[this._$topCoords] != null)
    {
        var cachedLeftCoord = sourceElement[this._$leftCoords][targetID],
            cachedTopCoord = sourceElement[this._$topCoords][targetID];
        if (cachedLeftCoord != null && cachedTopCoord != null) {
            
            return {
                left: cachedLeftCoord,
                top: cachedTopCoord
            };
        }
    }
    

    var element = sourceIsCanvas ? sourceElement.getClipHandle() : sourceElement;
    var targetElement;

    // if we're not passed an element, determine the offset from the top level HTML element.
    if (origTargetElement == null) targetElement = this.getDocumentBody();
    
    else if (targetIsCanvas) targetElement = origTargetElement.getHandle();
    else targetElement = origTargetElement;

    //!DONTCOMBINE
    //>DEBUG
    if (targetElement == null || element == null) {
        
        return {
            left: 0,
            top: 0
        };
    }
    //<DEBUG

    var offsets;

    if (isc.Browser._hasGetBCR) {
        
        var elementBCR = this.getBoundingClientRect(element),
            targetElementBCR = this.getBoundingClientRect(targetElement);
        
        offsets = {
            left: (elementBCR["left"] - targetElementBCR["left"]) << 0,
            top: (elementBCR["top"] - targetElementBCR["top"]) << 0
        };
    } else {
        var nextParent = element.offsetParent;

        
        if (isc.Browser.isMoz && nextParent == null) return 0;


        // To get the offsetLeft / Top with respect to the passed in targetElement,
        // iterate through the offsetParents, summing 'offsetLeft' until we reach the targetElement.
        // If we reach the targetElement's offsetParent before we hit the targetElement we've jumped
        // over the target - this is Ok - just deduct the offsetLeft of the targetElement to adjust
        // for it.
        // For each iteration adjust for scrolling and border / margin thickness
        // (see comments in the while loop below).
        var targetParent = targetElement.offsetParent,
            currentNode = element;

        offsets = {
            left: 0,
            top: 0
        };

        if (rtl == null) rtl = isc.Page.isRTL();

        // iterate up until we reach the targetElement, or the targetElement's offsetParent
        // We could also check for documentBody to avoid crashing in the case where we were
        // passed bad params.
        var iterations = 0;
        while (nextParent != targetElement && nextParent != targetParent) {

            this._adjustOffsets(offsets, currentNode, nextParent, rtl);

            // Move up the DOM chain
            currentNode = nextParent;
            nextParent = currentNode.offsetParent;
            iterations++;
        }

        // At this point the nextParent is either the target or its offsetParent.
        this._adjustOffsets(offsets, currentNode, nextParent, rtl);

        // OffsetLeft from the last iteration was relative to the target's offsetParent -
        // deduct the target's offsetLeft to get the offset relative to the target instead.
        if (nextParent == targetParent) {
            // deduct the targetElement's offsetLeft
            // No need to adjust for border / padding in this case
            offsets.left -= this.getOffsetLeft(targetElement);
            offsets.top -= this.getOffsetTop(targetElement);
        }

        
    }

    if (cacheCoords) {
        var coordCache = sourceElement[this._$leftCoords] = sourceElement[this._$leftCoords] || {};
        coordCache[targetID] = offsets.left;
        coordCache = sourceElement[this._$topCoords] = sourceElement[this._$topCoords] || {};
        coordCache[targetID] = offsets.top;
    }

    return offsets;
},

_adjustOffsets : function (offsets, currentNode, nextParent, rtl) {
    // Add the currentNode's offsetLeft/top - left w.r.t. its offsetParent
    offsets.left += this.getOffsetLeft(currentNode);
    offsets.top += this.getOffsetTop(currentNode);

    // The offsetLeft/top value is relative to the content of the parent's element - so if
    // the parent is scrolled, and we want the floating position of this element within
    // its parent we have to deduct the scrollLeft of the parent to page coordinate
    // relative to the parent's element's top/left
    if (!rtl) {
        // deduct the scrollLeft
        offsets.left -= nextParent.scrollLeft || 0;
    } else {
        // rtl var is only ever true when calculating left offset.
        
        

        var scrollLeft = nextParent.scrollLeft;
        if (isc.isA.Number(scrollLeft)) {
            var overflow = nextParent.style ? nextParent.style.overflow : isc.emptyString;
            if (overflow != isc.Canvas.VISIBLE && overflow != isc.emptyString) {
                var nativeScrollNegativeOrigin = this._nativeScrollNegativeOrigin,
                    nativeScrollInverse = this._nativeScrollInverse;

                // inverse - we can just flip the polarity of the reported scrollLeft to give
                // negative origin.
                if (nativeScrollInverse) {
                    scrollLeft = -scrollLeft;
                    nativeScrollNegativeOrigin = true;

                }
                // convert negative origin to zero origin (so zero = scrolled hard left)
                if (!nativeScrollNegativeOrigin) {
                    scrollLeft = isc.Canvas._adjustScrollLeftForRTL(
                                    scrollLeft, nextParent.scrollWidth,
                                    nextParent.clientWidth,
                                    // param indicates we're currently in zero origin coords and
                                    // want to move to negative origin
                                    true, true);
                }

                offsets.left -= scrollLeft;
            }
        }
    }

    offsets.top -= nextParent.scrollTop || 0;

    

    // add the border / margin thickness, because when we add the parent's offsetLeft
    // this will be the distance from the OUTSIDE of this element's border/margin
    // to the inside of the next parent's element.
    // Note: Skip this if the margin is negative as in this case the value we have from
    // getOffsetTop() / Left() is actually relative to the outside of the element
    var styleObj, borderLeftWidth, borderTopWidth, marginLeft, marginTop;
    if (this._useCurrentStyle) {
        
        styleObj = nextParent.currentStyle;

        
        if (!isc.Browser.isOpera || styleObj.borderLeftStyle != this._$none) {
            borderLeftWidth = parseInt(styleObj.borderLeftWidth);
            if (borderLeftWidth > 0) offsets.left += borderLeftWidth;
        }
        if (!isc.Browser.isOpera || styleObj.borderTopStyle != this._$none) {
            borderTopWidth = parseInt(styleObj.borderTopWidth);
            if (borderTopWidth > 0) offsets.top += borderTopWidth;
        }

        
        marginLeft = parseInt(styleObj.marginLeft);
        if (marginLeft > 0) offsets.left += marginLeft;
        marginTop = parseInt(styleObj.marginTop);
        if (marginTop > 0) offsets.left += marginLeft;
    } else if (isc.Browser.isMoz) {
        
        styleObj = window.getComputedStyle(nextParent, null);

        offsets.left += parseInt(styleObj.borderLeftWidth);
        offsets.top += parseInt(styleObj.borderTopWidth);

        marginLeft = parseInt(styleObj.marginLeft);
        if (marginLeft > 0) offsets.left += marginLeft;
        marginTop = parseInt(styleObj.marginTop);
        if (marginTop > 0) offsets.top += marginTop;
    } else {
        borderLeftWidth = parseInt(this.getComputedStyleAttribute(nextParent,
                                                                  this._$borderLeftWidth));
        if (borderLeftWidth > 0) offsets.left += borderLeftWidth;
        borderTopWidth = parseInt(this.getComputedStyleAttribute(nextParent,
                                                                 this._$borderTopWidth));
        if (borderTopWidth > 0) offsets.top += borderTopWidth;

        marginLeft = parseInt(this.getComputedStyleAttribute(nextParent,
                                                             this._$marginLeft));
        if (marginLeft > 0) offsets.left += marginLeft;
        marginTop = parseInt(this.getComputedStyleAttribute(nextParent,
                                                            this._$marginTop));
        if (marginTop > 0) offsets.top += marginTop;
    }
},


getOffset : function (dir, sourceElement, origTargetElement, rtl, canvasArgs) {
    var offsets = this.getOffsets(sourceElement, origTargetElement, rtl, canvasArgs);
    return offsets[dir];
},

// one-time flags for RTL mode coordinate calculations
_nativeScrollNegativeOrigin:(isc.Browser.isMoz || (isc.Browser.isIE && isc.Browser.minorVersion < 5.5)),
_nativeScrollInverse:(isc.Browser.isIE9 && isc.Browser.isStrict),



// ----------------------------------------------------------------------------------------
// CSS / Styling Lookups
// ----------------------------------------------------------------------------------------
// Retrieval of CSS style declaration and computed styles

// Styling: what we need and why:
// We need to be able to look up border, padding and margin sizes in order to:
// - compensate for errors in reported offsetLeft / offsetTop to have correct page-level
//   coordinates when relatively positioned (needed for arbitrary containing elements not
//   created by ISC)
// - when using the CSS standard box model, be able to write HTML that will render with
//   predictable sizes even when using author-specified CSS styling.  This is critical for grid
//   cell rendering, where correcting sizes after draw is not even close to feasible.
// - get correct scrollHeight when allowing natively positioned children



// cache of CSS style objects
_styleCache:{},
// get the edge widths (border, margin, padding) for a CSS style
getStyleEdges : function (className) {
    
    if (isc.Browser.isSafari && !isc.Element._safariStrictChecked) {
        isc.Browser.isStrict = isc.Element._testForSafariStrictMode();
        isc.Element._safariStrictChecked = true;
    }
    

    if (className == null) return null;

    // check whether cache value is defined, so we can cache failed lookups as nulls
    var undef;
    if (this._styleCache[className] !== undef) return this._styleCache[className];

    //this.logWarn("style lookup: " + className + this.getStackTrace());

    
    var cantDeriveStyles = (isc.Browser.isMoz && isc.Browser.geckoVersion < 20040616),
        styleInfo;
    
    if (cantDeriveStyles) {
        styleInfo = this.getStyleDeclaration(className);
    } else {
        
        var mask = isc.Browser.isIE ? this._styleEdgeMaskArray : this._styleEdgeMask;
        styleInfo = this._deriveStyleProperties(className, mask);
    }

    
    this._styleCache[className] = styleInfo;
    return styleInfo;
},

// In Safari, document.compatMode is not available, so we rely on the fact that 
// table cells render their padding outside their specified height in strict mode but not in
// normal compat mode to determine whether we're currently in strict mode
_testForSafariStrictMode : function () {
    if (document.compatMode != null) {
        
        return document.compatMode == "CSS1Compat";
    }
    
    var tableHTML = "<TABLE cellspacing=0 cellpadding=2 border=0><tr><td height=30>x</td></tr></TABLE>"
    
    var tester = isc.Element.createAbsoluteElement(tableHTML);
    
    var isStrict = tester.offsetHeight > 30;
    isc.Element.clear(tester);
    
    return isStrict;
},
  
// get certain key properties of a style by applying it to an element and inspecting that
// element.  Edge-related properties are reliably derivable this way, cssText is known not
// available, other properties would need testing.

_deriveStyleProperties : function (className, mask) {
    
    var requiresDivTester = (isc.Browser.isIE || isc.Browser.isOpera || isc.Browser.isSafari
                                || (isc.Browser.isMoz && isc.Browser.geckoVersion >=20080205));
    if (!this._cellStyleTester) {
        
        this.createAbsoluteElement(
            "<TABLE CELLPADDING=81 STYLE='position:absolute;left:0px;top:-2000px;'><TR><TD " +
            
            //(isc.Browser.isSafari ? "style='position:absolute;left:0px;top:0px;' " : "") +
            (isc.Browser.isIE8Strict ? 
            " ID=isc_cellStyleTester STYLE='border:0px;margin:0px'><DIV ID=isc_cellInnerStyleTester>" +
                isc.Canvas.blankImgHTML(30,30) + "</DIV></TD>"                
            :
            " ID='isc_cellStyleTester'>&nbsp;</TD><TD ID='isc_cellNoStyleTester'>&nbsp;</TD>"
            ) +
            "</TR></TABLE>"
        );
        this._cellStyleTester = isc.Element.get("isc_cellStyleTester");
        if (isc.Browser.isIE8Strict) {
            this._cellInnerStyleTester = isc.Element.get("isc_cellInnerStyleTester");
        }
        // we set the table cellPadding to 81px - this will then be reported back
        // if the padding on the style was unset (allows us to differentiate between
        // null and explicit zero)
        
        this._$81px = "81px";
        if (isc.Browser.isSafari || isc.Browser.isChrome) {
            var noStyleElement = isc.Element.get("isc_cellNoStyleTester");
            var paddingLeft = ["paddingLeft"];
            var reported81 = this.getComputedStyle(noStyleElement, paddingLeft).paddingLeft;
            if (reported81 != this._$81px) {
                this.logDebug("Browser natively misreporting cell-padding (81px reported as:"
                        + reported81 + "). This behavior is known to occur when the view is " +
                        "zoomed in certain browsers but is worked around by SmartClient and " +
                        "should have no visible effect on the application.", "sizing");
                this._$81px = reported81;
            }
        }
        this._$16384px = "-16384px";       

        if (requiresDivTester) {
            this.createAbsoluteElement(
                "<DIV ID=isc_styleTester STYLE='position:absolute;left:0px;top:-2000px;'>&nbsp;</DIV>"
            );
            this._styleTester = isc.Element.get("isc_styleTester");
            this._marginMask = ["marginLeft", "marginTop", "marginRight", "marginBottom"];
            if (isc.Browser.isIE8Strict) {
                this._marginMask.addList(["borderLeftWidth", "borderTopWidth", 
                                          "borderRightWidth", "borderBottomWidth"]);
            }
        }
    }

    this._cellStyleTester.className = className;
    var style = this.getComputedStyle(this._cellStyleTester, mask);

    //this.logWarn(className + " style is: " + this.echo(style));

    // test for unset padding 
    var nullIndicator = this._$81px;
    if (style.paddingLeft == nullIndicator) style.paddingLeft = null;
    if (style.paddingTop == nullIndicator) style.paddingTop = null;
    if (style.paddingRight == nullIndicator) style.paddingRight = null;
    if (style.paddingBottom == nullIndicator) style.paddingBottom = null;

    if (isc.Browser.isIE8Strict) {
        var innerTester = this._cellInnerStyleTester,
            offsetLeft = innerTester.offsetLeft,
            offsetTop = innerTester.offsetTop;
        
        if (offsetLeft == 81) style.paddingLeft = null;
        if (offsetTop == 81) style.paddingTop = null;
        if (this._cellStyleTester.offsetWidth - offsetLeft - 30 == 81) {
            style.paddingRight = null;
        }
        if (this._cellStyleTester.offsetHeight - offsetTop - 30 == 81) {
            style.paddingBottom = null;
        }
    }

    if (isc.Browser.isSafari) {
        // older Safari versions report unset padding as "auto" instead of reporting the
        // cellPadding
        if (isc.Browser.safariVersion < 419.3) {
            nullIndicator = isc.Canvas.AUTO;
            if (style.paddingLeft == nullIndicator) style.paddingLeft = null;
            if (style.paddingTop == nullIndicator) style.paddingTop = null;
            if (style.paddingRight == nullIndicator) style.paddingRight = null;
            if (style.paddingBottom == nullIndicator) style.paddingBottom = null;
        }

        // serious bug introduced in Safari 419.3 / 2.0.4, aka Tiger update 10.4.7: unset
        // marginTop/Bottom on cells reported as "-16384px".  Chimp factor 9.89
        nullIndicator = this._$16384px;        
        if (style.marginTop == nullIndicator) style.marginTop = null;
        if (style.marginBottom == nullIndicator) style.marginBottom = null;
    }


    
    if (requiresDivTester) {
        this._styleTester.className = className;
        var results = this.getComputedStyle(this._styleTester, this._marginMask);
        style.marginLeft = results.marginLeft;
        style.marginRight = results.marginRight;
        style.marginTop = results.marginTop;
        style.marginBottom = results.marginBottom;
        if (isc.Browser.isIE8Strict) {
            style.borderLeftWidth = results.borderLeftWidth;
            style.borderRightWidth = results.borderRightWidth;
            style.borderTopWidth = results.borderTopWidth;
            style.borderBottomWidth = results.borderBottomWidth;
        }
    }
    return style;

},

//> @classMethod Element.getComputedStyle()
//  Returns an object containing the current (computed) style for a DOM element.  This object 
//  includes all the attributes set directly on the element's style property, and those inherited
//  from the element's CSS class.
//  @param  ID  (string | object)   element, or ID of the element
//  @param  mask    (array)         list of propertyNames to include in the returned object
//  @return (object)    object containing computed style attributes.
//<
getComputedStyle : function (ID, mask) {
    
    var element, style, styleInfo;
    
    if (isc.isA.String(ID)) {
        element = isc.Element.get(ID);
    } else {
        // Otherwise just assume the DOM element was passed in directly
        element = ID;
    }

    if (element == null || !isc.isAn.Object(element)) {
        //>DEBUG
        this.logWarn("getComputedStyle: Unable to get to DOM element specified by '" + ID + "'." + this.getStackTrace());
        //<DEBUG
        return null;
    }
    
    if (this._useCurrentStyle) {
        
        style = element.currentStyle;
        // NOTE: use Array form of mask, faster with applyMask
        if (mask == null) mask = this._styleFullMaskArray;
        var results = isc.applyMask(style, mask);
        return results;
    } 

    // prepare a mask from camelCaps property to CSS dashed-property-name, because we want to
    // return camelCaps'd values but native getPropertyValue() uses dashed versions
    if (mask == null) { 
        // retrieve all properties
        mask = this._styleFullMask;
    } else if (isc.isAn.Array(mask)) {
        // if we have an explicit list of properties to retrieve, build a mask of camelCaps
        // name to CSS standard name (dash-separated) for just the desired properties.
        var obj = {},
            fullMask = this._styleFullMask;
        for (var i = 0; i < mask.length; i++) {
            obj[mask[i]] = fullMask[mask[i]];
        }
        mask = obj;
    }
    
    
    var safariPre13 = isc.Browser.isSafari && isc.Browser.safariVersion < 312,
        classStyleObject;
    if (safariPre13) {
        style = element.style;
        classStyleObject = this.getStyleDeclaration(element.className);

        styleInfo = {};
        for (var property in mask) {
            styleInfo[property] = style.getPropertyValue(mask[property]);
            
            if (styleInfo[property] == null &&
                classStyleObject != null && classStyleObject[property] != null &&
                !isc.isAn.emptyString(classStyleObject[property])) 
            { 
                
                styleInfo[property] = classStyleObject[property];
            }
        }
    } else {
        if (document.defaultView && document.defaultView.getComputedStyle) {
        style = document.defaultView.getComputedStyle(element, null);

        }
        if (style == null) {
            style = {};
            this.logWarn("getComputedStyle() not natively available - can't " +
                "guarantee accurate styling information for element:" + this.echoLeaf(element));
            if (this.logIsDebugEnabled()) {
                this.logDebug(this.getStackTrace());
            }
        }
        styleInfo = {};
        for (var property in mask) {
            styleInfo[property] = style[property];
        }
    }

    //this.logWarn("styleInfo for style: " + className + " is: " + this.echo(styleInfo));
    return styleInfo;
    
},

// return an individual attribute from the computed style.  Quicker than getting the full set
// of properties if you need only one.

_$operaBorderStyles:{
    border:"borderStyle", 
    borderWidth:"borderStyle", 
    borderLeft:"borderLeftStyle", 
    borderRight:"borderRightStyle", 
    borderTop:"borderTopStyle", 
    borderBottom:"borderBottomStyle",
    borderLeftWidth:"borderLeftStyle", 
    borderRightWidth:"borderRightStyle", 
    borderBottomWidth:"borderBottomStyle", 
    borderTopWidth:"borderTopStyle"
},
getComputedStyleAttribute : function (element, property) {

    if (element == null || property == null) return null;
    
    // Use currentStyle for IE (easy!)
    if (this._useCurrentStyle) {

        // we've seen element.currentStyle be reported as null (not sure why)
        if (element.currentStyle == null) return null;
        // special opera logic for undefined borders returning 3
        if (isc.Browser.isOpera && this._$operaBorderStyles[property] != null &&
            element.currentStyle[this._$operaBorderStyles[property]] == this._$none) return 0;
        return element.currentStyle[property];
    }

    //>Safari
    if (isc.Browser.isSafari && isc.Browser.version < 5) {
        
        var propertyValue = null;
        if (element.style) propertyValue = element.style[property];
        if ((propertyValue == null || isc.isAn.emptyString(propertyValue)) &&
            element.className)
        {
            var styleDecl = isc.Element.getStyleEdges(element.className);
            if (styleDecl) propertyValue = styleDecl[property];
        }
        if (isc.isAn.emptyString(propertyValue)) return null;
        return propertyValue;            
    } //<Safari
    
    // DOM and not broken (eg Moz).  Convert camelCaps to the CSS property name (only works for
    // a specific list of props)
    var mask = this._styleFullMask;
        
    var docView = this._docView = this._docView || document.defaultView;

    var cssProperty = (mask[property] || property),
        // get the style object for the element
        style = docView.getComputedStyle(element, null);
        
       
    return style.getPropertyValue(cssProperty);
},


_nonnativeRangeGetBoundingClientRectImpl : function (handle) {
    var handleBCR = handle.getBoundingClientRect();

    var top = handleBCR.top + isc.Element.getTopBorderSize(handle);
    var left = handleBCR.left + isc.Element.getLeftBorderSize(handle);

    // Create a new object because the attributes of a `ClientRect' object are read-only.
    // http://www.w3.org/TR/cssom-view/#the-clientrect-interface
    // We start with an empty rect.
    handleBCR = {
        top: top,
        right: left,
        bottom: top,
        left: left
    };

    // `textRange' is used to calculate the bounding client rect of text nodes.
    var textRange = null;

    if (handle.firstChild) {
        // If the first or last child node is a BR element, insert a temporary &nbsp;.
        // These nbsps are removed at the end.
        var beginTempNbspChild = null,
            endTempNbspChild = null;
        if (handle.firstChild.tagName == "BR") {
            handle.insertAdjacentHTML("afterbegin", "&nbsp;");
            beginTempNbspChild = handle.firstChild;
        }
        if (handle.lastChild.tagName == "BR" &&
            ((beginTempNbspChild != null && handle.firstChild.nextSibling != handle.lastChild) ||
             (beginTempNbspChild == null && handle.firstChild != handle.lastChild)))
        {
            handle.insertAdjacentHTML("beforeend", "&nbsp;");
            endTempNbspChild = handle.lastChild;
        }

        try {
            var childNodes = handle.childNodes,
                childNodes_length = childNodes.length;
            for (var i = 0; i < childNodes_length; ++i) {
                var child = childNodes[i];
                var bcr;
                if (child.nodeType == 1 /* ELEMENT_NODE */) {
                    bcr = child.getBoundingClientRect();

                    // If the bounding client rect of the element is an empty `ClientRect', discard it.
                    // This happens for BR elements in WebKit and Opera, as well as elements that are
                    // display:none.
                    if (bcr.top == 0 && bcr.left == 0 && bcr.width == 0 && bcr.height == 0) {
                        continue;
                    }

                    // Factor in the computed sizes of the child element's margins.
                    var topMargin = isc.Element.getTopMargin(child);
                    var rightMargin = isc.Element.getRightMargin(child);
                    var bottomMargin = isc.Element.getBottomMargin(child);
                    var leftMargin = isc.Element.getLeftMargin(child);
                    bcr.top -= topMargin;
                    bcr.right += rightMargin;
                    bcr.bottom += bottomMargin;
                    bcr.left -= leftMargin;
                    bcr.width += rightMargin + leftMargin;
                    bcr.height += topMargin + bottomMargin;
                } else {
                    if (textRange == null) textRange = handle.ownerDocument.createRange();
                    textRange.setStartBefore(child);
                    textRange.setEndAfter(child);
                    bcr = textRange.getBoundingClientRect();
                }

                handleBCR.top = Math.min(bcr.top, handleBCR.top);
                handleBCR.right = Math.max(handleBCR.right, bcr.right);
                handleBCR.bottom = Math.max(handleBCR.bottom, bcr.bottom);
                handleBCR.left = Math.min(bcr.left, handleBCR.left);
            }
        } finally {
            // Remove any temporarily-inserted &nbsp;.
            if (endTempNbspChild != null) {
                handle.removeChild(endTempNbspChild);
                endTempNbspChild = null;
            }
            if (beginTempNbspChild != null) {
                handle.removeChild(beginTempNbspChild);
                beginTempNbspChild = null;
            }
        }
    }

    handleBCR.width = handleBCR.right - handleBCR.left;
    handleBCR.height = handleBCR.bottom - handleBCR.top;

    return handleBCR;
},


_matchesSelector : function (element, selectorText) {
    
    try {
        if (isc.Browser.isIE9) return element.msMatchesSelector(selectorText);
        else if (isc.Browser.isChrome || isc.Browser.isWebKit) return element.webkitMatchesSelector(selectorText);
        else if (isc.Browser.isMoz) return element.mozMatchesSelector(selectorText);
        else if (isc.Browser.isOpera) return element.oMatchesSelector(selectorText);
    } catch (e) {
        
        return false;
    }

    // Incomplete fallback implementation for browsers that do not support the matches() DOM
    // method (formerly called matchesSelector()).
    

    

    if (isc.isAn.emptyString(element.className)) return false;
    var classNameSelector = "." + element.className,
        selectorTextArray = selectorText.split(/(?:,\s*)+/);
    for (var k = 0; k < selectorTextArray.length; k++) {
        if (selectorTextArray == classNameSelector) return true;
    }
    return false;
},



// look up a style declaration via document.stylesheets

//_defaultReferenceElement: null,
getStyleDeclaration : function (className, checkMultiples, referenceElement) {

    if (!className) return null;

    if (referenceElement == null) {
        referenceElement = this._defaultReferenceElement;
        if (referenceElement == null) {
            
            referenceElement = this._defaultReferenceElement = document.createElement("div");
            referenceElement.id = "isc_getStyleDeclaration_defaultReferenceElement";
            referenceElement.style.display = "none";
            document.body.appendChild(referenceElement);
        }
        referenceElement.className = className;
    } 

    
    if (!isc.allowDuplicateStyles) checkMultiples = false;

    

    // Check the array of style rules from any styleSheets 
    // - This will include <STYLE> tags in the doc
    // - Start with the most recently loaded
    var styleObj, styleObjs = checkMultiples ? [] : null;
    for (var i = document.styleSheets.length - 1; i >= 0; i--) {
        var rules = this._getCSSRules(document.styleSheets[i]);

        
        if (rules == null) continue; // stylesheet inaccessible

        // iterate backward through style declarations, since last wins
        for (var j = rules.length - 1; j >= 0; j--) {
            var selectorText = rules[j].selectorText;

            // @import css tags result in entries with no 'selectorText' property.            
            if (selectorText == null) continue;

            
            if (isc.Browser.isSafari && 530.17 > isc.Browser.safariVersion && isc.Browser.safariVersion >= 312) {
                selectorText += ", " + selectorText.toLowerCase()
            }

            if (this._matchesSelector(referenceElement, selectorText)) {
                var styleObj = rules[j].style;
                if (checkMultiples) styleObjs[styleObjs.length] = styleObj;
                else return styleObj;
            }
        }
    }
    if (checkMultiples && styleObjs.length > 0) return styleObjs;
    return null;
},

// retrieve the css rules property from a stylesheet definition
_getCSSRules : function (styleSheet) {
    
    if (!this._fetchStyle) {

        // "cssRules" in Moz, "rules" in IE.
        this._fetchStyle = function (sheet) {
            try {
                return sheet.rules || sheet.cssRules;
            } catch (e) {
                isc.Page._remoteStyleSheet = true;
            }
        };

    }
    return this._fetchStyle(styleSheet);
},





//>	@classMethod Element.getStyleText()	([A])
// Gets the raw CSS style text associated with a CSS className. For example, if you have
// defined the following class:<br><br>
// <code>.cellSelected {font-family:Verdana; font-size:10px; background-color:#FFFF99;<br>
// border-top:1px solid #FFFF99;<br>
// border-bottom:1px solid #FFFF99;<br>
// }</code><br><br>
// Then calling getStyleText("cellSelected") will return:<br><br>
// <code>font-family:Verdana; font-size:10px; background-color:#FFFF99;<br>
// border-top:1px solid #FFFF99; border-bottom:1px solid #FFFF99;</code>
//		@group	drawing
//
//      @param  className   (string)       The CSS ClassName for which you wish to get the style
//      @param  [checkMultiples]    (boolean)   If specified this will ensure we check for 
//                                          multiple definitions of the same className and pick
//                                          up cssText from each definition. False by default.
//                                          No effect if isc.allowDuplicateStyles is false.
//      @return (string)       The cssText property of this CSS style rule
//<
_cssTextCache:{},
_$semi:";",
getStyleText : function (className, checkMultiples) {

    
    if (!isc.allowDuplicateStyles) checkMultiples = false;
    

    
    
    // check cache.
    
    var cache = this._cssTextCache,
        cssText = cache[className];
    if (cssText != null) return cssText;

    var style = this.getStyleDeclaration(className, checkMultiples); 
    
    // if we didn't find anything, return the empty string (rather than null)
    if (style == null) {
        
        if (!isc.Browser.isSafari || isc.Page.isLoaded()) 
            this._cssTextCache[className] = isc.emptyString;
        return isc.emptyString;
    }

    // "style" will be an array if we checked multiple styles
    
    
    if (checkMultiples) {
        for (var i = style.length-1; i >-1; i--) {        
            var actualStyle = style[i];
            var currentCssText = actualStyle.cssText;
            if (currentCssText == null) continue;
            if (!isc.endsWith(currentCssText, this._$semi)) currentCssText += this._$semi;
            if (cssText == null) cssText = currentCssText;
            else cssText += currentCssText;
        }
        if (cssText == null) cssText = isc._emptyString;
    } else {
        cssText = (style.cssText || isc._emptyString);
    }
        
    // ensure it ends with a semicolon so it can be appended to
    if (!isc.endsWith(cssText, isc.semi)) cssText += isc.semi;
    // cache it and return it
    
    return (cache[className] = cssText);
},


// wipe out any cached CSS information
// helper for Canvas._clearCSSCaches
_clearCSSCaches : function () {
    //isc.Log.logWarn("styleCache is: " + isc.echoFull(isc.Element._styleCache));

    // wipe out the central style definition caches
    isc.Element._styleCache = {};
    isc.Element._cssTextCache = {};

    // wipe out central edge size caches
    isc.Element._borderSizeCache = isc.Element._marginsCache = 
        isc.Element._padSizeCache = null;
},

// Border, Padding and Margin on css classes
// --------------------------------------------------------------------------------------------
// We provide the static Element class methods to handle getting border and padding 
// thicknesses on each side ('getTopBorderSize()', 'getBottomBorderSize()', 'getTopPadding()',
// etc.) for css classes from their classNames.





//>	@classMethod	Element._getTopMargin()
// Get the size of the top margin for the style passed in.
//
//  @param  className   (string)    className to test for margin size
//  @return             (number)    size of top margin in pixels
//<
_getTopMargin : function (className) {
    return this._calculateMargins(className).top;
},

//>	@classMethod	Element._getBottomMargin()
// Get the size of the bottom margin for the style passed in.
//
//  @param  className   (string)    className to test for margin
//  @return             (number)    size of bottom margin in pixels
//<
_getBottomMargin : function (className) {
    return this._calculateMargins(className).bottom;
},

//>	@classMethod	Element._getLeftMargin()
// Get the size of the left margin for the style passed in.
//
//  @param  className   (string)    className to test for margin size
//  @return             (number)    size of left margin in pixels
//<
_getLeftMargin : function (className) {
    return this._calculateMargins(className).left;
},

//>	@classMethod	Element._getRightMargin()
// Get the size of the right margin for the style passed in.
//
//  @param  className   (string)    className to test for margin size
//  @return             (number)    size of right margin in pixels
//<
_getRightMargin : function (className) {
    return this._calculateMargins(className).right;
},

//>	@classMethod	Element._calculateMargins()
// Calculate the sizes of the margins on each side for the css class passed in.
//
//  @param  className   (string)    className to test for margin sizes
//  @return             (object)    Object with 'left', 'right', 'top', 'bottom' defined as the 
//                                  sizes of the margin on each side
//<
_calculateMargins : function (className) {

    if (this._marginsCache == null) this._marginsCache = {};
    else if (this._marginsCache[className] != null) {
        return this._marginsCache[className];
    }        
    var margins = {top:0, bottom:0, left:0, right:0},
        styleObject = isc.Element.getStyleEdges(className);

    if (styleObject == null) return margins;
    
    var topMarginString = styleObject.marginTop,
        bottomMarginString = styleObject.marginBottom,
        leftMarginString = styleObject.marginLeft,
        rightMarginString = styleObject.marginRight,
        pxString = isc.px;
    if (isc.isA.String(topMarginString) && isc.endsWith(topMarginString, pxString)) 
        margins.top = parseInt(topMarginString);

    if (isc.isA.String(bottomMarginString) && isc.endsWith(bottomMarginString, pxString)) 
        margins.bottom = parseInt(bottomMarginString);
        
    if (isc.isA.String(leftMarginString) && isc.endsWith(leftMarginString, pxString)) 
        margins.left = parseInt(leftMarginString);
        
    if (isc.isA.String(rightMarginString) && isc.endsWith(rightMarginString, pxString)) 
        margins.right = parseInt(rightMarginString);
        
    this._marginsCache[className] = margins;        

    return margins;

},

//>	@classMethod	Element._getTopBorderSize()
// Get the size of the top border for the style passed in.
//
//  @param  className   (string)    className to test for border size
//  @return             (number)    size of top border in pixels
//<
_getTopBorderSize : function (className) {
    return this._calculateBorderSize(className).top;
},

//>	@classMethod	Element._getBottomBorderSize()
// Get the size of the bottom border for the style passed in.
//
//  @param  className   (string)    className to test for border size
//  @return             (number)    size of bottom border in pixels
//<
_getBottomBorderSize : function (className) {
    return this._calculateBorderSize(className).bottom;
},

//>	@classMethod	Element._getLeftBorderSize()
// Get the size of the left border for the style passed in.
//
//  @param  className   (string)    className to test for border size
//  @return             (number)    size of left border in pixels
//<
_getLeftBorderSize : function (className) {
    return this._calculateBorderSize(className).left;
},

//>	@classMethod	Element._getRightBorderSize()
// Get the size of the right border for the style passed in.
//
//  @param  className   (string)    className to test for border size
//  @return             (number)    size of right border in pixels
//<
_getRightBorderSize : function (className) {
    return this._calculateBorderSize(className).right;
},

//>	@classMethod	Element._calculateBorderSize()
// Calculate the sizes of the borders on each side for the css class passed in.
//
//  @param  className   (string)    className to test for border sizes
//  @return             (object)    Object with 'left', 'right', 'top', 'bottom' defined as the 
//                                  sizes of the border on each side
//<
_calculateBorderSize : function (className) {

    if (this._borderSizeCache == null) this._borderSizeCache = {};
    else if (this._borderSizeCache[className] != null) {
        return this._borderSizeCache[className];
    }        

    var borderSize = {top:0, bottom:0, left:0, right:0},
        styleObject = isc.Element.getStyleEdges(className);

    if (styleObject == null) return borderSize;
    
    var topBorderString = styleObject.borderTopWidth,
        bottomBorderString = styleObject.borderBottomWidth,
        leftBorderString = styleObject.borderLeftWidth,
        rightBorderString = styleObject.borderRightWidth,
        pxString = isc.px;
        
    if (isc.isA.String(topBorderString) && isc.endsWith(topBorderString, pxString)) 
        borderSize.top = parseInt(topBorderString);

    if (isc.isA.String(bottomBorderString) && isc.endsWith(bottomBorderString, pxString)) 
        borderSize.bottom = parseInt(bottomBorderString);
        
    if (isc.isA.String(leftBorderString) && isc.endsWith(leftBorderString, pxString)) 
        borderSize.left = parseInt(leftBorderString);
        
    if (isc.isA.String(rightBorderString) && isc.endsWith(rightBorderString, pxString)) 
        borderSize.right = parseInt(rightBorderString);
    
    this._borderSizeCache[className] = borderSize;        

    return borderSize;

},

//>	@classMethod	Element._getVBorderSize()
// Get the total vertical border size for the style passed in. (Top border size + bottom border size)
//
//  @param  className   (string)    className to test for padding size
//  @return             (number)    total size of vertical (top and bottom) borders in pixels
//<
_getVBorderSize : function (className) {
    return this._getTopBorderSize(className) + this._getBottomBorderSize(className);
},

//>	@classMethod	Element._getHBorderSize()
// Get the total horizontal border size for the style passed in. 
// (Left border size + Right border size)
//
//  @param  className   (string)    className to test for padding size
//  @return             (number)    total size of horizontal (left and right) borders in pixels
//<
_getHBorderSize : function (className) {
    return this._getLeftBorderSize(className) + this._getRightBorderSize(className);
},

//>	@classMethod	Element._getTopPadding()
// Get the size of the top padding for the style passed in.
//
//  @param  className   (string)    className to test for padding size
//  @param  [explicit]  (boolean)   If no passed, when no padding was specified, return null
//                                  rather than true.
//  @return             (number)    size of top padding in pixels
//<
_getTopPadding : function (className, explicit) {
    var padding = this._calculatePadding(className);
    if (explicit && padding.nullTop) return null;
    return padding.top;
},

//>	@classMethod	Element._getBottomPadding()
// Get the size of the bottom padding for the style passed in.
//
//  @param  className   (string)    className to test for padding size
//  @param  [explicit]  (boolean)   If passed, when no padding was specified, return null
//                                  rather than true.
//  @return             (number)    size of bottom padding in pixels
//<
_getBottomPadding : function (className, explicit) {
    var padding = this._calculatePadding(className);
    if (explicit && padding.nullBottom) return null;
    return padding.bottom;
},

//>	@classMethod	Element._getLeftPadding()
// Get the size of the left padding for the style passed in.
//
//  @param  className   (string)    className to test for padding size
//  @param  [explicit]  (boolean)   If passed, when no padding was specified, return null
//                                  rather than true.
//  @return             (number)    size of left padding in pixels
//<
_getLeftPadding : function (className, explicit) {
    var padding = this._calculatePadding(className);
    if (explicit && padding.nullLeft) return null;
    return padding.left;
},

//>	@classMethod	Element._getRightPadding()
// Get the size of the right padding for the style passed in.
//
//  @param  className   (string)    className to test for padding size
//  @param  [explicit]  (boolean)   If passed, when no padding was specified, return null
//                                  rather than true.
//  @return             (number)    size of right padding in pixels
//<
_getRightPadding : function (className, explicit) {
    var padding = this._calculatePadding(className);
    if (explicit && padding.nullRight) return null;
    return padding.right;
},

//>	@classMethod	Element._calculatePadding()
// Calculate the sizes of the padding on each side for the css class passed in.
//
//  @param  className   (string)    className to test for padding sizes
//  @return             (object)    Object with 'left', 'right', 'top', 'bottom' defined as the 
//                                  sizes of the padding on each side
//<
_calculatePadding : function (className) {
    if (this._padSizeCache == null) this._padSizeCache = {};
    else if (this._padSizeCache[className] != null) {
        return this._padSizeCache[className];
    }

    var padSize = {top:0, bottom:0, left:0, right:0},
        styleObject = isc.Element.getStyleEdges(className);
        
    if (styleObject == null) {
        padSize.nullLeft = true; padSize.nullRight = true;
        padSize.nullTop = true; padSize.nullBottom = true;
        return padSize;
    }
    
    var topPadding = styleObject.paddingTop,
        bottomPadding = styleObject.paddingBottom,
        leftPadding = styleObject.paddingLeft,
        rightPadding = styleObject.paddingRight,
        pxString = isc.px;

    // In some cases we will want to know whether a style contains specified padding *at all*,
    // as opposed to explicit zero.
    padSize.nullTop = (topPadding == null || topPadding == isc.emptyString);
    padSize.nullBottom = (bottomPadding == null || bottomPadding == isc.emptyString)
    padSize.nullLeft = (leftPadding == null || leftPadding == isc.emptyString);
    padSize.nullRight = (rightPadding == null || rightPadding == isc.emptyString);
        
    if (isc.isA.String(topPadding) && isc.endsWith(topPadding, pxString)) 
        padSize.top = parseInt(topPadding);

    if (isc.isA.String(bottomPadding) && isc.endsWith(bottomPadding, pxString)) 
        padSize.bottom = parseInt(bottomPadding);
        
    if (isc.isA.String(leftPadding) && isc.endsWith(leftPadding, pxString)) 
        padSize.left = parseInt(leftPadding);
        
    if (isc.isA.String(rightPadding) && isc.endsWith(rightPadding, pxString)) 
        padSize.right = parseInt(rightPadding);

    this._padSizeCache[className] = padSize;

    return padSize;
    
},

//>	@classMethod	Element._getVPadding()
// Get the total size of the vertical padding for the style passed in.
//
//  @param  className   (string)    className to test for padding size
//  @return             (number)    size of vertical padding in pixels
//<
_getVPadding : function (className) {
    return this._getTopPadding(className) + this._getBottomPadding(className);
},

//>	@classMethod	Element._getHPadding()
// Get the total size of the horizontal padding for the style passed in.
//
//  @param  className   (string)    className to test for padding size
//  @return             (number)    size of horizontal padding in pixels
//<
_getHPadding : function (className) {
    return this._getLeftPadding(className) + this._getRightPadding(className);
},

//>	@classMethod	Element._getVBorderPad()
// Get the total size of the padding and border for this style, for the vertical axis.
//
//  @param  className   (string)    className to test for VBorderPad
//  @return             (number)    size of VBorderPad in pixels
//<
_getVBorderPad : function (className) {
    return this._getVBorderSize(className) + this._getVPadding(className);
},

//>	@classMethod	Element._getHBorderPad()
// Get the total size of the padding and border for this style, for the horizontal axis.
//
//  @param  className   (string)    className to test for HBorderPad
//  @return             (number)    size of HBorderPad in pixels
//<
_getHBorderPad : function (className) {
    return this._getHBorderSize(className) + this._getHPadding(className);
},

// ----------------------

// getNativeScrollbarSize()
// 
// If the browser is showing native scrollbars, we have no way of knowing their thickness by
// default. This can be set at the OS level (via the Control Panel in windows), and the 
// defaults vary based on which OS is being used.
// This method determines the size of the scrollbars by writing a DIV into the DOM and 
// examining it

getNativeScrollbarSize : function () {
    if (isc.Element._nativeScrollbarSize == null) {
        if (isc.Browser.isMobileWebkit) {
            // native scrollbars don't exist in iPhone
            return (isc.Element._nativeScrollbarSize = 0);
        }
        var elementHTML = "<div id=isc_ScrollbarTest " 
                            + "style='position:absolute;top:-100px;border:0px;padding:0px;margin:0px;height:100px;width:100px;overflow:scroll;'>"
                            + isc.nbsp // XHTML
                            + "</div>";
        this.createAbsoluteElement(elementHTML);
        var sbTest = this.get('isc_ScrollbarTest');
        isc.Element._nativeScrollbarSize = parseInt(sbTest.style.height) - sbTest.clientHeight;

        // At this point we've determined the scrollbar size, and can clear the test div out 
        // from the DOM
        this.clear(sbTest);
    }

    return isc.Element._nativeScrollbarSize;
},

// ---------------------------------------------------------------------------------------




// Return CSS to transform by degrees around an origin

_transformCSSName: (isc.Element.vendorStylePrefix + "Transform" in document.documentElement.style
                    ? isc.Element.vendorCSSPrefix + "transform"
                    : "transform"),
_transformOriginCSSName: (isc.Element.vendorStylePrefix + "TransformOrigin" in document.documentElement.style
                          ? isc.Element.vendorCSSPrefix + "transform-origin"
                          : "transform-origin"),
getRotationCSS : function (degrees, origin) {
    degrees = +degrees;
    
    var prefix = this.vendorCSSPrefix;
    
    var text = this._transformCSSName + ": rotate(" + degrees.toFixed(3) + "deg);";
    if (origin != null) {
        text += this._transformOriginCSSName + ": " + origin + ";";
    }
    return text;
},

_transformStyleName: (isc.Element.vendorStylePrefix + "Transform" in document.documentElement.style
                      ? isc.Element.vendorStylePrefix + "Transform"
                      : "transform"),
_transformOriginStyleName: (isc.Element.vendorStylePrefix + "TransformOrigin" in document.documentElement.style
                            ? isc.Element.vendorStylePrefix + "TransformOrigin"
                            : "transformOrigin"),
_updateTransformStyle : function (sourceElement, transformFunctions, origin) {
    

    var handle = sourceElement.getClipHandle();
    if (handle == null) return;

    
    sourceElement._$leftCoords = sourceElement._$topCoords = null;
    sourceElement._childrenCoordsChanged();

    if (!transformFunctions) transformFunctions = "none";

    var style = handle.style;
    style[this._transformStyleName] = transformFunctions;
    if (origin != null) {
        style[this._transformOriginStyleName] = origin;
    }
}

});

isc.Element._ElementInit = function () {

    var edgeMask = this._styleEdgeMask = {
            
            borderLeftWidth:"border-left-width", 
            borderRightWidth:"border-right-width", 
            borderTopWidth:"border-top-width", 
            borderBottomWidth:"border-bottom-width",
            marginLeft:"margin-left",
            marginRight:"margin-right",
            marginTop:"margin-top",
            marginBottom:"margin-bottom",

            
            paddingLeft:"padding-left", 
            paddingRight:"padding-right",
            paddingTop:"padding-top", 
            paddingBottom:"padding-bottom"
    }

    var fullMask = this._styleFullMask = isc.addProperties({
            position:"position", 
            overflow:"overflow",
            top:"top",
            left:"left",
            width:"width",
            height:"height",
            
            // display - block vs. inline (etc.)
            display:"display"
    }, edgeMask);

    if (isc.Browser.isIE || isc.Browser.isOpera) {
        this._styleFullMaskArray = isc.getKeys(fullMask);
        this._styleEdgeMaskArray = isc.getKeys(edgeMask);
    }
}
isc.Element._ElementInit();




//> @groupDef domIntegration
// SmartClient provides a huge variety of pre-built components and allows you to create new
// components via combining and composing the existing components.  However in rare cases, it can
// make sense to write code that works directly with raw HTML and the browser's DOM (document
// object model) APIs.  This level of coding is also involved when integrating third-party
// JavaScript components into SmartClient applications.
// <P>
// First, a warning: when you use HTML and the DOM directly, all of SmartClient's guarantees of
// cross-browser consistent behavior no longer apply.  When you use SmartClient's API, SmartClient
// is automatically compensating for many, many browser bugs - not just trivial things like
// different property names or missing utility methods, but problems where browsers fail to fire
// certain events, report sizes wrong only in certain modes with certain styling, or bugs that
// only occur with specific timing.
// <P>
// Before deciding to do direct HTML coding, consider whether you really want to expose yourself
// to all of these possible issues.  If you can achieve the same look and feel and behavior
// through SmartClient's APIs, that's usually best.
// <P>
// <h3>Adding or modifying the DOM</h3>
// <P>
// The DOM structures used by SmartClient necessarily differ by browser in order to work around
// each browser's specific bugs.  This DOM structure is intentionally undocumented because it is
// subject to change without notice - in may be necessary to modify the DOM structure to work
// around the bugs in each new browser release.
// <P>
// Instead of trying to modify the SmartClient-generated DOM, you should always <b>add new
// elements</b>.  For a new standalone component that will be based on direct use of HTML, you
// usually do this by subclassing Canvas and overriding +link{Canvas.getInnerHTML()} and returning
// an HTML string representing the components you want to create.
// <P>
// You can use a similar approach anywhere HTML is allowed in a widget property: formatting APIs
// for StaticTextItem, DetailViewer, TileGrid, and other DataBoundComponents, as well as places
// such as +link{tab.title} or +link{button.title}.
// <P>
// <h3>Third-party components</h3>
// <P>
// Most third-party JavaScript components have the ability to generate their HTML content into a
// DOM element specified by ID, or to replace such an element with new HTML.  This is true of
// Google Maps, +externalLink{http://ckeditor.com,CKEditor} and many other libraries.
// <P>
// To use this form of integration, implement a +link{canvas.getInnerHTML()} function that returns
// a DOM element with a known ID, then have the third-party library target that DOM element once
// the Canvas is drawn.  For example, CKEDITOR.replace() takes the ID of a &lt;textarea&gt;
// element, and the following code would create a &lt;textarea&gt; and have the CKEditor replace
// it with a CKEditor widget: 
// <smartclient>
// <pre>
// isc.defineClass("CKEditor", "Canvas");
// isc.CKEditor.addProperties({
//     // write out a textarea with a known ID
//     getInnerHTML : function () {
//         return "&lt;textarea style="width:100%;height:100%" ID='" + 
//                           this.getID() + "_ckEditor" + "'>&lt;/textarea>";
//     },
//     // call superclass method to draw, then have CKEditor replace the textarea we
//     // wrote out with the CKEditor widget
//     draw : function () {
//         if (!this.readyToDraw()) return this;
//         this.Super("draw", arguments);
//         CKEDITOR.replace(this.getID() + "_ckEditor";
//         return this;
//     },
//     redrawOnResize:false // see next section
// });
// </pre>
// </smartclient>
// <smartgwt>
// <pre>
// public class CKEditor extends Canvas {
// 
//     private static native void replace(String id) &#47;*-{
//         $wnd.CKEDITOR.replace(id);
//     }-*&#47;;
// 
//     public CKEditor() {
//         setRedrawOnResize(false);
//     }
// 
//     &#64;Override
//     public String getInnerHTML() {
//         return "&lt;textarea id='" + getID() + "_ckEditor' style='width:100%;height:100%'>&lt;/textarea>";
//     }
// 
//     &#64;Override
//     protected void onDraw() {
//         CKEditor.replace(getID() + "_ckEditor");
//     }
// }
// </pre>
// </smartgwt>
// <P>
// This same approach can be used when you want to insert third-party generated HTML into just a
// specific part of a SmartClient widget.  For example, you might want to insert 
// +externalLink{https://www.google.com/search?q=jquery+sparklines,JQuery 'sparklines'}, which are
// essentially miniature charts, into cells of a ListGrid.  You could use
// +link{listGridField.formatCellValue,a cell formatter} to write out &lt;div&gt; elements with
// known IDs into the cells, then target them with JQuery.
// <P>
// <h3>Resizing and Redraw</h3>
// <P>
// When implementing <code>canvas.getInnerHTML()</code>, your getInnerHTML() function will be
// called every time the component redraw()s, and the new HTML will replace the old.  This is also
// true of something like a ListGrid cell formatter.
// <P>
// Also by default, your component will redraw() if it is resized.  In the example above with
// CKEditor, we wouldn't want this because it would wipe out the CKEditor widget every time it was
// resized, so we set +link{canvas.redrawOnResize} to false.  In other circumstances you may want
// to redraw on every resize in order to generate new HTML.
// <P>
// If you do not redraw HTML on resize, you either have to write the HTML in a way that makes it
// flow into available space (width/height 100% may be enough here) <b>or</b> you need to manually
// resize the DOM element when the +link{canvas.resized,resized event} fires.  
// <P>
// In the latter case, you should adjust the size of the DOM element to match the
// +link{canvas.getInnerWidth(),inner width} and 
// +link{canvas.getInnerHeight(),inner height} of the containing Canvas.  The "inner" dimensions
// are the dimensions after border and margins have been subtracted, so this will work even if a
// border is added to your Canvas subclass via CSS or +link{canvas.setBorder()}.
// <P>
// <h3>Other redraws</h3>
// <P>
// Once you have set +link{canvas.redrawOnResize} to false you may still see redraws from other
// sources.  Generally this would be from code in your application which calls
// +link{canvas.redraw()} or +link{canvas.markForRedraw()} unnecessarily.  To troubleshoot, you
// can enable the "redraws" log category in the Developer Console - this will log the source of
// any redraws in the system.
// <P>
// <h3>Masking during drags</h3>
// <P>
// Third-party components that utilize iframes, browser plugins or other special elements will
// "swallow" events such that SmartClient never receives them.  This is a problem whenever a
// drag interaction goes over the component, including drag resizing of the component itself.
// To avoid this issue, set +link{canvas.useDragMask} for any component that contains an iframe
// or browser plugin, or that appears to be swallowing events during drag.  The telltale sign
// is that when the mouse goes over the plugin, the visual effect of the drag (differs by
// +link{canvas.dragAppearance}) stops updating or stutters.
// <P>
// <h3>Overflow &amp; Auto-Sizing</h3>
// <P>
// Consider which +link{canvas.overflow} setting to use for your custom component:
// <ul>
// <li> <code>overflow:"hidden"</code> is the most common.  In the context of third-party
// components, it means the component is prepared to render itself at any requested size (above
// a minimum). 
// <li> <code>overflow:"visible"</code> means you want SmartClient to attempt to automatically
// determine a minimum size based on the HTML content generated by the custom component
// <li> <code>overflow:"auto"</code> is similar to <code>overflow:"hidden"</code>, but means
// your custom component needs SmartClient to create scrollbars whenever its HTML content does
// not fit in the allocated space.
// </ul>
// Note that with the automatic measurement of HTML content enabled by
// <code>overflow:"visible"</code> or <code>overflow:"auto"</code>, if you make on-the-fly
// modifications to the HTML you returned from <code>getInnerHTML()</code>, there is no
// cross-browser-reliable way for the Canvas to detect this and auto-size again.  Instead, call
// +link{canvas.adjustForContent()} to trigger auto-sizing again.
// <P>
// <h3>zIndex</h3>
// <P>
// zIndex values control what component is rendered "on top" when multiple components appear in
// the same area of the page.
// <P>
// To work around various browser issues, SmartClient components use a very high range of
// zIndex values.  If a component creates pop-up widgets such as hovers or floating toolbars
// via direct HTML/DOM usage, these pop-up widgets will appear <b>behind</b> all SmartClient
// components unless they set a very high zIndex.  
// <P>
// For your own custom HTML/DOM components, the simplest strategy is to create pop-up widgets
// based on SmartClient components, even if they are triggered by interacting with
// hand-created HTML.  For example, even if you've written some kind of advanced SVG-based data
// visualization component, you can still implement pop-up configuration dialogs based on the
// SmartClient +link{Window} class; there's no reason to implement such dialogs directly in
// low-level HTML/DOM code.
// <P>
// If a third-party widget is creating pop-ups you don't directly control, you may be able to
// configure the third-party widget to use a certain zIndex range, or you may be able to
// directly reach into the widget's DOM and modify zIndexes that way.  You can use
// +link{canvas.getZIndex()} to discover the zIndex of any SmartClient widget you need to
// appear above, then set a higher value.
// <P>
// Finally, as a last resort and completely unsupported approach, you can modify the zIndex
// range used by SmartClient using the following JavaScript code:
// <pre>
// isc.Canvas.addClassProperties({
//    // default zIndex for the next item to be drawn
//    _nextZIndex:200000,
//
//    // zIndex of the next item to be sent to the back
//    _SMALL_Z_INDEX:199950,
//
//    // zIndex of the next item to be brought to the front
//    _BIG_Z_INDEX:800000
// });
// </pre>
// <P>
// <h3>Other issues</h3>
// <P>
// There are several other issues, listed below, for which there really is no general strategy
// for solving the issue, although some general pointers are provided.
// <P>
// Because of problems like these, it's a very very bad idea to freely intermix components from
// multiple component libraries.  While mixing components may appear to be an appealing
// strategy and you may experience apparent success with early attempts, the issues below will
// ultimately prevent you from completing an application of sufficient quality for enterprise
// use.
// <P>
// In the following discussion, "third-party widgets" should be understood to include
// widgets that you write using direct DOM/HTML techniques.
// <ul>
// <li> <b>tabbing order / accessibility</b>: a correct tabbing order that visits all
// components on the page is a requirement for your application to be considered accessible, as
// is ARIA markup (for more information, see +link{groupDef:accessibility}).  Third-party
// widgets may completely lack ARIA markup, such that you may be required to modify them or
// reach into their DOM to add ARIA attributes.  It may be necessary to add manual keyDown or
// keyPress event handlers to handle focus moving from SmartClient components to third-party
// widgets and back.
//
// <li> <b>modality</b>: aside from zIndex issues covered above, modality means that the tab
// order should be a closed loop that reaches only active widgets, which can create additional
// complexity in getting tabbing to work correctly.  Also, keyboard shortcuts should be
// disabled for inactive widgets; this may require calls to
// +link{EventHandler.targetIsMasked()} to make third-party widgets respect SmartClient
// modality, or may require calls to +link{Canvas.showClickMask()} to cause SmartClient
// components to consider themselves inactive when a third-party widget opens a pop-up that is
// intended to be modal.  Multi-layered modality, such as a modal window that in turn pops a
// modal dialog, is yet more complex.
//
// <li> <b>bad CSS</b>: some third-party widgets introduce CSS selectors that target, for
// example, every table cell on the entire page.  This very bad practice will interfere with
// SmartClient (or any other HTML on the page).  This may require modifying the third-party
// component, or extensively modifying SmartClient CSS to reverse any changes caused by
// third-party CSS.  For example, it may be necessary to modify every SmartClient CSS style
// that may be applied to a table cell to reverse a change in padding for all table cells that
// is introduced by bad third-party CSS.
//
// <li> <b>skinning</b>: third-party widgets may lack sufficient skinning APIs to allow you to match
// look and feel to SmartGWT, which may necessitate creating a custom SmartGWT skin to match
// the look and feel of third-party widgets (see +link{groupDef:skinning,Skinning Overview})
//
// <li> <b>event interference</b>: third-party widgets may register page-wide event handling
// logic that conflicts with or destroys similar event handling logic in SmartClient.  For best
// results, load third-party JavaScript libraries <b>before</b> SmartClient since SmartClient
// makes a best effort to preserve any previously installed handlers and allows such handlers to
// cancel native browser behaviors if they do so.
//
// <li> <b>RTL / i18n</b>: third party widgets may not allow all user-visible messages to be
// replaced, a requirement for internationalization / localization, or they may not support
// RTL/BIDI (Right-To-Left / Bi-Directional) rendering
// </ul>
//
// Because of issues like the above, not all of which may be resolvable for some third-party
// widgets, we recommend the following overall approach:
// <ul>
// <li> avoid using third-party widgets if you can build equivalent functionality in
//      SmartClient
// <li> if the third-party component is completely non-interactive, either does not require
//      ARIA markup or already includes such markup, and there are no conflicting look and feel
//      issues, go ahead and use it
// <li> if you anticipate issues, consider the 
// +externalLink{http://www.smartclient.com/services/index.jsp#features,Feature Sponsorship Program} 
//      as a means of getting new supported functionality added to SmartClient
// <li> search for existing posts and/or ask about the feasibility of integration on the
//      +externalLink{http://forums.smartclient.com/,SmartClient Forums}.
// <li> finally, you could attempt to tackle the issues above on your own.  To avoid wasting
//      time on dead ends, we would recommend assessing the amount of work involved in fixing
//      <b>all</b> problems that need to be solved before attempting actual fixes for any one issue.
// </ul>
//
// @title DOM Integration &amp; Third-party Components
// @treeLocation Concepts
// @visibility external
//<
