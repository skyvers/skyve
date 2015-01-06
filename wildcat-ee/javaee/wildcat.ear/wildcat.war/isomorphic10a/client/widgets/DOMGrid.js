/*
 * Isomorphic SmartClient
 * Version v10.0p_2015-01-04 (2015-01-04)
 * Copyright(c) 1998 and beyond Isomorphic Software, Inc. All rights reserved.
 * "SmartClient" is a trademark of Isomorphic Software, Inc.
 *
 * licensing@smartclient.com
 *
 * http://smartclient.com/license
 */


//> @class DOMGrid
//
// Provides a tree view of any DOM-compliant structure, such as an XML or HTML document.
//
// @treeLocation Client Reference/Grids
//
// @visibility external
//<
isc.defineClass("DOMGrid", "TreeGrid").addMethods({
    initWidget : function () {
        this.Super(this._$initWidget);
        if (this.url) {
            isc.xml.loadXML(this.url, 
                            this.getID() + ".setRootElement(xmlDoc.documentElement)");
        }
    },

    // implement getDefaultData() so we don't end up with a Tree instance
    getDefaultData : function () {
        return null;//isc.DOMTree.create();
    },

    //> @attr domGrid.rootElement   (DOMElement : null : IRW)
    // Root element (or document) to view in the tree.
    // @visibility external
    //<

    //> @attr domGrid.htmlMode (boolean : false : IR)
    // In html mode, a DOMGrid shows a limited set of properties appropriate for inspecting
    // sizing and styling issues in an HTML document. 
    //
    // Internal for now, used only by the DOMInspector and with capricious non-configurable
    // attributes shown..
    // @visibility internal
    //<

    // D&D DOM changes
    // ---------------------------------------------------------------------------------------
    canDragRecordsOut : true,
    canAcceptDroppedRecords : true,
    canReorderRecords : true,
 
    // 
    // ---------------------------------------------------------------------------------------
    getElement : function (node) {
        return this.data.getElement(node);
    },

    // ---------------------------------------------------------------------------------------
    showRoot:true,

    //> @method domGrid.setRootElement()   
    // Set the root element (or document) to view in the tree.
    // @param rootElement (DOMElement) new root element
    // @visibility external
    //<
    setRootElement : function (rootElement) {
        this.rootElement = rootElement;
        var tree = isc.DOMTree.create({
            rootElement : rootElement
        });
        this.setData(tree);
    },
  
    // ---------------------------------------------------------------------------------------
    // suppress folder/file icons
    getIcon : function () { },

    // Node Titles
    // ---------------------------------------------------------------------------------------
    _$lt : "&lt;",
    _$gt : "&gt;",
    getNodeTitle : function (node, recordNum, field) {
        if (node == null) return null;
        
        // cache generated titles
        if (node._title) return node._title;

        //try {
            var title = this.htmlMode ? this._getHTMLNodeTitle(node) 
                                      : this._getXMLNodeTitle(node);
        //} catch (e) {
        //    this.logWarn("exception trying to retrieve title: " + this.getStackTrace());
        //    title = "<BAD NODE/>";
        //}

        return (node._title = title);
    },
    
    // wipe out cached titles on dataChanged
    dataChanged : function () {
        this.Super("dataChanged", arguments);
        this.data.getOpenList().setProperty("_title", null);
    },

    // getXMLNodeTitle: shows all attributes
    _attrTemplate : [" ",,'="',,'"'],
    _getXMLNodeTitle : function (node) {
        if (node._element == null) {
            this.logWarn("no element for node: " + this.echo(node));
            //return;
        }

        var element = node._element,
            ns = isc.emptyString,
            buffer = this._titleSB;

        if (buffer == null) buffer = this._titleSB = isc.SB.create();
        else buffer.clear();
        
        buffer.append(this._$lt, (element.tagName || element.nodeName));

        var attrs = element.attributes;
        if (attrs != null) {
            var template = this._attrTemplate;
            for (var i = 0; i < attrs.length; i++) {
                var attr = attrs[i];
                template[1] = attr.name;
                template[3] = attr.value;
                buffer.append(template);
            }
        }

        if (!isc.xml.hasElementChildren(element)) {
            // just text
            buffer.append(this._$gt, isc.xml.getElementText(element),
                          "&lt;/", (element.tagName || element.nodeName), this._$gt);
        } else if (element.childNodes.length > 0) {
            buffer.append(this._$gt);
        } else {
            buffer.append("/&gt;");
        }
        return buffer.toString();
    },

    // getHTMLNodeTitle: shows only a selection of common HTML attributes
    _getHTMLNodeTitle : function (node) {
        var element = node._element,
            ns = isc.emptyString,
            width, height;
        
        if (isc.Browser.isIE && element.scopeName == "VML") {
            // VML elements JS error in IE if you access element.width or element.height.
            width = (element.style ? element.style.width : null);
            height = (element.style ? element.style.height : null);
        } else {
            width = element.width || (element.style ? element.style.width : null);
            height = element.height || (element.style ? element.style.height : null);
        }
        var isCell = (element.tagName && element.tagName.toLowerCase() == "td");

        return isc.SB.concat(
            this._$lt, (element.tagName || element.nodeName),
                   (element.id ? " ID=" + element.id : ns),
                   (!this.valueIsEmpty(width) ? " WIDTH=" + width : ns),
                   (!this.valueIsEmpty(height) ? " HEIGHT=" + height : ns),
                   (isCell && element.rowSpan > 1 ? " ROWSPAN=" + element.rowSpan : ns),
                   (isCell && element.colSpan > 1 ? " COLSPAN=" + element.colSpan : ns),
            this._$gt);
    },

    valueIsEmpty : function (value) {
        return value == null || isc.isAn.emptyString(value);
    },

    // Colorize nodes
    // ---------------------------------------------------------------------------------------
    colorMap : {
        table : "#009900",
        tr : "#333399",
        td : "#663366",

        form : "#CC6600",
        input : "#3333FF",
        textarea : "#3333FF",

        div : "#663300",
        span : "#663300"
    },

    colorPrefix : "color:",
    _tagCache : {},
    getCellCSSText : function (node, rowNum, fieldNum) {
        var element = this.data.getElement(node);
        if (element == null) return null;
        var tagName = element.tagName;
        if (tagName == null) return null;

        // translate to canonical case and cache
        if (this._tagCache[tagName]) {
            tagName = this._tagCache[tagName];
        } else {
            this._tagCache = tagName = tagName.toLowerCase();
        }

        //this.logWarn("cellCSS: " + tagName);

        if (this.colorMap[tagName] != null) {
            return isc.SB.concat(this.colorPrefix, this.colorMap[tagName], isc.semi);
        }
    }
    
}); 
