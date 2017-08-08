/*
 * Isomorphic SmartClient
 * Version v11.0p_2016-03-31 (2016-03-31)
 * Copyright(c) 1998 and beyond Isomorphic Software, Inc. All rights reserved.
 * "SmartClient" is a trademark of Isomorphic Software, Inc.
 *
 * licensing@smartclient.com
 *
 * http://smartclient.com/license
 */
// ----------------------------------------------------------------------------------------

//> @class MockDataSource
// A special kind of +link{dataSource.clientOnly,client-only DataSource} that can be configured
// with +link{mockDataSource.mockData,"mock data"} - a simple text format for table or tree
// data.
// <p>
// MockDataSources are produced by the Reify Mockup Importer when starting from mockup formats
// that use the mock data format.  The docs for the 
// +link{group:balsamiqImport,Reify Mockup Importer} explain various steps for converting a
// <code>MockupDataSource</code> to a real DataSource.
// <p>
// <code>MockupDataSource</code> is primarily intended as a temporary form of DataSource used
// during the process of converting a mockup into a real application.  Generally, if creating a
// client-only DataSource in <smartclient>JavaScript</smartclient> <smartgwt>Java</smartgwt>,
// there is no reason to use the mock data format, as the mock data is not especially readable
// when written as a String literal.  The mock data format <i>can</i> be a slightly more
// compact and readable as compared to declaring +link{DataSource.testData} in XML.
//
// @treeLocation Client Reference/Data Binding
// @visibility external
//<
isc.defineClass("MockDataSource", "DataSource");

isc.MockDataSource.addClassProperties({

    parseTextWikiSymbols : function (text) {
        var italic = false;
        var bold = false;
        var link = false;
        var res = [];
        for (var i=0; i<text.length; i++) {
            var c = text.charAt(i);
            if (c == '\\') {
                if( (i + 1) < text.length && text.charAt(i + 1) == 'r') {
                    c = "<br/>";
                    i++;    
                }
            } else if (c == '[' && text.indexOf("]",i+1) > 0) {
                c = "<a href='#'>";
                link = true;
            } else if (c == ']') {
                if (link) {
                    c = "</a>";
                    link = false;
                }
            } else if (c == '*') {
                if (bold) {
                    bold = false;
                    c = "</b>";
                } else {
                    bold = true;
                    c = "<b>";
                }
            } else if (c == '_') {
                if (italic) {
                    italic = false;
                    c = "</i>";
                } else {
                    italic = true;
                    c = "<i>";
                }
            }
            res.push(c);
        }
        return res.join("");
    },
    
    parseTableFields : function(tableData, fieldNamingConvention) {
        var rowsData = tableData.split("\n");
        var rawHeaders = isc.MockDataSource.splitComma(rowsData[0]),
             headerArray = []
        ;
        var fieldsParametersLine = rowsData[rowsData.length - 1];
        if (fieldsParametersLine.startsWith("[") && 
            fieldsParametersLine.endsWith("]") && 
            fieldsParametersLine != "[]" && 
            fieldsParametersLine != "[ ]" && 
            fieldsParametersLine != "[x]") {
            var fieldsParameters = fieldsParametersLine.substring(1, 
                fieldsParametersLine.length - 1).split(",");
        }
        for (var j = 0; j < rawHeaders.length; j++) {
            var text = rawHeaders[j].trim().replace(/(\\r|\r)/g, "<br/>");
            var name = isc.MockDataSource.convertTitleToName(rawHeaders[j],
                                                             fieldNamingConvention, rawHeaders);
            var actualName = name;
            var iter = 0;
            do {
                var wasSame = false;
                for (var i=0; i < headerArray.length; i++) {
                    if (headerArray[i].name == actualName) {
                        iter++;
                        actualName = name + iter;
                        wasSame = true;
                        break;
                    }
                }  
            } while (wasSame);
            if (text == "") {
                text = "&nbsp;";
            }
            var field = {
                name: actualName,
                title: text          
            };
            if (field.title.length <= 3) {
                field.align = "center";
            }
            if (fieldsParameters && fieldsParameters[j]) {
                field.width = fieldsParameters[j];
                var lastChar = field.width[field.width.length - 1];
                if (!isc.MockDataSource._isDigit(lastChar)) {
                    field.width = field.width.substring(0, fieldsParameters[j].length - 1);
                    if (lastChar == 'R' || lastChar == 'r') {
                        field.align = "right";
                    } else if (lastChar == 'L' || lastChar == 'l') {
                        field.align = "left";
                    } else if (lastChar == 'C' || lastChar == 'c') {
                        field.align = "center";
                    } 
                }
                field.width += "%";
            }
            headerArray.add(field);
        }
        return headerArray;
    },

    convertTitleToName : function (title, fieldNamingConvention, rawHeaders) {
        var trailingUnderscore = true,
            name = title.trim().replace(/(\\r|\r)/g, "_").replace(/[^a-zA-Z0-9_# ]/g, "_");
        
        name = name.replace(/#+/g, " Number ");
        name = name.trim();
        
        if (name == "" || (name.charAt(0) >= '0' && name.charAt(0) <= '9')) {
            name = '_' + name;
        }
        var parts = name.split(" ");
        name = "";
        // naming conventions used when generating MockDataSource and FormItem field names from
        //  titles:
        //     - "camelCaps" eg "First Name" becomes firstName
        //  - "underscores" eg "First Name" becomes first_name
        //  - "underscoresAllCaps" eg "First Name" becomes FIRST_NAME
        //  - "underscoresKeepCase" eg "First Name" becomes First_Name
        // - default to camelCaps
        if ("underscores" == fieldNamingConvention) {
            for (var i = 0; i < parts.length; i++) {
                name += parts[i].toLowerCase();
                if (i != (parts.length - 1)) {
                    name += "_";
                }
            }
        } else if ("underscoresAllCaps" == fieldNamingConvention) {
            for (var i = 0; i < parts.length; i++) {
                name += parts[i].toUpperCase();
                if (i != (parts.length - 1)) {
                    name += "_";
                }
            }
        } else if ("underscoresKeepCase" == fieldNamingConvention) {
            for (var i = 0; i < parts.length; i++) {
                name += parts[i];
                if (i != (parts.length - 1)) {
                    name += "_";
                }
            }
        } else {
            name = parts[0].toLowerCase();
            // camelCaps is default
            for (var i = 1; i < parts.length; i++) {
                name += parts[i].substring(0, 1).toUpperCase();
                name += parts[i].substring(1).toLowerCase();
            }
        }
        while (trailingUnderscore) {
            if (name.endsWith("_")) {
                name = name.substring(0, name.length - 1);
            } else {
                trailingUnderscore = false;
            }
        }

        if (name == "") {
            name = "isc_gen";
        }
        
        if (name != title && rawHeaders.contains(name)) {
            // new name collides with another title
            while (rawHeaders.contains(name)) {
                name += "_";
            }
        }
        
        return name;
    },

    // Test for digits
    _isDigit : function(aChar) {
        var myCharCode = aChar.charCodeAt(0);
        if((myCharCode > 47) && (myCharCode <  58)) {
            return true;
        }
        return false;
    },
   
    parseTable : function(tableData) {
        var lineArray = tableData.split("\n");
        var headerArray = isc.MockDataSource.parseTableFields(tableData);
        var rowArray = [];
        // ignore the first line which is column names
        for (var i=1; i < lineArray.length; i++) {   
            if (lineArray[i].startsWith("[") && lineArray[i].endsWith("]") && 
                lineArray[i] != "[]" && lineArray[i] != "[ ]" && lineArray[i] != "[x]") {
                continue;
            }
            var rowObject = {};
            var valueArray = isc.MockDataSource.splitComma(lineArray[i]);
            for (var j=0; j < headerArray.length; j++) {
                var currVal = valueArray[j];
                if (currVal == null) {
                    currVal = "";
                }
                currVal = currVal.replace(/\r/g, "<br/>");
                currVal = currVal.replace("[]", "<input type='checkbox' />");
                currVal = currVal.replace("[ ]", "<input type='checkbox' />");
                currVal = currVal.replace("[x]", "<input type='checkbox' checked='true' />");
                rowObject[headerArray[j].name] = isc.MockDataSource.parseTextWikiSymbols(currVal);        
            }            
            rowArray.add(rowObject);
        }
        return rowArray;
    },
    
    splitComma : function(str) {
        var rawParts = str.split(","), parts = [];
        for (var i = 0, len = rawParts.length, part; i < len; ++i) {
            part = "";
            while (rawParts[i].slice(-1) == "\\") {
                part += rawParts[i++].slice(0, -1) + ",";
            }
            parts.push(part + rawParts[i]);
        }
        return parts;
    },

    // Tree-specific formatting is documented here:
    // https://support.mybalsamiq.com/projects/uilibrary/Tree%20Pane
    parseTree : function(treeData) {
        var nodeArray = treeData.split("\n");
        var dataTree = [];
        var lastNode;
        var lastIndent = 0;
        for (var i=0; i < nodeArray.length; i++) {
            var newNode = {};
            
            var nodeChars = nodeArray[i].split("");
            var indent = 0;
            for (var j=0; j < nodeChars.length; j++) {             
                if (nodeChars[j] == " " || nodeChars[j] == ".") {
                    indent += 1;  
                } else {
                    break;
                }
            }
            var nodeName = nodeArray[i].substr(indent);
            
            // detect open folder
            if (nodeName.startsWith("f") || nodeName.startsWith(">") 
             || nodeName.startsWith("[x") || nodeName.startsWith("[+")) {
                newNode.isFolder = true;            
            // detect closed folder
            } else if (nodeName.startsWith("F") || nodeName.startsWith("v") 
             || nodeName.startsWith("[ ") || nodeName.startsWith("[-")) {
                newNode.isFolder = true;
                newNode.isOpen = true;
            }
            var checkedImage = isc.CheckboxItem.getInstanceProperty("checkedImage");
            var uncheckedImage = isc.CheckboxItem.getInstanceProperty("uncheckedImage");
            // set the appropriate icon
            if (nodeName.startsWith("f")) {
            } else if (nodeName.startsWith(">")) {
            } else if (nodeName.startsWith("[x")) {
                newNode.icon = checkedImage;
            } else if (nodeName.startsWith("[+")) {
            } else if (nodeName.startsWith("F")) {
            } else if (nodeName.startsWith("v")) {
            } else if (nodeName.startsWith("[ ")) {
                newNode.icon = uncheckedImage;
            } else if (nodeName.startsWith("[-")) {
            } else if (nodeName.startsWith("-")) {
                newNode.isFolder = false;

            // _ means "leave a space for your own icon". If it ends up being a folder node,
            // then the folder is open. See, e.g., the Windows Explorer example:
            // https://mockupstogo.mybalsamiq.com/projects/desktopapplications/Windows%20Explorer
            } else if (nodeName.startsWith("_")) {
                newNode.icon = isc.Canvas._blankImgURL;
                newNode.isOpen = true;
            }
            // strip out node metadata
            if (nodeName.startsWith("[")) nodeName = nodeName.substr(3);
            else if (newNode.isFolder) nodeName = nodeName.substr(1);
            else if (nodeName.startsWith("-") || nodeName.startsWith("_")) nodeName = nodeName.substr(1);

            newNode.name = isc.MockDataSource.parseTextWikiSymbols(nodeName).trim();
            newNode.children = [];
            if (indent == 0) {
                // node is top level
                dataTree.add(newNode);
            } else if (indent > lastIndent) {
                // node is child of previous node
                lastNode.children.add(newNode);
                newNode.parent = lastNode;
            } else if (indent == lastIndent) {
                // node is same level as previous node
                lastNode.parent.children.add(newNode);
                newNode.parent = lastNode.parent;
            } else {
                // indent is less than last indent, so we need to add further up
                // the parent hierarchy
                var ti = lastIndent;
                var parent = lastNode.parent;
                while (ti > indent) {
                    parent = parent.parent;
                    ti -= 1;
                }
                parent.children.add(newNode);
                newNode.parent = parent;
            }
            lastNode = newNode;
            lastIndent = indent;
        }
        return dataTree;
    },

    getGridSettings : function (control, fieldNamingConvention) {
        if (!control) control = {};
        var mockData = control.dataSource.MockDataSource.mockData;

        control.autoFetchData = true;
        if (isc.isA.TreeGrid(this.creator)) {
            control.dataProperties = {openProperty: "isOpen"};
            return control;
        }

        // compute headerHeight based on number of rows in titles
        var value = mockData,
            row1 = value.split("\n")[0],
            vs = row1.split(","),
            maxRows = 1
        ;
        for (var i = 0; i < vs.length; i++) {
            if (vs[i].endsWith(" ^") || vs[i].endsWith(" v") ||
                    vs[i].endsWith(" ^v"))
            {
                var newVsi = vs[i].substring(0, vs[i].length - 2).trim();
                control.dataSource.MockDataSource.mockData = mockData =
                    mockData.replace(vs[i], newVsi);
            }
            maxRows = Math.max(maxRows, vs[i].split("\\r").length);
        }
        control.headerHeight = Math.max(25, 15 * maxRows);
        control.autoFitFieldWidths = true;
        control.autoFitWidthApproach = "title";
        if (control.leaveScrollbarGap == null) {
            control.leaveScrollbarGap = false;
        }

        var fields = isc.MockDataSource.parseTableFields(value, fieldNamingConvention),
            fieldsCorrect = isc.MockDataSource.parseTableFields(mockData, fieldNamingConvention),
            cacheData = isc.MockDataSource.parseTable(value)
        ;
        for (var i = 0; i < fields.length; i++) {
            var title = fields[i].title; 
            if (title.endsWith(" ^")) {
                control.sortField = fieldsCorrect[i].name;
            } else if (title.endsWith(" v")) {
                control.sortField = fieldsCorrect[i].name;
                control.sortDirection = "descending";
            }
        }
        control.dataSource.MockDataSource.fields = fieldsCorrect;
        control.dataSource.MockDataSource.cacheData = cacheData;

        return control;
    }
})

isc.MockDataSource.addProperties({
    //> @attr mockDataSource.mockData (String : "md" : IR)
    // Data intended for a +link{ListGrid} or +link{TreeGrid}, expressed in a simple text
    // format popularized by mockup tools such as +externalLink{http://balsamiq.com} and now
    // commonly supported in a variety of mockup tools.
    // <p>
    // Balsamiq publishes documentation of the grid format 
    // +externalLink{http://support.balsamiq.com/customer/portal/articles/110188-working-with-data-grids-tables,here},
    // with a simple example of using tree-specific formatting
    // +externalLink{https://support.mybalsamiq.com/projects/uilibrary/Tree%20Pane,here}.
    //
    // @visibility external
    //<
    mockData: "md",

    //> @type MockDataType
    // Whether the mock data is for a flat grid-like dataset or for a tree.  If "grid" is
    // specified, text shortcuts that would cause a hierarchy to be created (such as starting a
    // line with "[+]") will not have special meaning and be considered to be just a normal
    // data value.
    //
    // @value "grid"              Mock data for a ListGrid
    // @value "tree"              Mock data for a TreeGrid
    //
    // @visibility external
    //<

    //> @attr mockDataSource.mockDataType (MockDataType : "grid" : IR)
    // Whether +link{mockData} is in the "grid" or "tree" format.  See +link{MockDataType}.
    //
    // @visibility external
    //<
    mockDataType: "grid",

    clientOnly: true,
    cacheData: [],
    fields: [],

    // Override init to setup cacheData and fields using mockData
    init : function () {
        if (this.mockDataType == "grid") {
            this.fields = isc.MockDataSource.parseTableFields(this.mockData);
            this.cacheData = isc.MockDataSource.parseTable(this.mockData);
        } else if(this.mockDataType == "tree") {
            this.fields = [{
                name: "name",
                type: "text"
            }];
            this.cacheData = isc.MockDataSource.parseTree(this.mockData);
        }
        return this.Super("init", arguments);
    }
    
});
