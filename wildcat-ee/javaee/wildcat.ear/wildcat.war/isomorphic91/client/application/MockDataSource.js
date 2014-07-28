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





// ----------------------------------------------------------------------------------------

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
        var rawHeaders = rowsData[0].split(","),
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
            var name = isc.MockDataSource.convertTitleToName(rawHeaders[j], fieldNamingConvention);
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

    convertTitleToName : function (title, fieldNamingConvention) {
        var name = title.trim().replace(/(\\r|\r)/g, "_").replace(/[^a-zA-Z0-9_ ]/g, "_");
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
            } else if (nodeName.startsWith("-") || nodeName.startsWith("_")) {
                newNode.isFolder = false;
            }
            // strip out node metadata
            if (nodeName.startsWith("[")) nodeName = nodeName.substr(3);
            else if (newNode.isFolder) nodeName = nodeName.substr(1);
            else if (nodeName.startsWith("-") || nodeName.startsWith("_")) nodeName = nodeName.substr(2);
            
            newNode.name = isc.MockDataSource.parseTextWikiSymbols(nodeName);
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
    }
})

isc.MockDataSource.addProperties({
    mockData: "md",
    clientOnly: true,
    cacheData: [],
    fields: [],
    mockDataType: "grid",
    
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