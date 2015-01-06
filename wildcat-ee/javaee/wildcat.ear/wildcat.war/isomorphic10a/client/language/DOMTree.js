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

 
// DOMTree
// Builds 
isc.ClassFactory.defineClass("DOMTree", "Tree");

//!>Deferred
isc.DOMTree.addMethods({

    // root of the DOM to build a tree for
    //rootElement : null,

    // property of the TreeNode objects where the DOM node tag name should be copied
    tagNameProperty : "tagName",

    // property on each node where the DOM element is stored
    elementProperty : "_element",

    // a list of other attributes to copy from the DOM node to the TreeNode
    //copyAttributes : [],

    // Tree behavior to cause folders to be opened at initialization in a breadth-first manner
    // until a certain number of nodes (loadBatchSize) is visible
    loadOnInit : true,  

    // true in the sense that tree nodes are dynamically and lazily created from DOM nodes
    loadDataOnDemand:true,

    // stick the root element on the root node, for uniformity in accessing node._element
    makeRoot : function () {    
        var root = this.Super("makeRoot", arguments);
        root[this.elementProperty] = this.rootElement;
        return root;
    },

    getElement : function (node) { return node[this.elementProperty]; },

    // whether to show text nodes as distinct elements
    hideTextNodes: true,

    // Node Titles
    // ---------------------------------------------------------------------------------------
    getElementTitle : function (element) {
        // look for an attributes named after the title or "name" properties.  If this produces
        // undesireable results, just set titleProperty/nameProperty to some attribute that
        // doesn't exist
        var title = element.getAttribute(this.titleProperty);
        if (!this.valueIsEmpty(title)) return title;

        title = element.getAttribute(this.nameProperty);
        if (!this.valueIsEmpty(title)) return title;
        
        if (!isc.xml.hasElementChildren(element)) {
            title = isc.xml.getElementText(element);
            if (!this.valueIsEmpty(title)) return title;
        }
    
        // fall back to the tagName
        return element.tagName || element.nodeName;
    },
    valueIsEmpty : function (value) {
        return value == null || isc.isAn.emptyString(value);
    },

    // Folderness
    // ---------------------------------------------------------------------------------------
    isFolder : function (node) {
        if (node == this.root || node.children != null) return true;
        var element = node[this.elementProperty];
        if (!element || !element.childNodes || element.childNodes.length == 0) return false;

        // if we're not hiding text nodes, any child qualifies as a folder
        if (!this.hideTextNodes) return true;

        // otherwise show it as a folder only if it has element children, not just text
        return isc.xml.hasElementChildren(element);
    },

    // Modification
    // ---------------------------------------------------------------------------------------
    moveList : function (moveList, newParent, index) {
        var moveNode = moveList[0],
            moveElement = this.getElement(moveNode);

        this.logWarn("moveList: " + this.echoAll(moveList) +
                     ", newParent: " + this.echo(newParent) +
                     ", index: " + index);

        
        //moveElement.parentNode.removeChild(moveElement);
        this._addToDOM(moveElement, newParent, index);
        this.Super("moveList", arguments);
    },
    remove : function (node) {
        var element = this.getElement(node);
        element.parentNode.removeChild(element);
        return this.Super("remove", arguments);
    },
    // NOTE: overriding add() to auto-detect being passed an XML Element is painfully slow due
    // to the Super call..
    addElement : function (element, parent, position) {
        this._addToDOM(element, parent, position);
        // an already loaded parent needs add called - otherwise the child will be
        // discovered when loadChildren is called
        if (this.isLoaded(parent)) {
            var node = this.nodeForElement(element);
            this.add(node, parent, position);
        } else {
            // otherwise we need to fire dataChanged() in order to cause an opener icon to be
            // rendered on any node that becomes a folder due to DOM addition
            this.dataChanged();
        }
    },
    
    _addToDOM : function (addElement, parent, position) {
        var parentElement = this.getElement(parent);
        if (position == null) {
            this.logWarn("appending: " + this.echoLeaf(addElement) +
                         " to: " + this.echoLeaf(parentElement));
            parentElement.appendChild(addElement);
        } else {
            // NOTE: the index in the visible nodes may not be the same as the index in the
            // DOM, since a DOMTree may not show all DOM nodes (eg it might hide text nodes).
            var beforeNode = this.getChildren(parent)[position],
                beforeElement = this.getElement(beforeNode);
            this.logWarn("inserting into: " + this.echoLeaf(parentElement) + 
                         ", before: " + this.echoLeaf(beforeElement));
            parentElement.insertBefore(addElement, beforeElement);
        }
    },

    // Loading (creating TreeNodes from DOM nodes)
    // ---------------------------------------------------------------------------------------

    // create a TreeNode from a DOM element
    nodeForElement : function (element) {
        var node = {};

        node[this.elementProperty] = element;

        // heuristically determine a title
        node[this.titleProperty] = this.getElementTitle(element);

        // copy the tagName from the DOM node to the tagName property of the TreeNode
        if (this.tagNameProperty) {
            node[this.tagNameProperty] = element.tagName || element.nodeName;
        }

        // copy other attributes if so configured
        if (this.copyAttributes) {
            for (var j = 0; j < this.copyAttributes.length; j++) {
                var attribute = this.copyAttributes[j];
                node[attribute] = element.getAttribute(attribute);
            }
        }
        return node;
    },

    loadChildren : function (parentNode) {
        if (this.isLoaded(parentNode)) return;
        try {
        //this.logWarn("loading children of: " + 
        //             (parentNode == this.root ? "root" : this.echoLeaf(parentNode._element)));

        var element = parentNode._element;
        if (element == null) return;
        var childNodes = element.childNodes;

        // allow inspection through IFRAMEs to get into SVGs in Moz
        if (isc.Browser.isMoz && element.contentDocument) {
            childNodes = [element.contentDocument.documentElement];
        } else {
            // defer loading uninteresting children if loading a batch
            // (IFRAMEs are exempt from this check)
            if (this.loadingBatch() && !isc.xml.hasElementChildren(element)) return;
        }

        parentNode[this.openProperty] = true;

        if (childNodes != null) {
            // make a node for each DOM child
            for (var i = 0; i < childNodes.length; i++) {
                var domNode = childNodes[i];
                // skip text, CDATA and comment nodes if so configured
                if (this.hideTextNodes && domNode.nodeName.startsWith("#")) continue;

                var node = this.nodeForElement(domNode);

                this.add(node, parentNode);
            }
        }

        this.setLoadState(parentNode, isc.Tree.LOADED);
        } catch (e) {
            this.logWarn("parent node: " + this.echo(parentNode) +
                         ", at path: " + this.getPath(parentNode) + 
                         ", error: " + this.echo(e) +
                         this.getStackTrace());
        }
    }
});
//!<Deferred

