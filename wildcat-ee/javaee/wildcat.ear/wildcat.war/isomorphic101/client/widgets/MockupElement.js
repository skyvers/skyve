/*
 * Isomorphic SmartClient
 * Version SNAPSHOT_v10.1p_2015-12-10 (2015-12-10)
 * Copyright(c) 1998 and beyond Isomorphic Software, Inc. All rights reserved.
 * "SmartClient" is a trademark of Isomorphic Software, Inc.
 *
 * licensing@smartclient.com
 *
 * http://smartclient.com/license
 */
//>	@class	MockupElement
// MockupElements are produced by the +link{group:balsamiqImport,Balsamiq Mockup Importer} as
// placeholders for Balsamiq controls that cannot be meaningfully translated to SmartClient
// controls (such as the big red X markup control).
// <p>
// MockupElement is just an instance of Img that uses .png files stored in the
// tools/visualBuilder/mockups folder.  
// <p>
// MockupElement is not intended to be included in any final applications.
//
// @treeLocation Client Reference/Tools
// @visibility external
//<


isc.overwriteClass("MockupElement", "Img");

isc.MockupElement.addProperties({
    controlName:"MockupElement",
    defaultWidth:16,
    defaultHeight:28,
    measuredW:-1,
    measuredH:-1
});
    
isc.MockupElement.addMethods({
    initWidget : function () {
        this.Super(this._$initWidget, arguments);
        var url = isc.Page.getToolsDir()+"visualBuilder/mockups/";
        var postfix = this.controlName.substr(this.controlName.indexOf("::") + 2, 
            this.controlName.length) + ".png";
        this.src=url + postfix;
        if (this.title != null) {
            this.addChild(
                isc.Label.create({
                    ID:this.getID() + "_titleLabel",
                    autoDraw:true,
                    left: 10,
                    top: 0,
                    width: this.width,
                    height: this.height,
                    zIndex: this.getZIndex(true) + 1,
                    contents: this.title
                })
            );
        }
    }
});
