/*
 * Isomorphic SmartClient
 * Version v10.0p_2014-09-10 (2014-09-10)
 * Copyright(c) 1998 and beyond Isomorphic Software, Inc. All rights reserved.
 * "SmartClient" is a trademark of Isomorphic Software, Inc.
 *
 * licensing@smartclient.com
 *
 * http://smartclient.com/license
 */

 

// SOAPUploadItem depends on MultiUploadItem, which requires ListGrid (not part of the forms module)
if (isc.ListGrid) {



//> @class SOAPUploadItem
// @visibility internal
//<
isc.ClassFactory.defineClass("SOAPUploadItem", "DialogUploadItem");
isc.SOAPUploadItem.addProperties({
    dataSource: "sessionFiles"
});

}