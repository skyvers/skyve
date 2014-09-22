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
//> @class EBay
//
// WebService object representing the eBay trading web service.
//
// @treeLocation Client Reference/Connectors
// @visibility EBay
//<

isc.EBay = isc.WebService.get("urn:ebay:apis:eBLBaseComponents");
isc.EBay.addProperties({
    
    //> @method EBay.setAuthToken()
    //
    // eBay web services require an authentication token to be passed with every request.  You
    // can obtain a token from +externalLink{http://developer.ebay.com/}.  Before using any
    // eBay web services, call this method to set the token provided to you by eBay.
    // 
    // @param authToken (string) Your authToken, provided to you by eBay. 
    //
    // @visibility EBay
    //<
    setAuthToken : function(authToken) {
        this._authToken = authToken;
    },
    getAuthToken : function () {
        if (this._authToken) return this._authToken;
        var authToken = this._useProduction ? this._productionToken : this._sandboxToken;
        if (!authToken) {
            this.logWarn("getAuthToken() called, but no token is available for the "
                        +(this._useProduction ? "production" : "sandbox")
                        +" environment.  Please provide your token(s) by editing"
                        +" /tools/visualBuilder/eBayAuthToken.js");
            return null;
        }
        return authToken;
    },

    //> @method EBay.setSandboxToken()
    //
    // Sets the token to be used by queries to the eBay sandbox environment.
    // 
    // @param authToken (string) Your sandbox authToken, provided to you by eBay. 
    //
    // @visibility EBay
    //<
    setSandboxToken : function (authToken) {
        this._sandboxToken = authToken;
    },

    //> @method EBay.setProductionToken()
    //
    // Sets the token to be used by queries to the eBay production environment.
    // 
    // @param authToken (string) Your production authToken, provided to you by eBay. 
    //
    // @visibility EBay
    //<
    setProductionToken : function (authToken) {
        this._productionToken = authToken;
    },

    //> @method EBay.setUseProduction()
    //
    // You can use web services in sandbox or production mode.  By default, this web service
    // will work in sandbox mode.  Call this method with a true value to switch to production
    // mode.
    //
    // @param useProduction (boolean)  true to use production, false to use sandbox
    // @visibility EBay
    //<
    setUseProduction : function (useProduction) {
        this._useProduction = useProduction;
    },

    //> @attr EBay.sandboxEndpoint       (URL : "https://api.sandbox.ebay.com/wsapi" : IR)
    //
    // The eBay sandbox requires the use of a different endpoint than that specified in the
    // WSDL - see  Getting Started > Invoking eBay Web Services > Routing the Request (Gateway
    // URLs)
    //
    // @visibility EBay
    //
    //<
    sandboxEndpoint: "https://api.sandbox.ebay.com/wsapi",
    getWebServiceEndpoint : function () {
        return this._useProduction ? this.dataURL : this.sandboxEndpoint;
    }
});

// eBay SOAP docs available here in HTML format:
// http://developer.ebay.com/DevZone/SOAP/docs/WebHelp/wwhelp/wwhimpl/js/html/wwhelp.htm
isc.defineClass("EBaySoapDataSource", "DataSource").addProperties({
    soapVersion: 509,
    transformRequest : function (request) {
        // apply the headerData to the request
        request.headerData = isc.addProperties({
            // SOAP header - overrideable by providing headerData on this class
            RequesterCredentials: {
                // dynamically pick up the authToken from the service and apply to every request
                eBayAuthToken: isc.EBay.getAuthToken()
            }            
        }, this.headerData);

        request.actionURL = isc.EBay.getWebServiceEndpoint();

        // various attributes of the request must be available as query params - presumably so
        // that eBay's routing logic can avoid the expense of parsing the request body.  See  
        // Getting Started > Invoking eBay Web Services > Routing the Request (Gateway URLs) 
        request.params = {
            callname: this.wsOperation,
            siteid: 0,
            version: this.soapVersion
        };

        // all requests must specify the version as part of the request body - see:
        // Getting Started > Invoking eBay Web Services > Standard Data for All Calls 
        request.data.version = this.soapVersion;
        return request.data;
    }
});

// encapsulation of GetCategories web service call - starter docs here:
// Selling Items on eBay > Categories > Retrieving the Category Hierarchy for a Site   
isc.defineClass("EBayCategoriesDS", "EBaySoapDataSource").addProperties({
//    ID: "itemCategoriesDS",
    resultTreeClass: "CategoryResultTree",
    serviceNamespace: "urn:ebay:apis:eBLBaseComponents",
    wsOperation: "GetCategories",
    recordXPath: "//default:Category",
    defaultCriteria: {
        LevelLimit: 1,
        CategorySiteID: 0,
        ViewAllNodes: true,
        DetailLevel: "ReturnAll"
    },
    
    transformRequest : function (request) {
        this.Super("transformRequest", arguments);
    
        // eBay requires no value when fetching the top-level categories
        if (request.data.CategoryParent == null) delete request.data.CategoryParent;

        // SmartClient trees typically provide just the parentId and expect that only children
        // will be sent back.  The eBay API allows you to specify a parent category and then an
        // absolute level - so to get the immediate children only, we specify the CategoryLevel
        // of the parentNode as the CategoryParent and then ask for parentNode depth + 1 for th
        // depth.
        var level = (request.parentNode ? request.parentNode.CategoryLevel : null);
        if (level != null) {
            request.data.CategoryLevel = new Number(level) + 1;
            request.data.LevelLimit = request.data.CategoryLevel;
        }
        return request.data;
    },
    transformResponse : function (response, request, data) {
        // strip parent category, which is included in the response
        if (request.data.CategoryLevel) {
            response.data = response.data.findAll("CategoryLevel", request.data.CategoryLevel+"");
        }
        return response;
    }
});

isc.defineClass("EBayGetItemDS", "EBaySoapDataSource").addProperties({
//	ID: "getItemDS",
    serviceNamespace: "urn:ebay:apis:eBLBaseComponents",
    wsOperation: "GetItem",
    recordXPath: "//default:Item",
    defaultCriteria: {
        DetailLevel: "ReturnAll",
        ItemTypeFilter: "AllItems"
    }
});
isc.EBayGetItemDS.create({
    ID: "getItemDS",
    fields: [
        {name: "ItemID"}
    ]
});


// encapsulation of GetCategoryListings web service call - starter docs here:
//  Searching for and Retrieving Items > Searching for Items > Finding All Item Listings in a Specified Category
isc.defineClass("EBayCategoryItemListingsDS", "EBaySoapDataSource").addProperties({
//	ID: "itemListingsDS",
    serviceNamespace: "urn:ebay:apis:eBLBaseComponents",
    wsOperation: "GetCategoryListings",
    recordXPath: "//default:Item",
    defaultCriteria: {
        DetailLevel: "ReturnAll",
        ItemTypeFilter: "AllItems"
    },
    maxPageOverflow: 10,
    transformRequest : function (request) {
        this.Super("transformRequest", arguments);

        if (request.startRow != null && request.endRow != null) {
            // SmartClient grids will request dataPageSize number of rows, at an arbitrary
            // rowStart.  The eBay API allows you to specify the number of rows you want per
            // page and the page number to fetch, so we need to do some math.
            //
            // optimal pageSize is endRow-startRow (Note: startRow/endRow are zero-based, so
            // need to add 1 for total number of rows to be fetched)
            var pageSize = request.endRow - request.startRow + 1;
    
            // the page number is 1-based for this interaction
            var pageNum = Math.ceil((request.endRow + 1) / pageSize);

            // if we get perfect page alignment with our pageSize, great - otherwise we will
            // need to fetch more rows than were strictly specified by pageSize.  This is ok
            // since the grid will simply integrate any additional rows into its cache.
            if ((request.endRow + 1) % pageSize != 0) {
                if ((request.endRow + 1) < (pageSize * 3)) {
                    pageSize = request.endRow + 1;
                    pageNum = 1;
                    request.startRow = 0;
                } else {
                    pageNum = Math.ceil(pageNum/2);
                    pageSize *= 2;
                    request.startRow = (pageNum-1)*pageSize + 1;
                }
            }

            request.data.Pagination = {
                PageNumber: pageNum,
                EntriesPerPage: pageSize
            }
        }
        return request.data;
    },
    transformResponse: function (response, request, xmlData) {
        // DataSource paging protocol - populate actual startRow, endRow,  totalRows
        response.data = response.data || [];
        if (response.data.length) {
            response.startRow = request.startRow;
            response.endRow = request.startRow + response.data.length;
            response.totalRows = xmlData.selectNumber("//default:TotalNumberOfEntries");
        }
        return response;
    }
});


// encapsulation of GetCategoryListings web service call - starter docs here:
//  Searching for and Retrieving Items > Searching for Items > Finding All Item Listings in a Specified Category
isc.defineClass("EBaySearchItemListingsDS", "EBayCategoryItemListingsDS").addProperties({
    wsOperation: "GetSearchResults"
});

    
// subclass of ResultTree to implement accurate isFolder handling
isc.defineClass("CategoryResultTree", "ResultTree").addProperties({
    isFolder : function (node) {
        if (node.LeafCategory) return node.LeafCategory == "false";
        return this.Super("isFolder", arguments);
    }
});
