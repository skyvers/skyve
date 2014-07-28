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



//> @groupDef treeDataBinding
// 
// The SmartClient +link{TreeGrid} component is a visual representation of a tree and requires
// a +link{Tree} or +link{ResultTree} datatype passed via the +link{TreeGrid.data} attribute to
// initialize the tree view.  The +link{Tree} datatype is used when you want to provide all of
// the tree nodes in one shot at initialization time.  The +link{ResultTree} datatype is used
// when you want portions of the tree to be loaded on demand from the server.
// <p>
// <b>Providing all data to the Tree at creation</b>
// <p>
// The simplest mechanism by which to initialize a Tree is to simply provide all the data
// up-front when the Tree itself is created.  Depending on the format of your tree data, this
// can be done by setting +link{Tree.root} or +link{Tree.data}.  This functionality is provided
// by the +link{Tree} class.
// <p>
// For examples of this type of databinding, see the following SDK examples:
// <ul>
// <li>+explorerExample{childrenArrays, TreeGrid Initialization Example}</li>
// <smartclient>
// <li>+externalLink{/examples/server_integration/#jstlTree, TreeGrid Initialization with JSTL}</li>
// </smartclient>
// </ul>
// <p>
// <b>Loading Tree nodes on demand</b>
// <p>
// In this mode, tree nodes are loaded on-demand the first time a user expands a folder.  This
// approach is necessary for large trees.  This functionality is provided by the
// +link{ResultTree} class, which uses a +link{DataSource} to load data from the server.  Each
// DataSource Record becomes a +link{TreeNode}.
// <p>
// When the user expands a folder whose children have not yet been loaded
// from the server (or you programmatically call openFolder() on such a node), the client
// automatically sends a +link{DSRequest} to the server to ask for all immediate children of
// that node.  
// <P>
// If you have a dataset that is +link{TreeModelType,"parent-linked"}, that is, every node has
// a unique ID (the +link{tree.idField}) and also has a property with the unique ID of it's
// parent node (the +link{tree.parentIdField}) the tree can load child nodes by simply sending
// a DSRequest with appropriate +link{Criteria}.  Given a parent node with ID "225" in a tree
// where the +link{tree.parentIdField} is called "parentId", the criteria would be:
// <pre>
//    { parentId : 225 }
// </pre>
// The client is asking the server: "give me all nodes whose parentId is 225", which are the
// children of node 225.  
// <P>
// If you have a DataSource that supports simple +link{Criteria} like the above, and your
// records have nodes with ids and parentIds, this strategy can be used by just declaring the
// tree relationship in your DataSource: the tree will automatically use your
// +link{DataSourceField.primaryKey} field as the +link{Tree.idField}.  To declare the
// +link{Tree.parentIdField}, declare a +link{DataSourceField.foreignKey} field with the
// name of the primaryKey field.
// <P>
// If you have a tree where there is no convenient unique ID, for example, you have mixed types
// of nodes (for example, departments and employees), use one of the following approaches:
// <ol>
// <li> generate a synthetic node ID and return it with every tree node.
// <P>
// Typically two or more properties can be combined into a String that serves as a unique ID.
// For example, if you are loading a mixed tree of "Departments" and "Users", each of which
// have unique numeric IDs, you could generate synthetic node IDs like "department:353" and
// "user:311".  Your server-side code will then receive these synthetic node IDs when the tree
// loads children, and you can parse the IDs, look up the appropriate object and return its
// child nodes.
// <P>
// In the case of filesystems or XML documents, you can use the full path to the file or XML
// element as the unique ID.
// <P>
// <li> have all properties of the parentNode +link{DataSource.sendParentNode,sent to the server}
// <P>
// If having all the properties of the parentNode would allow you to look up children, this
// approach may be more convenient than having to generate synthetic node IDs and parse them
// when looking up children.
// <P>
// For example, with a mixed-type tree, your server-side code may be able to quickly identify
// the type of the parent node be looking for specific properties, and then call methods to
// look up children for that type of node.
// <P>
// In this case there is no need to declare an idField or parentIdField.
// </ol>
// <P>
// +link{ResultTree}s are created for you by the +link{TreeGrid} when you set
// +link{TreeGrid.dataSource}, but you can pass an initial dataset to a databound TreeGrid by
// setting +link{TreeGrid.initialData}.
// <P>
// If you do not provide +link{TreeGrid.initialData}, the first DSRequest you receive will be a
// request for the nodes under root.  The id of the root node of the tree is the value of the
// <code>rootValue</code> attribute on the +link{Tree.parentIdField} of the Tree DataSource. 
// <p>
// For examples of this type of databinding, see the following SDK examples:
// <ul>
// <li>+explorerExample{initialData, TreeGrid DataBinding Example}</li>
// <smartclient>
// <li>+externalLink{/examples/server_integration/#xml2JSLOD, TreeGrid XML DataBinding}
// </smartclient>
// </ul>
// <P>
// <b>Folders and load on demand</b>
// <P>
// When using load on demand, the Tree cannot simply check whether a node has children to
// determine whether it's a folder, and will assume all loaded nodes are folders.  To avoid
// this, you can add a boolean field to your DataSource called "isFolder" that indicates
// whether a node is a folder or not.  If you already have a boolean field that indicates
// whether a node is a folder, you can instead set +link{tree.isFolderProperty} to the name of
// that field via +link{TreeGrid.dataProperties}.
// <P>
// <b>Multi-Level load on demand</b>
// <P>
// The ResultTree's DSRequests ask for the immediate children of a node only (by specifying
// <code>parentId</code> in the criteria). Any nodes returned whose <code>parentId</code> field
// value is unset or matches this criterion will be added to the tree as immediate children of the
// node. However you are also free to return multiple levels of children.  This can be done by
// simply returning a flat list of descendents with valid id's and parentId's, exactly as though 
// you were initializing a multi-level tree via +link{Tree.data}.  
// <P>
// Note that when receiving multiple levels of children, the ResultTree's assumption is that
// if any children are loaded for a parent, then that parent is considered fully loaded.
// <P>
// When loading children for a given parent node, the ResultTree calls
// +link{DataSource.fetchData} on its DataSource.  For custom code that may need to reference
// the parentNode or tree in some way, the parent node whose children are being loaded is
// available on the dsRequest instance in the DataSource flow as dsRequest.parentNode, where it
// can be inspected during +link{DataSource.transformRequest()}.
// <P>
// For an example of this feature, see the following SDK example:
// <ul>
// <li>+explorerExample{multiLevelLOD,Multi-Level Load on Demand Example}</li>
// </ul>
// <P>
// <b>Paging large sets of children</b>
// <p>
// If some nodes in your tree have a very large number of immediate children, you can enable
// +link{resultTree.fetchMode,fetchMode:"paged"} to load children in batches.  This means that
// whenever the children of a folder are loaded, the <code>resultTree</code> will set
// +link{dsRequest.startRow} and +link{dsRequest.endRow,endRow} when requesting children from
// the DataSource.  This includes the initial fetch of top-level nodes, which are children of
// the +link{tree.showRoot,implicit root node}.
// <p>
// As with all paged DSRequests, the server is free to ignore startRow/endRow and
// simply return all children of the node.  This allows the server to make on-the-fly
// folder-by-folder choices as to whether to use paging or just return all children.  However,
// whenever the server returns only some children, the server must provide an accurate value for
// +link{dsResponse.totalRows}.
// <p>
// If the server does return a partial list of children, the <code>resultTree</code> will
// automatically request further children as they are accessed; typically this happens because
// the user is scrolling around in a +link{TreeGrid} which is viewing the
// <code>resultTree</code>.
// <p>
// In this mode, the server may return multiple levels of the tree as described above
// ("Multi-Level load on demand"), however, by default the server is not allowed to return
// folders that are open, as this creates a potential performance issue: consider the case of a
// user scrolling rapidly into an unloaded area of the tree, skipping past many nodes that have
// not been loaded.  If the skipped nodes might have been open parents, then the only way to
// know what nodes should be visible at the new scroll position is to load all skipped nodes
// and discover how many visible children they had.
// <p>
// If this performance consequence is acceptable, the restriction against returning open
// folders from the server may be lifted on a tree-wide basis by setting the
// +link{resultTree.canReturnOpenFolders,canReturnOpenFolders} property to <code>true</code>
// and/or on a folder-by-folder basis by setting the property named by the
// +link{resultTree.canReturnOpenSubfoldersProperty,canReturnOpenSubfoldersProperty} to
// <code>true</code>.  In this case, it is recommended to also set
// +link{resultTree.progressiveLoading,progressiveLoading} to <code>true</code> to prevent
// users from causing a large number of nodes to be loaded by scrolling too far ahead in the
// tree.
// <p>
// In addition, if any folder is returned already open, it must include children via the
// +link{tree.childrenProperty,childrenProperty} or there will be an immediate, new fetch to
// retrieve the children.  When returning children, a partial list of children may be
// returned, but if so, the +link{resultTree.childCountProperty,childCountProperty} must be
// set to the total number of children.
// <p>
// Paged ResultTrees may also be filtered like other trees (see
// +link{resultTree.setCriteria}).  However, if +link{resultTree.keepParentsOnFilter} is
// enabled then server filtering is required.  To illustrate with an example, consider a case
// where the ResultTree has 10,000 folders at root level and where criteria applied to their
// children would eliminate all but 20, which happen to be at the end of the 10,000.  Purely
// client-side logic would have to perform 10,000 fetch operations to check whether each
// root-level node had children before arriving at the final set of 20.
// <p>
// For examples of this feature, see the following SDK example:
// <ul>
// <li>+explorerExample{pagingForChildren,Paging for Children Example}</li>
// </ul>
// <p>
// <b>NOTE:</b> trees with thousands of visible nodes are very difficult for end users to
// navigate.  A <b>majority of the time</b> the best interface for showing a very large tree
// is to show a TreeGrid that displays just folders, adjacent to a ListGrid that shows items
// within those folders.
// <p>
// For example, the data in your email account can be thought of as an enormous tree of
// folders (Inbox, Sent, Drafts, Trash etc) with thousands of messages in each folder.
// However, none of the common email clients display email this way; all of them choose to
// show folders and messages separately, as this is clearly more usable.
// <p>
// Before starting on implementing paging within sets of children, carefully consider whether
// an interface like the above, or some entirely different interface, is actually a superior
// option.  It is exceedingly rare that paging within sets of children is the best choice.
//
// @title Tree DataBinding
// @treeLocation Client Reference/Data Binding
// @visibility external
//< 


//>	@class ResultTree
// ResultTrees are an implementation of the +link{class:Tree} API, used to handle hierarchical
// data, whose nodes are DataSource records which are retrieved from a server.
//
// @visibility external
// @treeLocation    Client Reference/Data Binding
//<
isc.ClassFactory.defineClass("ResultTree", isc.Tree);

isc.ResultTree.addClassProperties({
    
    getLoadingMarker : function () {
        return (isc.ResultSet != null ? isc.ResultSet.getLoadingMarker() : Array.LOADING);
    }
});

isc.ResultTree.addProperties({
    nameProperty:"__nodePath",
    nodeTypeProperty:"nodeType",
    childTypeProperty:"childType",
    modelType: "parent",

    // DataModel
    // ---------------------------------------------------------------------------------------

    //> @attr resultTree.data (List of TreeNode : null : IRA)
    // Optional initial data for the tree.  If the +link{resultTree.fetchMode,fetchMode} is
    // <code>"basic"</code> or <code>"local"</code> then the format of this data is exactly
    // the same +link{tree.parentIdField,parentId}-linked list of tree nodes as
    // documented on +link{Tree.data} (when the <code>modelType</code> is set to
    // <code>"parent"</code>).  If the <code>fetchMode</code> is <code>"paged"</code> then the
    // format is extended to allow the +link{resultTree.childCountProperty,childCountProperty}
    // to be set on folder nodes.
    // <P>
    // Providing an initial set of nodes in this way does not affect the behavior of the
    // ResultTree in its loading of unloaded folders.  An equivalent result is achieved if the
    // first fetch from the server returns this same data.
    // <P>
    // If <code>fetchMode</code> is <code>"paged"</code> then you may make folder-by-folder
    // choices as to whether to use paging for the childen of each folder.  If you would like
    // to use paging in a folder then you may include a partial list of that folder's children
    // with the data, provided that you set the <code>childCountProperty</code> to the total
    // number of children.  Otherwise you will need to include either all children of the
    // folder or none of the children.  Open folders without any children provided will cause
    // immediate, new fetches for the children, as usual.
    // <P>
    // Because the initial data is treated exactly as though it were returned from the tree's
    // first server fetch, the order of the initial data must match the initial sort order of
    // the TreeGrid displaying the data or, if no such sort is specified, the native storage
    // order on the server.  For example, consider initial data containing <code>n</code>
    // records having the <code>parentId</code> <code>"X"</code>, meaning they are all in
    // the same folder.  These <code>n</code> records are the records at indices
    // <code>0</code> through <code>(n - 1)</code> that are stored on the server under the
    // parent node.  If the <code>childCountProperty</code> set on the parent node indicates
    // that there are <code>m > n</code> total rows under the parent node then the records at
    // indices <code>n</code> to <code>(m - 1)</code> will be fetched from the server as the user
    // scrolls the additional rows into view.
    //
    // @see attr:Tree.data
    // @see TreeNode
    // @group treeDataBinding
    // @visibility external
    //<

    //> @attr resultTree.dataSource (DataSource or ID : null : IR)
    //  What +link{class:DataSource} is this resultTree associated with?
    //
    // @include dataBoundComponent.dataSource
    // @visibility external
    //<

    //> @attr resultTree.context (OperationContext : null : IRA)
    // OperationContext to be sent with all operations performed by this ResultTree.
    //<

    //> @attr resultTree.loadDataOnDemand (Boolean : true : IR)
    // Does this resultTree load data incrementally as folders within the tree are opened, or
    // is it all loaded in a single request?
    // @see treeGrid.loadDataOnDemand
    // @visibility external
    //<
    loadDataOnDemand:true,
    
    //> @attr resultTree.autoPreserveOpenState (PreserveOpenState : "whenUnique" : IRW)
    // Controls what happens to the +link{getOpenState(),"open state"} - the set of 
    // nodes opened or closed by the end user after tree data is loaded - when an entirely
    // new tree of nodes is loaded from the server, as a consequence of calling 
    // +link{invalidateCache()} or of changing criteria such that the current cache of
    // nodes is dropped.
    // @visibility external
    //<
    autoPreserveOpenState:"whenUnique",
    
    //> @type PreserveOpenState
    // @value never   
    //  Never try to automatically preserve the openState.  Nodes will be initially open 
    //  or closed based solely on the +link{tree.openProperty} optionally set by the server.
    // @value whenUnique
    //  If either the +link{tree.idField} or +link{tree.nameProperty} has been set on 
    //  the Tree, (so that nodes have either unique ids or unique paths), 
    //  preserve openState by respecting the +link{tree.openProperty} set by the server, 
    //  then applying the openState.
    // @value always
    //  Like "whenUnique" but automatically preserves openState even if nodes cannot be
    //  uniquely identified.  This means that nodes at the same tree positions 
    //  (eg 3rd child of 5th node under root) will be placed in the same openState, 
    //  regardless of whether that node has anything to do with the node that previously 
    //  was at that tree position.
    //
    // @visibility external
    //<

    //> @attr resultTree.fetchMode (FetchMode : "basic" : IR)
    // Mode of fetching records from server.
    // <P>
    // fetchMode:"local" implies that local filtering will always be performed. See
    // +link{keepParentsOnFilter} for additional filtering details.
    // <P>
    // fetchMode:"basic" or "paged" implies that if search criteria change, the entire
    // tree will be discarded and re-fetched from the server.  When retrieving the replacement
    // tree data, the default behavior will be to preserve the +link{getOpenState,openState}
    // for any nodes that the server returns which were previously opened by the user.  Note
    // that this implies that if +link{loadDataOnDemand} is enabled and the server returns only
    // root-level nodes, open state will be preserved only for root-level nodes, and children
    // of open root-level nodes will be immediately fetched from the server if
    // they are not included in the server's initial response.
    // <P>
    // fetchMode:"paged" enables paging for nodes that have very large numbers of children.
    // Whenever the children of a folder are loaded, the <code>resultTree</code> will set
    // +link{dsRequest.startRow} and +link{dsRequest.endRow,endRow} when requesting children
    // from the DataSource, and will manage loading of further children on demand, similar to
    // how a +link{ResultSet} manages paging for lists.  For a deeper discussion see the
    // <b>Paging large sets of children</b> section of the +link{group:treeDataBinding}
    // overview.
    //
    // @example pagingForChildren
    // @group treeDataBinding
    // @visibility external
    //<
    fetchMode:"basic",
    

    //> @attr resultTree.resultSize (integer : 75 : IRA)
    // How many tree nodes to retrieve at once from each large set of children in the tree.
    // <P>
    // Applicable only with <code>fetchMode: "paged"</code>.  When a paged ResultTree is asked
    // for rows that have not yet been loaded, it will fetch adjacent rows that are likely to
    // be required soon, in batches of this size.
    // @group treeDataBinding
    // @visibility external
    //<
    resultSize: 75,

    
    _childrenResultSetProperties: {
        fetchMode: "paged",

        
        _dataAdd : function (records, length, rowNum) {
            var tree = this._tree,
                parentNode = this._parentNode;
            tree._childrenDataAdd(this, parentNode, records, length, rowNum, true);
        },
        _dataAdded : function (records, length, rowNum) {
            var tree = this._tree,
                parentNode = this._parentNode;
            tree._childrenDataAdd(this, parentNode, records, length, rowNum, false);
        },
        _dataRemove : function (records, length, rowNum) {
            var tree = this._tree,
                parentNode = this._parentNode;
            tree._childrenDataRemove(this, parentNode, records, length, rowNum, true);
        },
        _dataRemoved : function (records, length, rowNum) {
            var tree = this._tree,
                parentNode = this._parentNode;
            tree._childrenDataRemove(this, parentNode, records, length, rowNum, false);
        },
        _dataSplice : function (originalRecords, originalLength, rowNum, updatedRecords, updatedLength) {
            var tree = this._tree,
                parentNode = this._parentNode;
            tree._childrenDataSplice(
                this, parentNode, originalRecords, originalLength, rowNum,
                updatedRecords, updatedLength, true);
        },
        _dataSpliced : function (originalRecords, originalLength, rowNum, updatedRecords, updatedLength) {
            var tree = this._tree,
                parentNode = this._parentNode;
            tree._childrenDataSplice(
                this, parentNode, originalRecords, originalLength, rowNum,
                updatedRecords, updatedLength, false);
        },
        _dataMoved : function (records, length, originalRowNum, updatedRowNum) {
            var tree = this._tree,
                parentNode = this._parentNode;
            tree._childrenDataMoved(this, parentNode, records, length, originalRowNum, updatedRowNum);
        },
        _dataLengthIsKnownChanged : function (originalValue, updatedValue) {
            var tree = this._tree,
                parentNode = this._parentNode;
            tree._childrenDataLengthIsKnownChanged(this, parentNode, originalValue, updatedValue);
        }
    },

    //> @attr resultTree.childCountProperty (String : "childCount" : IR)
    // When using +link{fetchMode,fetchMode:"paged"} and providing multiple levels of the tree in
    // one DSResponse, <code>childCountProperty</code> must be set for any folders that include
    // only a partial list of children.
    // For a deeper discussion see the <b>Paging large sets of children</b> section of the
    // +link{group:treeDataBinding} overview.
    // @example multiLevelChildPaging
    // @visibility external
    //<
    childCountProperty: "childCount",

    //> @attr resultTree.canReturnOpenSubfoldersProperty (String : "canReturnOpenSubfolders" : IR)
    // When using +link{fetchMode,fetchMode:"paged"} and providing multiple levels of the tree
    // in one DSResponse, <code>canReturnOpenSubfoldersProperty</code> may be set on any
    // folder to indicate whether child folders might be returned by the server already open.
    // If the property is set to false on a folder then subfolders of that folder are never
    // allowed to be returned already open.  This enables the paging mechanism to be more
    // efficient in the amount of data that it requests from the server.
    // <P>
    // For example, setting the <code>canReturnOpenSubfoldersProperty</code> value to
    // <code>false</code> on a node is appropriate if the server-side code determines that the
    // the node's children consist of entirely leaf nodes.
    // @see resultTree.canReturnOpenFolders
    // @visibility external
    //<
    canReturnOpenSubfoldersProperty: "canReturnOpenSubfolders",

    //> @attr resultTree.canReturnOpenFolders (boolean : false : IR)
    // When using +link{fetchMode,fetchMode:"paged"} and providing multiple levels of the tree
    // in one DSResponse, this property specifies the default value assumed for the
    // +link{canReturnOpenSubfoldersProperty} when no value for that property is provided for
    // a node.
    // @visibility external
    //<
    canReturnOpenFolders: false,

    //> @attr resultTree.progressiveLoading (boolean : null : IRW)
    // Sets +link{DataSource.progressiveLoading,progressive loading mode} for this
    // ResultTree.  The ResultTree will copy this setting onto the +link{DSRequest}s that it
    // issues, overriding the OperationBinding- and DataSource-level settings, in cases where
    // the use of progressive loading does not affect the correctness of the tree's paging
    // algorithm.
    // <p>
    // This setting is applied automatically by +link{DataBoundComponent}s that have their own
    // explicit setting for +link{DataBoundComponent.progressiveLoading,progressiveLoading}.
    // <p>
    // <b>Note:</b>  This property only has an effect for +link{fetchMode,fetchMode:"paged"}
    // ResultTrees.
    // @see dataSource.progressiveLoading
    // @see operationBinding.progressiveLoading
    // @see dsRequest.progressiveLoading
    // @see dataBoundComponent.progressiveLoading
    // @group progressiveLoading
    // @visibility external
    //<

    //> @attr resultTree.defaultIsFolder (boolean : null : IR)
    // Controls whether nodes are assumed to be folders or leaves by default.
    // <P>
    // Nodes that have children or have the +link{tree.isFolderProperty,isFolderProperty} set
    // to true will always be considered folders.  Other nodes will be considered folders or
    // leaves by default according to this setting.
    // <P>
    // If <code>defaultIsFolder</code> is unset, the ResultTree will automatically set it to
    // match the value of +link{loadDataOnDemand}.  This means that, when using
    // folder-by-folder load on demand (<code>loadDataOnDemand:true</code>), by default a newly
    // loaded node will be considered to be a folder that has not loaded its children yet.  
    // <P>
    // When not using folder-by-folder load on demand, by default a newly loaded node is
    // considered a leaf.  If you set <code>defaultIsFolder:true</code> explicitly, by default
    // a newly loaded node is considered to be a folder with no children.
    // <P> 
    // See +link{Tree.isFolder()} for details on how to explicitly mark nodes as folders or leaves.
    //
    // @see treeGrid.loadDataOnDemand
    // @visibility external
    //<
    
    //> @attr resultTree.rootNode (any : null :IR)
    // This attribute may be used to specify a root value for the parentIdField of this resultTree.
    // This overrides the default +link{DataSourceField.rootValue} for this tree, allowing
    // a component to navigate a tree starting at a specific node.
    // <P>
    // May be overridden via +link{TreeGrid.treeRootValue} for ResultTrees generated by a TreeGrid
    // component.
    // @visibility external
    //<
        
    //> @attr resultTree.discardParentlessNodes (boolean : null : IRA)
    // When data is loaded from the server, should nodes with an explicit value for
    // the +link{tree.parentIdField} which doesn't map to a valid parent node be dropped?
    // If set to false, for +link{TreeGrid.loadDataOnDemand}:false trees, parentless nodes will be
    // added as children of the root node - for +link{TreeGrid.loadDataOnDemand}:true, they will be
    // added as children of the folder currently requesting children.
    // <P>
    // This effectively allows nodes to be loaded into the current (or root) folder without
    // needing an explicit +link{tree.parentIdField,parentIdField value} that matches the folder's
    // ID or <code>rootValue</code> for the resultTree.
    // <P>
    // Note: For <code>loadDataOnDemand:false</code> trees, if this property is unset at init time,
    // it will default to <code>true</code> if an explicit +link{resultTree.rootNode} has been
    // specified. This ensures that if the data tree retrieved from the server includes ancestors
    // of the desired root-node we don't display them. Otherwise this property always defaults to
    // false.
    // @visibility external
    //<
    
    //>@attr ResultTree.defaultNewNodesToRoot (Boolean : false : IRWA)
    // This attribute governs how to handle cache-synch when a new node is added to this dataSource 
    // with no explicit parentId.
    // <P>
    // If set to <code>true</code>, when a new node is added to this dataSource via
    // +link{DataSource.addData()}, with no explicit parentId, the node will be added as a 
    // child of the root node of this result tree. Otherwise it will be ignored.
    // <P>
    // Similar logic applies to +link{DataSource.updateData(),updated nodes} - if this property is
    // true and the parentId of an updated node is cleared, it will be moved to become a child of
    // root, otherwise it will be dropped from the tree.
    // @visibility external
    //<
    defaultNewNodesToRoot:false,

    
    //> @attr resultTree.updateCacheFromRequest (Boolean : true : IRA) 
    // When a successful Add, Update or Remove type operation fires on this ResultTree's 
    // dataSource, if +link{dsResponse.data} is unset, should we integrate the submitted
    // data values (from the request) into our data-set?
    //
    // @group cacheSync
    // @visibility external
    //<
    updateCacheFromRequest:true
    
    //> @attr   resultTree.disableCacheSync (Boolean : false : IRA)
    // By default when the data of this ResultTree's dataSource is modified, the ResultTree will
    // be updated to display these changes.
    // Set this flag to true to disable this behavior.
    // @group cacheSync
    // @visibility external
    //<

    // Filtering
    // ----------------------------------------------------------------------------------------

    //> @attr resultTree.keepParentsOnFilter (boolean : null : IR)
    // If set, tree-based filtering is performed such that parent nodes are kept as long as
    // they have children that match the filter criteria, even if the parents themselves do not
    // match the filter criteria. If not set, filtering will exclude parent nodes not matching
    // the criteria and all nodes below it in the tree.
    // <P>
    // When <code>keepParentsOnFilter</code> is enabled for paged ResultTrees, server-side
    // filtering is required.
    // <P>
    // When enabled for non-paged trees, +link{fetchMode,fetchMode:"local"} is automatically
    // enabled so that all filtering behavior shifts to the client-side and full criteria are
    // no longer sent to the server.  Instead, server fetches will always load all nodes, or
    // with +link{loadDataOnDemand}:true, will always load all nodes under a given parent.
    // This means that the server does not need to implement special tree filtering logic.
    // <P>
    // Optionally, +link{resultTree.serverFilterFields} can be set to a list of field names that
    // will be sent to the server whenever they are present in the criteria.
    // @group treeDataBinding
    // @visibility external
    //<
    

    //> @attr resultTree.serverFilterFields (Array of String : null : IR)
    // When +link{keepParentsOnFilter} is enabled for +link{fetchMode,fetchMode:"local"}
    // ResultTrees, this property lists field names that will be sent to the server if they
    // are present in the criteria.
    // @visibility external
    //<
});

isc.ResultTree.addMethods({


add : function (node, parent, position) {
    if (this.isPaged()) {
        isc.logWarn(
            "ResultTrees with fetchMode \"paged\" are read-only.  This operation (add) will " +
            "be ignored.");
    } else {
        return this.invokeSuper(isc.ResultTree, "add", node, parent, position);
    }
},
addList : function (nodeList, parent, position) {
    if (this.isPaged()) {
        isc.logWarn(
            "ResultTrees with fetchMode \"paged\" are read-only.  This operation (addList) " +
            "will be ignored.");
    } else {
        return this.invokeSuper(isc.ResultTree, "addList", nodeList, parent, position);
    }
},
linkNodes : function (records, idProperty, parentIdProperty, rootValue, isFolderProperty, contextNode, suppressDataChanged) {
    if (this.isPaged()) {
        isc.logWarn(
            "ResultTrees with fetchMode \"paged\" are read-only.  This operation " +
            "(linkNodes) will be ignored.");
    } else {
        return this.invokeSuper(
            isc.ResultTree, "linkNodes",
            records, idProperty, parentIdProperty, rootValue, isFolderProperty, contextNode,
            suppressDataChanged);
    }
},
move : function (node, newParent, position) {
    if (this.isPaged()) {
        isc.logWarn(
            "ResultTrees with fetchMode \"paged\" are read-only.  This operation (move) " +
            "will be ignored.");
    } else {
        return this.invokeSuper(isc.ResultTree, "move", node, newParent, position);
    }
},
remove : function (node, noDataChanged) {
    if (this.isPaged()) {
        isc.logWarn(
            "ResultTrees with fetchMode \"paged\" are read-only.  This operation (remove) " +
            "will be ignored.");
    } else {
        return this.invokeSuper(isc.ResultTree, "remove", node, noDataChanged);
    }
},
removeList : function (nodeList) {
    if (this.isPaged()) {
        isc.logWarn(
            "ResultTrees with fetchMode \"paged\" are read-only.  This operation " +
            "(removeList) will be ignored.");
    } else {
        return this.invokeSuper(isc.ResultTree, "removeList", nodeList);
    }
},


//>	@method	resultTree.init()	(A)
//			Initialize this ResultTree.  Pass in objects with properties to add or override
//			defaults.
//
//		@param	[all arguments]	(object)	objects with properties to override from default
//<
init : function (a,b,c,d,e,f) {
	// create a pointer to us in the global context
	isc.ClassFactory.addGlobalID(this);

	if (!this.criteria) this.criteria = {};

    if (!this.operation) this.operation = {operationType : "fetch"};

    // dataSource can be specified either on the operation or the ResultTree.
    if (!this.dataSource) this.dataSource = this.operation.dataSource;
    if (!this.operation.dataSource) this.operation.dataSource = this.dataSource;
    
    
    if (isc.isAn.Array(this.dataSource)) {
        this.dataSource = this.dataSource[0];
        this.operation.dataSource = this.dataSource;
    }
    
    // If any of rootValue, idField, parentIdField are not explicitly specified on this
    // ResultTree, autodetect them from the DataSource relationship.
    if (!this.isMultiDSTree()) {
        
        // root node has to exist for getTreeRelationship to work, so create it now if it
        // doesn't exist 
        if (!this.root) this.root = this.makeRoot();
        var relationship = this.getTreeRelationship(this.root);

        var undef;
        // compare to undef because rootValue can be set to null
        if (this.rootValue === undef) this.rootValue = relationship.rootValue;
        
        // If we're not loading on demand, and the rootValue is not null/undef,
        // 'discardParentlessNodes' to true.
        // This ensures that if we load an entire tree, and have a rootValue set to pick up 
        // a sub-tree of that, we don't add the full tree's top level element to root and thus
        // show the entire tree
        if (!this.loadDataOnDemand && 
            (this.rootValue != null || (this.root != null && this.root[this.idField] != null)) &&
            this.discardParentlessNodes == null)
        {
            this.discardParentlessNodes = true;
        }
        
        if (this.idField == null) this.idField = relationship.idField;
        if (this.parentIdField == null) this.parentIdField = relationship.parentIdField;
        if (relationship.childrenProperty) this.childrenProperty = relationship.childrenProperty;
        
        this.root[this.idField] = this.rootValue;
    }

    // establish default values for isFolderProperty et al that were not derived from the tree
    // relationship
    this.setupProperties();

    // For paged ResultTrees, use the same resultSize for every ResultSet of children.
    if (this.isPaged()) {
        // context.dataPageSize may be set if specified on a DataBoundComponent that created us.
        var context = this.context;
        this.resultSize = (
            context && context.dataPageSize != null ? context.dataPageSize : this.resultSize);

        this._childrenResultSetProperties = isc.addProperties({}, this._childrenResultSetProperties, {
            resultSize: this.resultSize
        });
    }

    // keepParentsOnFilter usually implies fetchMode:"local".
    if (this.keepParentsOnFilter && !(this.isPaged() || this.isLocal())) {
        this.fetchMode = "local";
    }

    if (this.initialData) {
        if ("parent" == this.modelType) this.data = this.initialData;
        else if ("children" == this.modelType) this.root = this.initialData;
    }

    // observe dataChanged on our dataSource
    var dataSource = isc.DataSource.getDataSource(this.dataSource);
    this.observe(dataSource, "dataChanged", "observer.dataSourceDataChanged(dsRequest,dsResponse);");

    // whether to invalidate our cache when an update occurs on one of our datasources.
    // Default is update the current cache in place.
    this.dropCacheOnUpdate = this.operation.dropCacheOnUpdate;
    
    // set up defaultIsFolder before invoking Tree.init
    // This is required in _linkNodes() to ensure LOD ResultTrees' child nodes show up as
    // openable folders.
    if (this.defaultIsFolder == null) this.defaultIsFolder = this.loadDataOnDemand;

    this.invokeSuper(isc.ResultTree, "init", a,b,c,d,e,f);

    // if we're not using folder-by-folder load on demand, all nodes should be initially marked loaded
    this.setDefaultLoadState(this.loadDataOnDemand ? isc.Tree.UNLOADED : isc.Tree.LOADED);
},


setupProperties : function () {
    this.invokeSuper(isc.ResultTree, "setupProperties");
    if (this.isPaged()) {
        // An auto-generated property name to store precomputed booleans for whether a node and
        // all of its visible descendants are all loaded.
        this._visibleDescendantsCachedProperty = ("_visibleDescendantsCached_" + this.ID);
    }
},


duplicate : function (includeData, includeLoadState) {
    if (!this.isPaged()) {
        return this.invokeSuper(isc.ResultTree, "duplicate", includeData, includeLoadState);
    }

    var serverFilterFields = this.serverFilterFields;
    if (isc.isAn.Array(serverFilterFields)) {
        serverFilterFields = serverFilterFields.duplicate();
    }

    
    var newResultTreeConfig = {
        fetchMode: this.fetchMode,
        dataSource: this.dataSource,
        loadDataOnDemand: this.loadDataOnDemand,
        childCountProperty: this.childCountProperty,
        defaultIsFolder: this.defaultIsFolder,
        discardParentlessNodes: this.discardParentlessNodes,
        defaultNewNodesToRoot: this.defaultNewNodesToRoot,
        updateCacheFromRequest: this.updateCacheFromRequest,
        disableCacheSync: this.disableCacheSync,
        keepParentsOnFilter: this.keepParentsOnFilter,
        serverFilterFields: serverFilterFields,
        canReturnOpenFolders: this.canReturnOpenFolders
    };
    this._copyKnownProperties(newResultTreeConfig);

    
    newResultTreeConfig.autoOpenRoot = false;

    var newResultTree = isc.ResultTree.create(newResultTreeConfig),
        root = this.getRoot(),
        rootIsOpen = this.isOpen(root),
        rootIsFolder = this.isFolder(root),
        rootCachedLength = root[this._cachedLengthProperty],
        rootRecursionCount = root[this._recursionCountProperty],
        rootAllCached = root[this._visibleDescendantsCachedProperty],
        newRoot = this.getCleanNodeData(root, false, false, includeLoadState);
    
    newRoot[newResultTree.openProperty] = rootIsOpen;
    newRoot[newResultTree.isFolderProperty] = rootIsFolder;
    newRoot[newResultTree._cachedLengthProperty] = rootCachedLength;
    newRoot[newResultTree._recursionCountProperty] = rootRecursionCount;
    newRoot[newResultTree._visibleDescendantsCachedProperty] = rootAllCached;

    this._duplicate(root, newResultTree, newRoot, includeLoadState);
    newResultTree.setRoot(newRoot);
    return newResultTree;
},


_getCleanNodeData : function (newTree, nodeList, includeLoadState) {
    var cachedLengthProperty = this._cachedLengthProperty,
        recursionCountProperty = this._recursionCountProperty,
        allCachedProperty = this._visibleDescendantsCachedProperty,

        newOpenProperty = newTree.openProperty,
        newIsFolderProperty = newTree.isFolderProperty,
        newCachedLengthProperty = newTree._cachedLengthProperty,
        newRecursionCountProperty = newTree._recursionCountProperty,
        newAllCachedProperty = newTree._visibleDescendantsCachedProperty;

    if (nodeList == null) {
        return null;
    } else if (isc.isAn.Array(nodeList)) {
        var newNodeList = new Array(nodeList.length);
        for (var i = nodeList.length; i--; ) {
            var oldNode = nodeList[i],
                newNode = null;
            if (oldNode != null) {
                
                var isOpen = this.isOpen(oldNode),
                    isFolder = this.isFolder(oldNode),
                    cachedLength = oldNode[cachedLengthProperty],
                    recursionCount = oldNode[recursionCountProperty],
                    allCached = oldNode[allCachedProperty];

                newNode = this.getCleanNodeData(oldNode, false, false, includeLoadState);

                newNode[newOpenProperty] = isOpen;
                newNode[newIsFolderProperty] = isFolder;
                newNode[newCachedLengthProperty] = cachedLength;
                newNode[newRecursionCountProperty] = recursionCount;
                newNode[newAllCachedProperty] = allCached;
            }
            newNodeList[i] = newNode;
        }
        return newNodeList;
    } else {
        var isOpen = this.isOpen(nodeList),
            isFolder = this.isFolder(nodeList),
            cachedLength = nodeList[cachedLengthProperty],
            recursionCount = nodeList[recursionCountProperty],
            allCached = nodeList[allCachedProperty],
            newNode = this.getCleanNodeData(nodeList, false, false, includeLoadState);

        newNode[newOpenProperty] = isOpen;
        newNode[newIsFolderProperty] = isFolder;
        newNode[newCachedLengthProperty] = cachedLength;
        newNode[newRecursionCountProperty] = recursionCount;
        newNode[newAllCachedProperty] = allCached;
        return newNode;
    }
},

_duplicate : function (node, newTree, newNode, includeLoadState) {
    

    // If the node is a leaf, return immediately since it's not going to have any children.
    if (this.isLeaf(node)) {
        return;
    }

    var children = this.getChildren(node),
        childrenLength = 0;

    if (isc.isA.ResultSet(children)) {
        childrenLength = children.getLength();
        newNode[newTree.childCountProperty] = childrenLength;
        var newChildren = newNode[newTree.childrenProperty] = new Array(childrenLength);
        for (var i = childrenLength; i--; ) {
            newChildren[i] = children.getCachedRow(i);
        }
    } else if (isc.isAn.Array(children)) {
        childrenLength = children.length;
        newNode[newTree.childrenProperty] = children;
    } else if (children) {
        childrenLength = 1;
        newNode[newTree.childrenProperty] = [children];
    }

    // Iterate through all the children of the node to put clean copies of the children under
    // the node of the new tree.
    var modelTypeParent = (this.modelType == isc.Tree.PARENT),
        parentIdField = this.parentIdField,
        newChildren = newNode[newTree.childrenProperty] = this._getCleanNodeData(
            newTree, newNode[newTree.childrenProperty], includeLoadState);
    for (var i = 0; i < childrenLength; ++i) {
        var child = children.getCachedRow(i);
        if (child == null) {
            continue;
        }
        if (modelTypeParent) {
            newChildren[i][parentIdField] = child[parentIdField];
        }

        // If the child is a folder, recurse, but check that it actually has children.
        var grandChildren = child[this.childrenProperty];
        if (grandChildren && !grandChildren.isEmpty()) {
            // Now duplicate the descendants of the child.
            this._duplicate(child, newTree, newChildren[i], includeLoadState);
        }
    }
},


destroy : function () {
    if (this.isPaged()) {
        this._cleanResultSetChildren(this.getRoot(), false);
    }

    var dataSource = isc.DataSource.getDataSource(this.dataSource);
    if (dataSource) this.ignore(dataSource, "dataChanged");
    this.Super("destroy", arguments);
},

isLocal : function () { return this.fetchMode == "local" },
isPaged : function () { return this.fetchMode == "paged" },
haveCriteria : function (criteria) {
    return !(
        criteria == null ||
        isc.isAn.emptyObject(criteria) ||
        // `isc.DataSource.convertCriteria({})` is considered empty as well:
        (criteria._constructor === "AdvancedCriteria" &&
            criteria.operator === "and" &&
            isc.isAn.Array(criteria.criteria) &&
            criteria.criteria.length == 0));
},

// This is necessary to support higher-level callback processing like for DBC.filterData.
setContext : function (context) {
    this.context = context;

    // Update the context on any ResultSet children.
    if (this.isPaged() && this._resultSetChildren != null) {
        for (var i = this._resultSetChildren.length; i--; ) {
            var newContext = context && isc.addProperties({}, context);
            if (context) {
                delete newContext.clientContext;
                delete newContext.internalClientContext;
            }
            this._resultSetChildren[i].setContext(newContext);
        }
    }
},

// A Tree navigates a 1 to many (parent to children) relationship, which can exist within or 
// across DataSources.

// figuring out the type of child records at each level of the tree
// - use cases
//   - all one type
//     - supported: set just this.dataSource
//   - fixed levels
//     - example: salesOrder, lineItem
//     - supported: set this.dataSource for root DataSource, this.treeRelations for transitions
//   - mixed child types (each parent in a level has different child types)
//     - example: outlook left-hand tree navigation: top level is a random conglomeration of Inbox,
//       Favorites, etc, each with different child node types (message folders, filesystem folders,
//       etc)
//     - supported: next level is specified via node[this.childNodeType], or via overriding
//       getChildDataSource
//   - mixed type within a level
//     - supported: the Tree just needs a DataSource with a primary key for the level.  Any join
//       that can produce this is fine.

getTreeRelationship : function (parentNode) {
    var childDS = this.getChildDataSource(parentNode);
    // ask the datasource for a tree relationship, which can be declared explicitly or
    // autodetected from field declarations
    var relationship = childDS.getTreeRelationship();
    return relationship;

    
},

//> @method resultTree.getChildDataSource()
// Get the DataSource for children under this node.
//
// If this node has no appropriate child node type, this method will return null - in a multi-DS
// tree this indicates that there is no appropriate next DataSource to navigate to, and this node
// will be a leaf.
//<
// NOTE: nodeDS is an optional parameter, used when we need to know the child datasource of a node
// before it gets linked into the tree (at that time, the node's DS can't be determined by looking
// at it's future parent).
getChildDataSource : function (node, nodeDS) {
    // look for explicitly specified child type 
    var childDSName = node[this.childTypeProperty];
    if (childDSName != null) return isc.DS.get(childDSName);

    // see if there is a mapping from this parent's type to its child type
    var nodeDS = nodeDS || this.getNodeDataSource(node);

    // - if this is a single DS tree, use the one and only DataSource
    // - if we're at root (which is the only node with no DS), use the root DataSource
    if (nodeDS == null || !this.isMultiDSTree()) return this.getRootDataSource();

    // otherwise try to find a relation from this node's DS to some other DS

    // see if there's an explicitly declared tree relation
    var treeRelations = this.treeRelations,
        childDataSources = nodeDS.getChildDataSources();

    //this.logWarn("getChildDataSource: nodeDS is : " + nodeDS + 
    //             ", treeRelations: " + this.echo(treeRelations) + 
    //             ", childDataSources: " + this.echo(childDataSources));

    if (treeRelations) {
        childDSName = treeRelations[nodeDS.ID];
        if (childDSName != null) return isc.DS.get(childDSName);
    }
    // otherwise take the first relationship to any other DataSource
    if (childDataSources != null) return childDataSources[0];
},

// get the DataSource for this node
getNodeDataSource : function (node) {
    // check for explicitly specified type (this allows mixed types within a set of children)
    var dsName = node[this.nodeTypeProperty];

    // use the type stored on parent node when this child was fetched
    if (dsName == null) {
        var parentNode = this.getParent(node);
        if (parentNode == null) {
            // the special, singular "root" object has no DataSource 
            return null; 
        } else if (parentNode == this.root) {
            // nodes under root are of the first or "root" DataSource (slightly confusing)
            dsName = this.getRootDataSource().ID;
        } else {
            // when we have a mixture of node types, and the parent stores the type of the
            // child nodes when they are loaded
            dsName = parentNode._derivedChildNodeType;
            // otherwise we have just one node type
            if (dsName == null) dsName = this.getRootDataSource().ID;
        }
    }
    return isc.DS.get(dsName) || this.getRootDataSource();
},

isMultiDSTree : function () {
    return this.multiDSTree || this.treeRelations != null;
},

// get the DataSource for the nodes that appear at root
getRootDataSource : function () {
    if (this.operation && this.operation.dataSource) return isc.DS.get(this.operation.dataSource);
    else return  isc.DS.get(this.dataSource);
},

// get the criteria to apply (aside from parentId) when selecting children from childDS
getCriteria : function (childDS, parentDS, parentNode) {
    if (this.getRootDataSource() == childDS) return this.criteria;
    return null;
},

// get an operationId to use to select children from childDS.   operation can optionally depend
// on parentDS and parentNode
getOperationId : function (childDS, parentDS, parentNode) {
    // FIXME we may want a declarative way to specify the operation to use to select on each
    // DataSource the tree may encounter
    return this.operation ? this.operation.ID : null;
},

//>	@method resultTree.loadChildren()
// @include tree.loadChildren()
//<

//>	@method resultTree.unloadChildren()
// @include tree.unloadChildren()
//<

_getRelationship : function (parentNode, debugLog) {

    // figure out what parent-child relationship will be used to select children of this node.  
    var isRoot = (parentNode == null || parentNode == this.root),
        relationship;

//    if (debugLog) {
//        this.logWarn(
//            "parentNode: " + this.echo(parentNode) + ", isRoot: " + isRoot);
//    }

    // if we're at root, and this is a multi-DataSource tree, the root-level nodes have no parent
    // dataSource.  We just do a normal select, using only the criteria
    var childDS, parentDS;
    if (isRoot && this.isMultiDSTree()) {
        childDS = this.getRootDataSource();
        parentDS = null;
        // XXX this isn't really a relationship: the singular "root" has no schema, hence there is
        // no "parentDS" or "idField", and in the childDS there is no parentIdField that points to
        // root.  But the notion of "childDS", the DataSource of the nodes being loaded, is still
        // valid.
        relationship = { childDS:childDS }; 
    } else {    
        // otherwise, we detect some relationship that this node has either within its own
        // DataSource or across DataSources, and load children using that relationship
        relationship = this.getTreeRelationship(parentNode);
        childDS = relationship.childDS;
        parentDS = relationship.parentDS;
    }

    if (!this.isMultiDSTree()) {
        // force local overrides of idField, parentIdField and rootValue on the relationship -
        // these are autodetected and initialized in init() if unset on this ResultTree.
        relationship.idField = this.idField;
        relationship.parentIdField = this.parentIdField;
        relationship.rootValue = relationship.rootValue;
    }
    if (debugLog && this.logIsDebugEnabled()) {
        this.logDebug("parent id: " + (isRoot ? "[root]" : parentNode[relationship.idField]) + 
                     " (type: " + (isRoot ? "[root]" : (parentDS ? parentDS.ID : "null")) + ")" +
                     " has childDS: " + childDS.ID +
                     ", relationship: " + this.echo(relationship));
    }
    return relationship;
},


_getLoadChildrenCriteria : function (parentNode, relationship, debugLog) {

    // put together criteria that should always be used when selecting against this DataSource
    var isRoot = (parentNode == null || parentNode == this.root),
        childDS = relationship.childDS,
        parentDS = relationship.parentDS,
        criteria = {};

    if (!this.isLocal()) {
        // no local filtering - send all criteria to the server
        criteria = isc.addProperties({}, this.getCriteria(childDS, parentDS, parentNode));
    } else if (this._serverCriteria != null) {
        criteria = isc.addProperties({}, this._serverCriteria);
    }

    if (isRoot && this.isMultiDSTree()) {
        // leave criteria alone
    } else if (this.loadDataOnDemand || this.isPaged()) {
        // loadOnDemand: instead of loading the whole tree, only load the children of a single
        // node.  Put together criteria that will find all records from the childDS that belong
        // under this parent record (eg lineItems in a salesOrder)
        
        var parentIdFieldValue = parentNode[relationship.idField];
        // Note: If we're loading the children of the root node, default to the
        // rootValue as specified at the dataSource level if no rootValue was specified directly
        // on the tree
        var undefined;
        if (isRoot && parentIdFieldValue === undefined) {
            parentIdFieldValue = relationship.rootValue;
        }   
        if (criteria._constructor == "AdvancedCriteria") {
            criteria = isc.DataSource.combineCriteria(
                criteria, 
                {_constructor:"AdvancedCriteria", 
                 fieldName:relationship.parentIdField, 
                 value:parentIdFieldValue, operator:"equals"},
                "and"
            );
        } else {
            criteria[relationship.parentIdField] = parentIdFieldValue;
        }
        //if (debugLog) {
        //    this.logWarn("criteria is: " + isc.JSON.encode(criteria));
        //}
    }
    return criteria;
},

// Note this is an internal method to fetch the children and fold them into the children array
// for the node in question. It doesn't check for the children already being loaded - so if
// called repeatedly you'd end up with duplicates in the children array.
_loadChildren : function (parentNode, start, end, callback) {

    var relationship = this._getRelationship(parentNode, true),
        childDS = relationship.childDS,
        parentDS = relationship.parentDS;

    // remember the type of children under this parent, because we'll use this to figure out the
    // type of the children's children.
    parentNode._derivedChildNodeType = childDS.ID;

    var isRoot = (parentNode == null || parentNode == this.root),
        criteria = this._getLoadChildrenCriteria(parentNode, relationship, true);
    if (!((isRoot && this.isMultiDSTree()) || this.loadDataOnDemand)) {
        // we're going to fetch the entire tree in one go, so mark everything as loaded
        this.setDefaultLoadState(isc.Tree.LOADED);
    }

    // Remember the parentNode whose children we are loading, and what relationship we used
    // also set up the callback to fire on return.    
    
    var internalClientContext = {
        parentNode: parentNode,
        relationship: relationship,
        childrenReplyCallback: callback
    };

    // If this is the initial fetch, hang a flag on the internalClientContext so we know to
    // fire the initial fetch callback.
    
    if (!this._performedInitialFetch) {
        internalClientContext._isInitialFetch = true;
        this._performedInitialFetch = true;
    }
    
    // Hang onto a unique fetch "id" so if invalidateCache is called before the fetch
    // returns we know the results are essentially invalid.
    internalClientContext.fetchCount = ++this.currentFetch;

    var requestProperties = isc.addProperties({
        parentNode: parentNode,
        resultTree: this
    }, this.context);
    if (this.context && this.context.internalClientContext) {
        internalClientContext = isc.addProperties(
            {}, this.context.internalClientContext, internalClientContext);
    }
    requestProperties.internalClientContext = internalClientContext;

    // get an operation to do a select against the child DS
    var operationId = this.getOperationId(childDS, parentDS, parentNode);
    if (operationId) requestProperties.operationId = operationId;

    // set willHandleErrors to true so we can clear up our loading prompt on a server error    
    requestProperties.willHandleError = true;

    // In `fetchMode: "paged"`, a startRow and endRow are specified with the request.  If
    // keepParentsOnFilter is enabled, then the same-named flag is also set on all DSRequests
    // issued from the tree.
    if (this.isPaged()) {
        requestProperties.startRow = start;
        requestProperties.endRow = end;

        if (this.keepParentsOnFilter) {
            requestProperties.keepParentsOnFilter = true;
        }
    }

    // set the parent as loading
    if (parentNode != null) this.setLoadState(parentNode, isc.Tree.LOADING);

    
    var progressiveLoading = this._getProgressiveLoading(),
        progressiveLoadingProperties = null;
    if (progressiveLoading !== false) {
        var parentOfLastNode = true;
        for (var n = parentNode, p = null; parentOfLastNode && n != null; n = p) {
            p = this.getParent(n);
            if (p == null) {
                parentOfLastNode = true;
            } else {
                var c = this.getChildren(p);
                parentOfLastNode = (c.getLength() == 1 + c.indexOf(n));
            }
        }

        progressiveLoading = parentOfLastNode && progressiveLoading;
        if (progressiveLoading === true || progressiveLoading === false) {
            progressiveLoadingProperties = { progressiveLoading: progressiveLoading };
        }
    } else {
        progressiveLoadingProperties = { progressiveLoading: false };
    }

    // kick off the operation to fetch children
    childDS.fetchData(criteria, { caller: this, methodName: "loadChildrenReply" },
        isc.addProperties(requestProperties, progressiveLoadingProperties));
},
currentFetch:0,

loadChildrenReply : function (dsResponse, data, request) {
    var context = dsResponse.internalClientContext;
    
    // If 'invalidateCache' was called while a fetch was in operation, ignore the
    // response.
    
    var fetchCount = context.fetchCount;
    if (this.invalidatedFetchCount != null && fetchCount <= this.invalidatedFetchCount) {
        return;
    }

    var parentNode = context.parentNode;
    
    var ancestor = parentNode,
        greatAncestor;
    while ((greatAncestor = this.getParent(ancestor)) != null) {
        ancestor = greatAncestor;
    }
    if (ancestor !== this.getRoot()) {
        return;
    }

    // Are we filtering data locally?
    var localFiltering = (this.isLocal() && this.haveCriteria(this._localFilter || this.criteria));

    // incorporate the new records into the tree
    var relationship = context.relationship,
        newNodes = dsResponse.data;

    // if we're returned an error handle it as if we were returned no data, then
    // call the standard RPCManager error handling code
    if (dsResponse.status < 0) newNodes = null;

    // if we're returned the STATUS_OFFLINE condition, handle it as an empty dataset
    if (dsResponse.status == isc.RPCResponse.STATUS_OFFLINE) {
        newNodes = [];
        if (parentNode != null && !this.isRoot(parentNode)) {
            isc.say(window[request.componentId].offlineNodeMessage);
        }
    }

    // Determine target tree for this new data.
    // Re-target our parentNode if using a different tree.
    var tree = this;
    if (localFiltering) {
        // Link new data into the complete tree
        if (!this.completeTree) this.completeTree = this.duplicate(true, true);
        tree = this.completeTree;

        var parentPath = this.getPath(parentNode);
        parentNode = tree.find(parentPath);
    }

    if (newNodes == null || newNodes.length == 0) {
        // no new nodes, mark parentNode as loaded
        if (dsResponse.status == isc.RPCResponse.STATUS_OFFLINE) {
            tree.setLoadState(parentNode, isc.Tree.UNLOADED);
            tree.delayCall("closeFolder", [parentNode], 0);
        } else {
            tree.setLoadState(parentNode, isc.Tree.LOADED);
        }

        if (newNodes == null) {
            if (dsResponse.status < 0) {
                isc.RPCManager._handleError(dsResponse, request);
            } else {
                this.logWarn("null children returned; return empty List instead");
            }
            newNodes = [];
        }
    }

    if (this.isPaged()) {
        var numResults = newNodes.length;

        // If the server did not specify startRow then assume that the startRow is what was asked for.
        var startRow = (dsResponse.startRow != null ? dsResponse.startRow : request.startRow);

        // If the server did not specify endRow then assume that the endRow is startRow plus the
        // number of records returned.
        var endRow = (dsResponse.endRow != null ? dsResponse.endRow : startRow + numResults);

        // If the server did not specify totalRows but the resulting endRow is less than what we
        // asked for then we know that the server does not have more rows, so set totalRows to endRow.
        var totalRows = (
                dsResponse.totalRows == null && endRow < request.endRow ?
                    endRow : dsResponse.totalRows);

        var children = parentNode[this.childrenProperty];
        if (isc.isA.ResultSet(children)) {
            
        } else if (endRow < totalRows) {
            parentNode[this.childCountProperty] = totalRows;
            var grandParent = this.getParent(parentNode),
                origParentLength = grandParent != null && this._getNodeLengthToParent(parentNode, grandParent);
            children = this._canonicalizeChildren(
                parentNode, parentNode[this.childrenProperty], false);
            

            children.fillCacheData(newNodes, startRow);
        } else {
            if (children != null) {
                var existingNodes = children.getRange(startRow, endRow);
                for (var i = existingNodes.length; i--; ) {
                    this._remove(existingNodes[i]);
                }
            }
            this._addList(newNodes, parentNode, startRow);
        }
    } else if (tree.isMultiDSTree()) {
        for (var i = 0; i < newNodes.length; i++) {
            var node = newNodes[i];
            // in a multi-DS tree, a node is a folder if there's a childDS to fetch nodes from
            var nextChildDS = this.getChildDataSource(node, relationship.childDS);
            if (nextChildDS != null) this.convertToFolder(node);

            // Node naming:
            // - in a single-DS tree, all nodes have an id that is unique tree-wide, the "idField" from
            //   the tree relationship
            // - in a multi-DS tree, nodes are from a mix of DataSources and do not necessarily have a
            //   tree-wide unique id - they only have a unique id within each set of children, since
            //   each set of children can be from a different DataSource (even when on the same level).
            // 
            // So, for multiDSTrees, in this case all newNodes are immediate children
            //
            // link it in
            tree._add(node, parentNode);
        }
    } else {
        // we're dealing with a mixed bag of parents and children, any number of levels deep.  In
        // this case we assume a unique id across all tree nodes, as opposed to just one level, and
        // run a linking algorithm that can handle the nodes in any order.
        
        if (dsResponse.status == isc.RPCResponse.STATUS_OFFLINE) {
            tree.setLoadState(parentNode, isc.Tree.UNLOADED);
            tree.delayCall("closeFolder", [parentNode], 0);
        } else {
            // If we are filtering locally, postpone dataChanged event until we finish filtering.
            var suppressDataChanged = localFiltering;
            tree._linkNodes(newNodes,
                           relationship.idField,
                           relationship.parentIdField,
                           relationship.rootValue,
                           relationship.isFolderProperty,
                           parentNode,
                           suppressDataChanged);
        }
    }
    // If filtering locally, do it now.
    if (localFiltering) {
        // If we didn't set up the "openStateForLoad" flag, do it now.
        if (this._openStateForLoad == null) this._openStateForLoad = this._getOpenState();
        this.filterLocalData();
    }
    // Reopen any nodes after cache is filled
    if (this._openStateForLoad) {
        this._setOpenState(this._openStateForLoad, true);
        delete this._openStateForLoad;
    }

    // Fire any callback passed to 'loadChildren' in the scope of this tree.
    if (context.childrenReplyCallback) {
        this.fireCallback(context.childrenReplyCallback, "node", [parentNode], this);
    }
    
    // NOTE: when paging within child sets is implemented, we'll add "startChild,endChild" to
    // this signature
    if (this.dataArrived != null) {
        isc.Func.replaceWithMethod(this, "dataArrived", "parentNode");
        this.dataArrived(parentNode);
    }
},


// Cache sync
// ------------------------------------
// On initial load of data for some folder, we always retrieve the entire set of children for the
// parents of the node.
// When dataChanged fires on our dataSource, we need to update these stored children arrays to
// incorporate the modified nodes into our tree of local data.

// helper method to get this.dataSource as a datasource object (even if specified as an ID only)
getDataSource : function () {
    return isc.DataSource.getDataSource(this.dataSource);
},

//> @method resultTree.invalidateCache() [A]
// Manually invalidate this ResultTree's cache.
// <P>
// Generally a ResultTree will observe and incorporate updates to the DataSource that provides its
// records, but when this is not possible, <code>invalidateCache()</code> allows manual cache
// invalidation.
// <P>
// Components bound to this ResultTree will typically re-request the currently visible portion
// of the dataset, causing the ResultTree to re-fetch data from the server.
// @visibility external
//<
invalidateCache : function () {
    if (!this.isLoaded(this.root)) return;
    
    // Ensure that if a current fetch is in progress, we ignore its response in favor
    // of the new results.
    this.invalidatedFetchCount = this.currentFetch;
    
    // Save current open state so it can be reapplied when new data arrives
    this._openStateForLoad = this._getOpenState(true);
    // reset autoName to zero.
    
    this._autoName = 0;

    // Reset root to refetch all our data.
    this.setRoot(this.makeRoot(), true);

    
    if (!this.loadDataOnDemand) {
        this.reloadChildren(this.root);
    }
},

dataSourceDataChanged : function (dsRequest, dsResponse) {

    // respsect the flag to suppress cache sync altogether
    if (this.disableCacheSync) return;
    
    var updateData = isc.DataSource.getUpdatedData(dsRequest, dsResponse, 
                                                   this.updateCacheFromRequest);
    
    this.handleUpdate(dsRequest.operationType, updateData, dsResponse.invalidateCache);
},

handleUpdate : function (operationType, updateData, forceCacheInvalidation) {
    if (isc._traceMarkers) arguments.__this = this;

    var dropCacheOnUpdate = (
            this.dropCacheOnUpdate || forceCacheInvalidation ||
            
            (this.isPaged() && this.keepParentsOnFilter));
    if (dropCacheOnUpdate) {
        

        this.invalidateCache();

        
        if (!this.getDataSource().canQueueRequests) this.dataChanged();
        return;
    }

    // update our cached tree directly  Note our cache is filtered, so we may just discard the
    // update if the new row doesn't pass the filter
    this.updateCache(operationType, updateData);
    this.dataChanged();
},


// updateCache() - catch-all method fired when the dataSource dataChanged method fires.
// Integrates (or removes) the modified nodes into our local tree of data.
updateCache : function (operationType, updateData) {
    if (updateData == null) return;
    

    operationType = isc.DS._getStandardOperationType(operationType);

	if (!isc.isAn.Array(updateData)) updateData = [updateData];

	//>DEBUG
    if (this.logIsInfoEnabled()) {
        this.logInfo("Updating cache: operationType '" + operationType + "', " + 
                     updateData.length + " rows update data" +
                     (this.logIsDebugEnabled() ? 
                      ":\n" + this.echoAll(updateData) : ""));
    } //<DEBUG

	switch (operationType) {
	case "remove":
        this.removeCacheData(updateData);
		break;
	case "add":
        this.addCacheData(updateData);
		break;
	case "replace":
	case "update":
        this.updateCacheData(updateData);
		break;
	}

}, 

addCacheData : function (updateData) {
	if (!isc.isAn.Array(updateData)) updateData = [updateData];

    // Don't add rows that don't pass filtering
    var validRows = this.getDataSource().applyFilter(updateData, this.criteria, this.context);

    this.logInfo("Adding rows to cache: " + validRows.length + " of " + updateData.length + 
                 " rows match filter criteria");

    var checkParent = (this.idField != undefined && this.parentIdField != undefined),
        pk = this.getDataSource().getPrimaryKeyFieldNames()[0];

    

    for (var i = 0; i < validRows.length; i++) {
        var addRow = validRows[i];

        
        if (checkParent &&
            addRow != null &&
            addRow[this.idField] == addRow[this.parentIdField])
        {
            this.logWarn(
                "Invalid attempt to add a node that is specified to be its own parent " +
                "(the '" + this.idField + "' + and '" + this.parentIdField + "' properties " +
                "of the node are both set to " + (addRow[this.idField] == null ? "null" :
                addRow[this.idField].toString()) + ").  Skipping this node.");
            continue;
        }

        // Update cache of the entire tree (all nodes)
        if (this.completeTree) {
            this._addNodeToCache(this.completeTree, addRow, pk);
        }
        // Update the visible tree
        this._addNodeToCache(this, addRow, pk);
    }
},

_addNodeToCache : function (tree, node, pk) {
    

    var parentId = node[this.parentIdField], parentNode;
    
    if (parentId != null) parentNode = tree.find(pk, parentId);
    else {
        if (this.defaultNewNodesToRoot || tree.rootValue == null) parentNode = tree.getRoot();
        else parentNode = null;
    }
    
    // Duplicate the node when adding it -- this is required to avoid us writing 
    // properties onto the object directly
    // Note: _add() will automatically sort the new node in the children array
    var addNode = (
            parentNode != null &&
            (tree.getLoadState(parentNode) == isc.Tree.LOADED) &&
            
            !(this.isPaged() && isc.isA.ResultSet(this.getChildren(parentNode))));
    if (addNode) {
        node = isc.clone(node);
        tree._add(node, parentNode);
    }
    return addNode;
},

updateCacheData : function (updateData) {
	if (!isc.isAn.Array(updateData)) updateData = [updateData];

    //>DEBUG
    var debugTotals = {
        addedRows: 0,
        updatedRows: 0,
        removedRows: 0};
    //<DEBUG

    // Are we filtering data locally?
    var criteria = (this._localCriteria || this.criteria),
        haveCriteria = this.haveCriteria(criteria);
    

    var checkParent = (this.idField != undefined && this.parentIdField != undefined),
        ds = this.getDataSource();

    for (var i = 0; i < updateData.length; i++) {
        var updateRow = updateData[i];
        var matchesFilter = true;
        if (haveCriteria) {
            var matches = ds.applyFilter([updateRow], criteria, this.context);
            matchesFilter = (matches != null && matches.length > 0);
        }
        //>DEBUG
        if (this.logIsDebugEnabled() && !matchesFilter) {
            this.logDebug("updated node :\n" + this.echo(updateRow) +
                         "\ndidn't match filter: " + this.echo(criteria));
        }
        //<DEBUG

        if (updateRow == null) {
            continue;
        }

        
        if (checkParent &&
            updateRow[this.idField] == updateRow[this.parentIdField])
        {
            this.logWarn(
                "Invalid attempt to update a node where the '" + this.idField + "' and '" +
                this.parentIdField + "' properties of the record are both set to " +
                (updateRow[this.idField] == null ? "null" : updateRow[this.idField].toString()) +
                " (this would reparent the node to itself).  Skipping this node.");
            continue;
        }

        // Update cache of the entire tree (all nodes)
        if (this.completeTree) {
            this._updateNodeInCache(this.completeTree, updateRow, true, null);
        }
        // Update the visible tree
        this._updateNodeInCache(this, updateRow, matchesFilter
        //>DEBUG
        , debugTotals
        //<DEBUG
        );
    }

    //>DEBUG
    if (this.logIsDebugEnabled()) {
        this.logDebug("updated cache: "
             + debugTotals.addedRows + " row(s) added, "
             + debugTotals.updatedRows + " row(s) updated, "
             + debugTotals.removedRows + " row(s) removed.");            
    }
    //<DEBUG
},

_updateNodeInCache : function (tree, updateRow, matchesFilter, debugTotals) {
    
    var ds = this.getDataSource(),
        pk = ds.getPrimaryKeyFieldNames()[0],
        node = tree.find(pk, updateRow[pk]);

    // Very likely we'll see null nodes - we probably haven't opened their parent folder yet
    // However - check for the case where we have and if so, add to our data-set
    if (node == null) {
        if (matchesFilter) {
            if (this._addNodeToCache(tree, updateRow, pk)) {
                // This situation is valid - a developer updated a child of a parent we haven't
                // loaded (possibly in another tree on the page) and shifted it into a
                // parent we have loaded
                this.logInfo("updated row returned by server doesn't match any cached row, " +
                             " adding as new row.  Primary key value: " + this.echo(updateRow[pk]) +
                             ", complete row: " + this.echo(updateRow));
                if (debugTotals) debugTotals.addedRows++;
            }
        }
        return;
    }

    
    var paged = this.isPaged(),
        prevSiblings = paged && this.getChildren(this.getParent(node));
    if (matchesFilter) {
        // the change may have reparented a node.
        if (updateRow[this.parentIdField] != node[this.parentIdField]) {
            var newParentNode = tree.find(pk, updateRow[this.parentIdField]);
            if (newParentNode == null && 
                (this.defaultNewNodesToRoot || this.rootValue == null))
            {
                newParentNode = tree.getRoot();
            }

            if (newParentNode == null || (tree.getLoadState(newParentNode) != isc.Tree.LOADED)) {
                if (!(paged && isc.isA.ResultSet(prevSiblings))) {
                    tree._remove(node);
                    if (debugTotals) debugTotals.removedRows++;
                }
                return;
            } else {
                var newSiblings = paged && this.getChildren(newParentNode),
                    add = !(paged && isc.isA.ResultSet(newSiblings)),
                    remove = !(paged && isc.isA.ResultSet(prevSiblings));
                if (add && remove) {
                    tree._move(node, newParentNode);
                } else if (add) {
                    tree._add(node, newParentNode);
                } else if (remove) {
                    tree._remove(node);
                }
            }
        }
        // apply all modified fields to the node.
        isc.addProperties(node, updateRow);

        
        var fieldNames = ds.getFieldNames(); 
        for (var i = 0; i < fieldNames.length; i++) {
            var name = fieldNames[i];
            if (!updateRow.hasOwnProperty(name)) delete node[name];
        }
        if (debugTotals) debugTotals.updatedRows++;
    } else if (!(paged && isc.isA.ResultSet(prevSiblings))) {
        tree._remove(node);
        if (debugTotals) debugTotals.removedRows++;
    }
},

removeCacheData : function (updateData) {
    if (!isc.isAn.Array(updateData)) updateData = [updateData];

    var pk = this.getDataSource().getPrimaryKeyFieldNames()[0];

    // Update cache of the entire tree (all nodes)
    
    if (this.completeTree) {
        this._removeNodesFromCache(this.completeTree, updateData, pk);
    }
    // Update the visible tree
    this._removeNodesFromCache(this, updateData, pk);
},

_removeNodesFromCache : function (tree, updateData, pk) {
    

    // Build list of nodes to be removed
    var paged = this.isPaged(),
        nodes = [];
    for (var i = 0; i < updateData.length; i++) {
        var node = tree.find(pk, updateData[i][pk]);
        if (node == null) {
            this.logWarn("Cache synch: couldn't find deleted node:" + this.echo(updateData[i]));
        } else if (!(paged && isc.isA.ResultSet(this.getChildren(this.getParent(node))))) {
            nodes.add(node);
        }
    }
    tree._removeList(nodes);
},

// get the title for this node
getTitle : function (node) {
    // look up the node's DataSource and return its title field
    var dataSource = this.getNodeDataSource(node);

    // the special, singular root node has no DataSource
    if (!dataSource) return "root";

    var title = node[dataSource.getTitleField()];
    if (title != null) return title;

    // if there's no title on this node, try not to leave a blank
    return this.Super("getTitle", arguments);
},

// indexOf: As with ResultSets support being passed primaryKey values only as well as pointers 
// to nodes
// Note: This will return the index wrt the visible (open) nodes of the tree. If the node is not
// currently visible, -1 will be returned.
indexOf : function (node, a,b,c,d) {
    var pks = this.getDataSource().getPrimaryKeyFieldNames();
    for (var i = 0; i < pks.length; i++) {
        var pk = pks[i];
        if (node[pk] != null) return this.findIndex(pk, node[pk]);
    }
    
    return this.invokeSuper(isc.ResultTree, "indexOf", node, a,b,c,d);
},


contains : function (node, pos, comparator) {
    if (this.isPaged() && node != null && pos === undefined && comparator === undefined) {
        var idProperty = this.idField;
        return (node[this.treeProperty] == this.ID && this.nodeIndex[node[idProperty]] == node);
    } else {
        return this.invokeSuper(isc.ResultTree, "contains", node, pos, comparator);
    }
},




getLength : function () {
    var length = this.invokeSuper(isc.ResultTree, "getLength");
    if (this.isPaged() && this._resultSetChildren != null) {
        var root = this.getRoot(),
            defaultChildLength = (
                this.openDisplayNodeType == isc.Tree.FOLDERS_AND_LEAVES ? 1 : 0);

        for (var i = this._resultSetChildren.length; i--; ) {
            var children = this._resultSetChildren[i],
                parent = children._parentNode,
                openSubfoldersAllowed = (
                    parent[this.canReturnOpenSubfoldersProperty] != null ?
                    parent[this.canReturnOpenSubfoldersProperty] : this.canReturnOpenFolders),
                knownLengthNulls = !(openSubfoldersAllowed || defaultChildLength == 0);
            if (!knownLengthNulls) {
                var visible = true;
                for (var p = parent; visible && p != root; p = this.getParent(p)) {
                    visible = p != null && this.isOpen(p);
                }
                if (visible) {
                    length += Math.max(
                        0, children._getCachedLength() - children._getCachedRows());
                }
            }
        }
    }
    return length;
},

//> @method resultTree.get()
// @include tree.get()
// @visibility external
//<
_$preallocatedRangeForGet: new Array(1),
get : function (pos) {
    if (pos < 0) {
        return undefined;
    } else {
        return this.getRange(pos, pos + 1, false, this._$preallocatedRangeForGet)[0];
    }
},

// Override Tree.getCachedRow() to get a record at the given position without causing a fetch
// if the record has not yet been loaded.  See ResultSet.getCachedRow().
getCachedRow : function (pos) {
    if (pos < 0) {
        return null;
    } else {
        var record = this.getRange(pos, pos + 1, true, this._$preallocatedRangeForGet)[0];
        if (record != null && record !== isc.ResultTree.getLoadingMarker()) {
            return record;
        } else {
            return null;
        }
    }
},

//> @method resultTree.getRange()
// @include tree.getRange()
// @visibility external
//<
_$getRangeInfoObj: {},
getRange : function (start, end, dontFetch, preallocatedRange) {
    
    if (start == null) {
        this.logWarn("getRange() called with no specified range - ignoring.");
        return;
    } else if (end == null) {
        end = start + 1;
    }
    if (start < 0 || end < 0) {
        //>DEBUG
        this.logWarn(
            "getRange(" + start + ", " + end + "): negative indices not supported, clamping " +
            "start to 0");
        //<DEBUG
        start = Math.max(start, 0);
    }
    if (end <= start) {
        //>DEBUG
        this.logDebug("getRange(" + start + ", " + end + "): returning empty list");
        //<DEBUG
        return [];
    }

    
    
    var cachedRanges = this._cachedRanges;
    if (cachedRanges != null && cachedRanges.length > 0) {
        var cachedRange = cachedRanges[cachedRanges.length - 1],
            start0 = cachedRange.start;
        if (start0 <= start && end <= cachedRange.end) {
            return cachedRange.range.slice(start - start0, end - start0);
        }
    }

    if (!this.isPaged()) {
        return this.invokeSuper(isc.ResultTree, "getRange", start, end);
    }

    var loadingMarker = isc.ResultTree.getLoadingMarker(),
        cachedLengthProperty = this._cachedLengthProperty,
        root = this.getRoot(),
        info = this._$getRangeInfoObj;
    info.range = preallocatedRange || [];
    info.rangeLength = 0;
    info.rangeLoading = false;
    info.needQueue = false;
    info.wasAlreadyQueuing = false;
    info.dontFetch = (dontFetch === true);

    if (end > start) {
        
        if (!(this.openDisplayNodeType != isc.Tree.LEAVES_ONLY && this.showRoot)) {
            // The root is not visible in the open list.  The length of the root includes 1
            // for the root, so shift the requested range by 1 to implement hiding of the root.
            ++start;
            ++end;
        }
        var progressiveLoading = this._getProgressiveLoading();
        this._getRange(root, root, [root], 0, start, end, true, progressiveLoading, false, info);
        
        if (preallocatedRange != null) {
            info.range.length = info.rangeLength;
        }
    }
    return info.range;
},

_pushCachedRange : function (start, end) {
    
    if (this.isPaged()) {
        if (this._cachedRanges == null) {
            this._cachedRanges = [];
        }
        this._cachedRanges.push({
            start: start,
            end: end,
            range: this.getRange(start, end)
        });
    }
},

_popCachedRange : function (start, end) {
    if (this.isPaged()) {
        
        this._cachedRanges.pop();
        if (this._cachedRanges.length == 0) {
            delete this._cachedRanges;
        }
    }
},


_getRange : function (root, node, children, i, start, end, recursionTopLevel, progressiveLoading, subrangeLoading, info) {
    
    var separateFolders = this.separateFolders,
        foldersBeforeLeaves = separateFolders && this.sortFoldersBeforeLeaves,
        leavesBeforeFolders = separateFolders && !this.sortFoldersBeforeLeaves,
        range = info.range,
        rangeLoading = info.rangeLoading,
        needQueue = info.needQueue,
        dontFetch = info.dontFetch;
    

    
    var defaultChildLength = (this.openDisplayNodeType == isc.Tree.FOLDERS_AND_LEAVES ? 1 : 0);

    var allCachedProperty = this._visibleDescendantsCachedProperty,
        cachedLengthProperty = this._cachedLengthProperty,
        treeLoadingMarker = isc.ResultTree.getLoadingMarker(),
        openSubfoldersAllowed = (
            node[this.canReturnOpenSubfoldersProperty] != null ?
            node[this.canReturnOpenSubfoldersProperty] : this.canReturnOpenFolders),
        childrenResultSet = isc.isA.ResultSet(children),
        
        j = i;

    var length = (children == null ? 0 :
            (childrenResultSet ? children._getCachedLength() : children.getLength()));

    
    for (var z = 0; z < (separateFolders ? 2 : 1); ++z) {
        var firstPass = (z == 0),
            justFolders = (firstPass == foldersBeforeLeaves),
            justLeaves = (firstPass == leavesBeforeFolders),
            startedSubrange = false,
            subrangeStart = 0,
            p = 0;

        for ( ; j < end && p < length; ++p) {
            

            
            var child = children.getCachedRow(p);
            
            var cachedFlag = (
                child != null && (!this.isOpen(child) || child[allCachedProperty]));

            if (child == null) {
                
                var firstThreeConditions = (
                    openSubfoldersAllowed || rangeLoading || defaultChildLength == 0);
                if (firstPass && (firstThreeConditions || separateFolders || j >= start)) {
                    if (firstThreeConditions) {
                        if (!rangeLoading) {
                            // Fill in the rest of the range with LOADING markers.
                            for (var r = Math.max(start, j); r < end; ++r) {
                                range[r - start] = treeLoadingMarker;
                            }
                            rangeLoading = info.rangeLoading = true;
                            info.rangeLength = (end - start);
                            if (dontFetch) {
                                return;
                            }
                        }
                    } else {
                        
                        subrangeLoading = separateFolders;

                        if (j >= start) {
                            range[j - start] = treeLoadingMarker;
                            ++info.rangeLength;
                        }
                    }

                    
                    if (!(dontFetch || startedSubrange)) {
                        startedSubrange = true;
                        subrangeStart = p;
                        if (!needQueue) {
                            info.wasAlreadyQueuing = isc.RPCManager.startQueue();
                            needQueue = info.needQueue = true;
                        }
                    }
                }

                
                j += (separateFolders && firstPass ? 0 : defaultChildLength);

            } else {
                
                if (startedSubrange) {
                    
                    startedSubrange = false;
                    var prevProgressiveLoading = children.progressiveLoading;
                    children.progressiveLoading = false;
                    children.getRange(subrangeStart, p);
                    children.progressiveLoading = prevProgressiveLoading;
                }

                
                if (this.isFolder(child) ? !justLeaves : !justFolders) {
                    var childLength, visibleChild;
                    if (child == root) {
                        childLength = child[cachedLengthProperty];
                        visibleChild = true;
                    } else {
                        childLength = this._getNodeLengthToParent(child, node);
                        visibleChild = this._isNodeVisibleToParent(child, node);
                    }
                    var descendantsLength = childLength - (visibleChild ? 1 : 0);
                    if (j >= start && visibleChild && !rangeLoading) {
                        range[j - start] = (subrangeLoading ? treeLoadingMarker : child);
                        ++info.rangeLength;
                    }
                    var k = j + (visibleChild ? 1 : 0),
                        l = j + childLength;
                    if (k < end && (!cachedFlag || (l >= start && descendantsLength > 0))) {
                        var grandchildren = this.getChildren(child);
                        this._getRange(
                            root, child, grandchildren,
                            k, start, end, false,
                            // Only allow progressive loading mode to be utilized for
                            // loading the last node of the tree or its siblings.
                            (p == length - 1 && progressiveLoading),
                            subrangeLoading,
                            info);
                        rangeLoading = info.rangeLoading;
                        needQueue = info.needQueue;
                        if (dontFetch && rangeLoading) {
                            return;
                        }
                    }
                    j = l;
                }
            }
        }

        if (startedSubrange) {
            
            startedSubrange = false;
            var prevProgressiveLoading = children.progressiveLoading;
            children.progressiveLoading = progressiveLoading;
            children.getRange(subrangeStart, p);
            children.progressiveLoading = prevProgressiveLoading;
        }
    }
    

    if (length == 0 || (childrenResultSet && !children.lengthIsKnown())) {
        if (!rangeLoading) {
            // Fill in the rest of the range with LOADING markers.
            for (var r = Math.max(start, j); r < end; ++r) {
                range[r - start] = treeLoadingMarker;
            }
            rangeLoading = info.rangeLoading = true;
            info.rangeLength = (end - start);
            if (dontFetch) {
                return;
            }
        }

        if (!needQueue) {
            info.wasAlreadyQueuing = isc.RPCManager.startQueue();
            needQueue = info.needQueue = true;
        }

        // Copy the progressiveLoading setting onto the ResultSet for this request only.
        var prevProgressiveLoading;
        if (childrenResultSet) {
            prevProgressiveLoading = children.progressiveLoading;
            children.progressiveLoading = progressiveLoading;
        }

        if (separateFolders || (defaultChildLength == 0)) {
            
            if (childrenResultSet) {
                children._fetchAllRemoteData();
            } else {
                this._loadChildren(node, null, null, null);
            }
        } else if (childrenResultSet) {
            children.getRange(0, end - j);
        } else {
            this._loadChildren(node, null, null, null);
        }

        // Restore the original progressiveLoading setting.
        if (childrenResultSet) {
            children.progressiveLoading = prevProgressiveLoading;
        }
    }

    if (recursionTopLevel && needQueue && !info.wasAlreadyQueuing) {
        
        isc.RPCManager.sendQueue();
    }
},


_getProgressiveLoading : function () {
    return (this.isPaged() && this.progressiveLoading);
},


_canonicalizeChildren : function (node, children, alreadyInitialized) {
    

    
    if (isc.isA.ResultSet(children)) {
        node[this.childrenProperty] = children;
        return children;
    } else if (this._linkingNodes === true) {
        return children;
    }

    var childCount = node[this.childCountProperty],
        numChildren = (isc.isAn.Array(children) ? children.length : 0),
        validChildCount = (
            childCount != null &&
            isc.isA.Number(childCount) &&
            Math.floor(childCount) == childCount &&
            childCount >= 0);

    //>DEBUG
    if (validChildCount && childCount < numChildren) {
        isc.logWarn(
            "The child count of a record was set to " + childCount + ".  The number of " +
            "children was " + numChildren + ".  The number of children cannot exceed the " +
            "child count.  Clamping the number of children to the child count.");

        children = node[this.childrenProperty] = children.slice(0, childCount);
    }
    //<DEBUG

    if (validChildCount) {
        

        // Define a list of all of the ResultSets of children.
        this._resultSetChildren = this._resultSetChildren || [];

        if (this.dataArrived != null) {
            isc.Func.replaceWithMethod(this, "dataArrived", "parentNode");
        }

        
        var openNormalizer = this._openNormalizer,
            sortSpecifiers = this._sortSpecifiers,
            clonedSortSpecifiers = false;
        if (isc.isAn.Array(sortSpecifiers) && openNormalizer != null) {
            for (var i = sortSpecifiers.length; i--; ) {
                var sort = sortSpecifiers[i];
                if (sort != null && sort.normalizer === openNormalizer) {
                    if (!clonedSortSpecifiers) {
                        sortSpecifiers = sortSpecifiers.duplicate();
                        clonedSortSpecifiers = true;
                    }
                    sortSpecifiers[i] = isc.addProperties({}, sort, { normalizer: null });
                }
            }
        }

        
        var dataSource = this.getDataSource(),
            initialData;
        if (this.keepParentsOnFilter) {
            initialData = (children ? children.duplicate() : []);
        } else {
            initialData = dataSource.applyFilter(children, this.criteria, this.context);
        }
        var initialLength = Math.max(
                initialData.length,
                node[this.childCountProperty] + (initialData.length - numChildren));

        var context = this.context && isc.addProperties({}, this.context);
        if (context) {
            delete context.clientContext;
            delete context.internalClientContext;
        }

        // If keepParentsOnFilter is enabled then the same-named flag is also set on all
        // DSRequests issue by the tree (including all DSRequests issued by any ResultSet of
        // children nodes in the tree).
        if (this.keepParentsOnFilter) {
            context = context || {};
            context.keepParentsOnFilter = true;
        }

        var resultSetConfig = {
            init : function () {
                var tree = this._tree;
                this._parentNode[tree.childrenProperty] = this;

                // Add this ResultSet to the list of ResultSet children.
                if (!tree._resultSetChildren.contains(this)) {
                    tree._resultSetChildren.add(this);
                }

                if (tree.dataArrived != null) {
                    tree.observe(
                        this, "dataArrived", "observer.dataArrived(observed._parentNode)");
                }

                // Cache criteria that enforces the tree relationship.
                var node = this._parentNode,
                    relationship = tree._getRelationship(node, false);
                this._loadChildrenCriteria = tree._getLoadChildrenCriteria(
                        node, relationship, false);

                var ret = this.Super("init", arguments);

                
                var parent = this._parentNode,
                    grandParent = parent != tree.root && tree.getParent(parent),
                    openSubfoldersAllowed = (
                        parent[tree.canReturnOpenSubfoldersProperty] != null ?
                        parent[tree.canReturnOpenSubfoldersProperty] : tree.canReturnOpenFolders),
                    defaultChildLength = (
                        tree.openDisplayNodeType == isc.Tree.FOLDERS_AND_LEAVES ? 1 : 0),
                    knownLengthNulls = !(openSubfoldersAllowed || defaultChildLength == 0);
                if (knownLengthNulls) {
                    var origLength = (
                            grandParent &&
                            tree._getNodeLengthToParent(parent, grandParent));
                    parent[tree._cachedLengthProperty] = tree._getNodeLength(parent);
                    if (grandParent) {
                        var deltaLength = (
                                tree._getNodeLengthToParent(parent, grandParent) - origLength);
                        tree._updateParentLengths(grandParent, deltaLength);
                    }
                }

                
                if (this._alreadyInitialized) {
                    this.addProperties(tree._childrenResultSetProperties);
                }
                delete this._alreadyInitialized;

                return ret;
            },

            
            _tree: this,
            _parentNode: node,

            _alreadyInitialized: alreadyInitialized,

            dataSource: dataSource,
            criteria: this.criteria,
            context: context,
            disableCacheSync: this.disableCacheSync,
            updateCacheFromRequest: this.updateCacheFromRequest,
            sortSpecifiers: sortSpecifiers,

            initialData: initialData,
            initialLength: initialLength,

            
            setCriteria : function (newCriteria) {
                var tree = this._tree;
                newCriteria = arguments[0] = tree._combineCriteria(
                    newCriteria, this._loadChildrenCriteria);
                return this.Super("setCriteria", arguments);
            }
        };

        if (this.keepParentsOnFilter) {
            
            resultSetConfig.dropCacheOnUpdate = true;
        }

        if (!alreadyInitialized) {
            isc.addProperties(resultSetConfig, this._childrenResultSetProperties);
        }

        children = node[this.childrenProperty] = isc.ResultSet.create(resultSetConfig);

        // The ResultSet currently has only a partial cache of the children of the node, so
        // mark the node as partially loaded.
        this.setLoadState(node, isc.Tree.LOADED_PARTIAL_CHILDREN);

        this._setVisibleDescendantsCached(node, false, null, false);

        
    }
    return children;
},


_combineCriteria : function (criteria1, criteria2) {
    var criteria1Null = (criteria1 == null || isc.isAn.emptyObject(criteria1)),
        criteria2Null = (criteria2 == null || isc.isAn.emptyObject(criteria2));
    if (criteria1Null || criteria2Null) {
        if (!criteria1Null) {
            return criteria1;
        } else if (!criteria2Null) {
            return criteria2;
        } else {
            return (criteria1 || criteria2);
        }
    }

    var advanced = (
            criteria1._constructor == "AdvancedCriteria" ||
            criteria2._constructor == "AdvancedCriteria");
    if (!advanced) {
        var cloned = false;
        for (var key in criteria1) {
            if (criteria2[key] != undefined) {
                if (criteria2[key] !== criteria1[key]) {
                    advanced = true;
                } else {
                    if (!cloned) {
                        criteria2 = isc.addProperties({}, criteria2);
                        cloned = true;
                    }
                    delete criteria2[key];
                }
            }
        }
    }

    var combinedCriteria = isc.DataSource.combineCriteria(criteria1, criteria2, "and");
    
    advanced = (combinedCriteria._constructor == "AdvancedCriteria");

    // Fix instances of { operator: "iContains", value: null } in advanced
    // criteria to use the "isNull" operator instead.
    if (advanced && isc.isAn.Array(combinedCriteria.criteria)) {
        var subcriteria = combinedCriteria.criteria;
        for (var i = subcriteria.length; i--; ) {
            var subcriterion = subcriteria[i];
            if (subcriterion.operator == "iContains" && subcriterion.value == null) {
                subcriterion.operator = "isNull";
                delete subcriterion.value;
            }
        }
    }

    return combinedCriteria;
},

// Override Tree.setSort() to call setSort() on any ResultSets of children in paged trees.
setSort : function (sortSpecifiers) {
    var ret = this.invokeSuper(isc.ResultTree, "setSort", sortSpecifiers);
    sortSpecifiers = this._sortSpecifiers; // set by Tree.setSort()

    if (this.isPaged()) {
        var resultSetChildren = this._resultSetChildren;

        if (resultSetChildren != null && resultSetChildren.length > 0) {
            var wasAlreadyQueuing = isc.RPCManager.startQueue();

            for (var i = resultSetChildren.length; i--; ) {
                var children = resultSetChildren[i];
                children.setSort(sortSpecifiers);
            }

            if (!wasAlreadyQueuing) {
                isc.RPCManager.sendQueue();
            }
        }
    }

    return ret;
},

_getStartRow : function (node) {
    if (node == null) {
        return null;
    }

    var root = this.getRoot(),
        parent = null,
        start = 0;

    for ( ; node != root; node = parent) {
        parent = this.getParent(node);
        if (parent != null) {
            if (!this._includeNodeLengthInParent(node, parent)) {
                return null;
            }

            var children = this.getChildren(parent),
                j = children.indexOf(node);

            if (j == -1) {
                return null;
            }

            for (var i = 0; i < j; ++i) {
                var child = children.getCachedRow(i);
                if (child != null) {
                    start += this._getNodeLengthToParent(child, parent);
                }
            }

            if (this._isNodeVisibleToParent(node, parent)) {
                ++start;
            }
        } else if (node != root) {
            // The node is not actually in the tree!
            return undefined;
        }
    }

    return start;
},

_setVisibleDescendantsCached : function (node, newAllCached, parent, recalc) {
    
    var allCachedProperty = this._visibleDescendantsCachedProperty,
        prevAllCached = node[allCachedProperty];

    if (newAllCached == null) {
        

        // UNLOADED, LOADING, and FOLDERS_LOADED LoadStates cause the allCachedProperty to be
        // false.
        var loadState = this.getLoadState(node);
        newAllCached = (
            loadState === isc.Tree.LOADED ||
            loadState === isc.Tree.LOADED_PARTIAL_CHILDREN);

        
        var children = this.getChildren(
                node, undefined, undefined, undefined, undefined, undefined, true, undefined),
            childrenResultSet = isc.isA.ResultSet(children);

        if (newAllCached && (childrenResultSet || isc.isAn.Array(children))) {
            newAllCached = newAllCached && (
                (!childrenResultSet || (
                    children.allMatchingRowsCached() && children.lengthIsKnown())));

            for (var i = 0, length = children.getLength(); newAllCached && i < length; ++i) {
                var child = children.getCachedRow(i);
                newAllCached = (child != null && (
                    !this.isOpen(child) || child[allCachedProperty] || false));
            }
        }
    }

    
    node[allCachedProperty] = newAllCached;

    
    parent = parent || this.getParent(node);
    if ((recalc || !prevAllCached) && newAllCached) {
        
        if (parent != null && this.isOpen(node)) {
            var loadState = this.getLoadState(parent),
                children = this.getChildren(parent),
                childrenResultSet = isc.isA.ResultSet(children),
                allCached = (
                    (loadState === isc.Tree.LOADED ||
                        loadState === isc.Tree.LOADED_PARTIAL_CHILDREN) &&
                    (!childrenResultSet || (
                        children.allMatchingRowsCached() && children.lengthIsKnown())));

            if (allCached && (childrenResultSet || isc.isAn.Array(children))) {
                var index = children.indexOf(node);

                
                for (var i = children.getLength(); allCached && i-- > 0; ) {
                    if (i != index) {
                        var child = children.getCachedRow(i);
                        allCached = (child != null &&
                            (!this.isOpen(child) || child[allCachedProperty]));
                    }
                }
            }

            if (allCached) {
                this._setVisibleDescendantsCached(parent, true, null, false);
            } else {
                
            }
        }
    } else if ((recalc || prevAllCached) && !newAllCached) {
        
        while (parent != null && parent[allCachedProperty] && this.isOpen(node)) {
            parent[allCachedProperty] = false;
            node = parent;
            parent = this.getParent(parent);
        }
    }
},


__add : function (node, parent, position) {
    if (this.isPaged() && !this.keepParentsOnFilter) {
        var validRows = this.getDataSource().applyFilter([node], this.criteria, this.context);
        if (validRows.length == 0) {
            return;
        }
    }
    return this.invokeSuper(isc.ResultTree, "__add", node, parent, position);
},


_preAdd : function (node, parent, removeCollisions, info) {
    var ret = this.invokeSuper(isc.ResultTree, "_preAdd", node, parent, removeCollisions, info);
    if (this.isPaged()) {
        var loadState = this.getLoadState(node),
            newAllCached = (
                loadState === isc.Tree.LOADED ||
                loadState === isc.Tree.LOADED_PARTIAL_CHILDREN);

        this._setVisibleDescendantsCached(node, newAllCached, parent, true);
    }
    return ret;
},


setRoot : function (newRoot, autoOpen) {
    if (this.isPaged()) {
        
        var newRootFromSameTree = (newRoot && isc.endsWith(this.parentProperty, this.ID));
        this._cleanResultSetChildren(newRootFromSameTree ? newRoot : this.getRoot(), true);
    }
    return this.invokeSuper(isc.ResultTree, "setRoot", newRoot, autoOpen);
},


_preRemove : function (node, parent, info) {
    if (this.isPaged()) {
        this._setVisibleDescendantsCached(node, true, parent, false);
        delete node[this._visibleDescendantsCachedProperty];
        this._cleanResultSetChildren(node, false);
    }
    return this.invokeSuper(isc.ResultTree, "_preRemove", node, parent, info);
},


unloadChildren : function (node, displayNodeType) {
    if (this.isPaged()) {
        this._cleanResultSetChildren(node, false);
    }
    return this.invokeSuper(isc.ResultTree, "unloadChildren", node, displayNodeType);
},

_cleanResultSetChildren : function (node, cleanNonDescendants) {
    
    var resultSetChildren = this._resultSetChildren;
    for (var i = (resultSetChildren != null ? resultSetChildren.length : 0); i--; ) {
        var children = resultSetChildren[i],
            parentNode = children._parentNode;

        if (cleanNonDescendants ^ (parentNode == node || this.isDescendantOf(parentNode, node))) {
            
            resultSetChildren.removeAt(i);
            if (this.isObserving(children, "dataArrived")) {
                this.ignore(children, "dataArrived");
            }
            delete children._parentNode;
            delete children._tree;
            delete children.context.keepParentsOnFilter;
            children.setCriteria = isc.ResultSet.getInstanceProperty("setCriteria");
            // Clear the properties from _childrenResultSetProperties.
            delete children._dataAdd;
            delete children._dataAdded;
            delete children._dataRemove;
            delete children._dataRemoved;
            delete children._dataSplice;
            delete children._dataSpliced;
            delete children._dataMoved;
            delete children._dataLengthIsKnownChanged;
        }
    }
},


changeDataVisibility : function (node, newState, callback) {
//!DONTOBFUSCATE  (obfuscation breaks the inline function definitions)

    if (this.isPaged()) {
        

        var parent = this.getParent(node),
            state = node[this.openProperty],
            changed = (!this.isLeaf(node) && (state ^ newState)),
            closedToOpen = (changed && !state && newState),
            openToClosed = (changed && !closedToOpen),
            visibleCached = node[this._visibleDescendantsCachedProperty],
            parentVisibleCached = (
                parent != null && parent[this._visibleDescendantsCachedProperty]);

        
        var ret = this.invokeSuper(isc.ResultTree, "changeDataVisibility", node, newState, callback);
        if (parent != null && !visibleCached && (
            (closedToOpen && parentVisibleCached) || (openToClosed && !parentVisibleCached)))
        {
            this._setVisibleDescendantsCached(parent, null, null, false);
        }
        return ret;
    } else {
        return this.invokeSuper(isc.ResultTree, "changeDataVisibility", node, newState, callback);
    }
},

_childrenDataAdd : function (children, parent, addedChildren, addedLength, index, before) {
    var loadingMarker = isc.ResultSet.getLoadingMarker();
    

    
    var openSubfoldersAllowed = (
            parent[this.canReturnOpenSubfoldersProperty] != null ?
            parent[this.canReturnOpenSubfoldersProperty] : this.canReturnOpenFolders),
        defaultChildLength = (
            this.openDisplayNodeType == isc.Tree.FOLDERS_AND_LEAVES ? 1 : 0),
        knownLengthNulls = (
            // The conditions for null/LOADING nodes said to have length 1:
            !(openSubfoldersAllowed || defaultChildLength == 0) &&
            // Only add the 1s from null/LOADING nodes into the length once (so do this only
            // when `!before` is true) and only when the parent's length incorporates the
            // lengths of its children.
            !before &&
            this._includeNodeLengthInParent(null, parent));

    if (addedChildren == null || addedChildren == loadingMarker) {
        if (knownLengthNulls) {
            this._updateParentLengths(parent, addedLength);
        }
        return;
    }
    if (addedLength == 1 && !isc.isAn.Array(addedChildren)) {
        addedChildren = [addedChildren];
    }

    var parentStartRow = this._getStartRow(parent);
    if (parentStartRow === undefined) {
        
        return;
    }

    if (before) {
        parent[this._recursionCountProperty] = 1 + (parent[this._recursionCountProperty] || 0);
    }

    var infoStack = (this._infoStack = this._infoStack || []),
        collisionsStack = (this._collisionsStack = this._collisionsStack || []),
        collisions,
        grandChildrenStack = [],
        deltaParentLength = 0,
        grandParent,
        origParentLength;
    if (before) {
        collisions = [];
        collisionsStack.push(collisions);
    } else {
        collisions = collisionsStack.pop();
    }

    
    var i0 = (before ? 0 : addedLength - 1),
        iInc = (before ? 1 : -1);
    for (var i = i0; (before ? i < addedLength : i >= 0); i += iInc) {
        var addedChild = addedChildren[i];
        if (addedChild != null && addedChild != loadingMarker) {
            if (before) {
                var info = {};
                infoStack.push(info);
                var collision = this._findCollision(addedChild);
                if (collision) {
                    collisions.add(collision);
                }
                this._preAdd(addedChild, parent, false, info);
            } else {
                var info = infoStack.pop();
                this._postAdd(addedChild, parent, index, info);
                grandChildrenStack.push(info.grandChildren);

                
                deltaParentLength += info.deltaParentLength;
                grandParent = info.grandParent;
                origParentLength = info.origParentLength;
            }
        } else if (knownLengthNulls) {
            // The `knownLengthNulls` flag indicates that this null/LOADING is given length 1.
            ++parent[this._cachedLengthProperty];
        }
    }

    if (!before) {
        

        
        var fromParent = (parent[this.canReturnOpenSubfoldersProperty] != null),
            openSubfoldersNotAllowed = !openSubfoldersAllowed;

        for (var i = 0; i < addedLength; ++i) {
            var addedChild = addedChildren[i];

            if (addedChild != null && addedChild != loadingMarker) {
                var grandChildren = this._canonicalizeChildren(addedChild, grandChildrenStack.pop(), false);

                
                if (openSubfoldersNotAllowed &&
                    this.isOpen(addedChild) &&
                    grandChildren != null && !grandChildren.isEmpty())
                {
                    this.logWarn(
                        "Adding the open folder node '" + this.getPath(addedChild) + "' as " +
                        "a child of the parent node '" + this.getPath(parent) + "' is " +
                        "contradictory to the setting of the " + (fromParent ? "'" +
                        this.canReturnOpenSubfoldersProperty + "' property of the parent node." :
                        "'canReturnOpenFolders' property of the tree."));
                }

                if (grandChildren != null) {
                    if (isc.isA.ResultSet(grandChildren)) {
                        
                        if (!(grandChildren.lengthIsKnown() && grandChildren.allMatchingRowsCached())) {
                            this._setVisibleDescendantsCached(addedChild, false, parent, false);
                        }
                    } else if (!isc.isAn.Array(grandChildren)) {
                        this.__add(grandChildren, addedChild);
                    } else {
                        this.__addList(grandChildren, addedChild);
                    }
                }
            }
        }

        
        if ((--parent[this._recursionCountProperty]) == 0) {
            delete parent[this._recursionCountProperty];
            if (grandParent) {
                deltaParentLength += (this._getNodeLengthToParent(parent, grandParent) - origParentLength);
                this._updateParentLengths(grandParent, deltaParentLength);
            }
        }


        this._setVisibleDescendantsCached(parent, null, null, false);

        if (collisions.length > 0) {
            for (var i = collisions.length; i--; ) {
                var collision = collisions.pop();
                this._removeCollision(collision);
            }
        }
    }
},

_childrenDataRemove : function (children, parent, removedChildren, removedLength, index, before) {
    var loadingMarker = isc.ResultSet.getLoadingMarker();
    

    // Define conditions for when null/LOADING nodes are given length 1.
    var openSubfoldersAllowed = (
            parent[this.canReturnOpenSubfoldersProperty] != null ?
            parent[this.canReturnOpenSubfoldersProperty] : this.canReturnOpenFolders),
        defaultChildLength = (
            this.openDisplayNodeType == isc.Tree.FOLDERS_AND_LEAVES ? 1 : 0),
        knownLengthNulls = (
            !(openSubfoldersAllowed || defaultChildLength == 0) &&
            !before &&
            this._includeNodeLengthInParent(null, parent));

    if (removedChildren == null || removedChildren == loadingMarker) {
        if (knownLengthNulls) {
            this._updateParentLengths(parent, -removedLength);
        }
        return;
    }
    if (removedLength == 1 && !isc.isAn.Array(removedChildren)) {
        removedChildren = [removedChildren];
    }

    var parentStartRow = this._getStartRow(parent);
    if (parentStartRow === undefined) {
        return;
    }

    if (before) {
        parent[this._recursionCountProperty] = 1 + (parent[this._recursionCountProperty] || 0);
    }

    var infoStack = (this._infoStack = this._infoStack || []),
        grandParent,
        origParentLength;

    var i0 = (before ? 0 : removedLength - 1),
        iInc = (before ? 1 : -1);
    for (var i = i0; (before ? i < removedLength : i >= 0); i += iInc) {
        var removedChild = removedChildren[i];
        if (removedChild != null && removedChild != loadingMarker) {
            if (before) {
                var info = {};
                infoStack.push(info);
                this._preRemove(removedChild, parent, info);
            } else {
                var info = infoStack.pop();
                this._postRemove(removedChild, parent, info);
                grandParent = info.grandParent;
                origParentLength = info.origParentLength;
            }
        } else if (knownLengthNulls) {
            --parent[this._cachedLengthProperty];
        }
    }

    if (!before) {
        
        if ((--parent[this._recursionCountProperty]) == 0) {
            delete parent[this._recursionCountProperty];
            if (grandParent) {
                var deltaParentLength = (
                        this._getNodeLengthToParent(parent, grandParent) - origParentLength);
                this._updateParentLengths(grandParent, deltaParentLength);
            }
        }

        this._setVisibleDescendantsCached(parent, null, null, false);
    }
},

_childrenDataSplice : function (children, parent, removedChildren, removedLength, index, addedChildren, addedLength, before) {
    var loadingMarker = isc.ResultSet.getLoadingMarker();
    

    // Define conditions for when null/LOADING nodes are given length 1.
    var openSubfoldersAllowed = (
            parent[this.canReturnOpenSubfoldersProperty] != null ?
            parent[this.canReturnOpenSubfoldersProperty] : this.canReturnOpenFolders),
        defaultChildLength = (
            this.openDisplayNodeType == isc.Tree.FOLDERS_AND_LEAVES ? 1 : 0),
        knownLengthNulls = (
            !(openSubfoldersAllowed || defaultChildLength == 0) &&
            !before &&
            this._includeNodeLengthInParent(null, parent));

    var removedNodes = !(removedChildren == null || removedChildren == loadingMarker),
        addedNodes = !(addedChildren == null || addedChildren == loadingMarker);

    if (!(removedNodes || addedNodes)) {
        if (knownLengthNulls) {
            this._updateParentLengths(parent, addedLength - removedLength);
        }
        // There is nothing to do.
        return;
    } else if (!addedNodes) {
        if (knownLengthNulls) {
            this._updateParentLengths(parent, addedLength);
        }
        this._childrenDataRemove(children, parent, removedChildren, removedLength, index, before);
        return;
    } else if (!removedNodes) {
        if (knownLengthNulls) {
            this._updateParentLengths(parent, -removedLength);
        }
        this._childrenDataAdd(children, parent, addedChildren, addedLength, index, before);
        return;
    }

    

    var parentStartRow = this._getStartRow(parent);
    if (parentStartRow === undefined) {
        return;
    }

    if (before) {
        parent[this._recursionCountProperty] = 1 + (parent[this._recursionCountProperty] || 0);
    }

    var infoStack = (this._infoStack = this._infoStack || []),
        collisionsStack = (this._collisionsStack = this._collisionsStack || []),
        collisions,
        grandChildrenStack = [],
        deltaParentLength = 0,
        grandParent,
        origParentLength;
    if (before) {
        collisions = [];
        collisionsStack.push(collisions);
    } else {
        collisions = collisionsStack.pop();
    }

    var i0 = (before ? 0 : addedLength + removedLength - 1),
        iInc = (before ? 1 : -1);
    for (var i = i0; (before ? i < addedLength + removedLength : i >= 0); i += iInc) {
        
        var add = (removedLength <= i),
            child = (add ? addedChildren[i - removedLength] : removedChildren[i]);
        if (child != null && child != loadingMarker) {
            if (before) {
                var info = {};
                infoStack.push(info);
                if (add) {
                    var collision = this._findCollision(child);
                    if (collision && !(removedNodes && removedChildren.contains(collision))) {
                        collisions.add(collision);
                    }
                    this._preAdd(child, parent, false, info);
                } else {
                    this._preRemove(child, parent, info);
                }
            } else {
                var info = infoStack.pop();
                if (add) {
                    this._postAdd(child, parent, index, info);
                    grandChildrenStack.push(info.grandChildren);

                    
                    deltaParentLength += info.deltaParentLength;
                } else {
                    this._postRemove(child, parent, info);
                }
                grandParent = info.grandParent;
                origParentLength = info.origParentLength;
            }
        } else if (knownLengthNulls) {
            if (add) {
                ++parent[this._cachedLengthProperty];
            } else {
                --parent[this._cachedLengthProperty];
            }
        }
    }

    if (!before) {
        
        var fromParent = (parent[this.canReturnOpenSubfoldersProperty] != null),
            openSubfoldersNotAllowed = !openSubfoldersAllowed;

        for (var i = 0, j = 0; i < addedLength; ++i) {
            var addedChild = addedChildren[i];
            if (addedChild != null && addedChild != loadingMarker) {
                var grandChildren = this._canonicalizeChildren(addedChild, grandChildrenStack.pop(), false);

                
                if (openSubfoldersNotAllowed &&
                    this.isOpen(addedChild) &&
                    grandChildren != null && !grandChildren.isEmpty())
                {
                    this.logWarn(
                        "Adding the open folder node '" + this.getPath(addedChild) + "' as " +
                        "a child of the parent node '" + this.getPath(parent) + "' is " +
                        "contradictory to the setting of the " + (fromParent ? "'" +
                        this.canReturnOpenSubfoldersProperty + "' property of the parent node." :
                        "'canReturnOpenFolders' property of the tree."));
                }

                if (grandChildren != null) {
                    if (isc.isA.ResultSet(grandChildren)) {
                        if (!(grandChildren.lengthIsKnown() && grandChildren.allMatchingRowsCached())) {
                            this._setVisibleDescendantsCached(addedChild, false, parent, false);
                        }
                    } else if (!isc.isAn.Array(grandChildren)) {
                        this.__add(grandChildren, addedChild);
                    } else {
                        this.__addList(grandChildren, addedChild);
                    }
                }
            }
        }

        
        if ((--parent[this._recursionCountProperty]) == 0) {
            delete parent[this._recursionCountProperty];
            if (grandParent) {
                deltaParentLength += (this._getNodeLengthToParent(parent, grandParent) - origParentLength);
                this._updateParentLengths(grandParent, deltaParentLength);
            }
        }


        this._setVisibleDescendantsCached(parent, null, null, false);

        if (collisions.length > 0) {
            for (var i = collisions.length; i--; ) {
                var collision = collisions.pop();
                this._removeCollision(collision);
            }
        }
    }
},

_childrenDataMoved : function (children, parent, movedChildren, movedLength, oldIndex, newIndex) {
    var loadingMarker = isc.ResultSet.getLoadingMarker();
    
},


_childrenDataLengthIsKnownChanged : function (children, parent, oldValue, newValue) {
    var loadingMarker = isc.ResultSet.getLoadingMarker();
    

    
    this._setVisibleDescendantsCached(parent, null, null, false);

},


setDefaultLoadState : function (newDefaultLoadState) {
    var prevDefaultLoadState = this.defaultLoadState;
    this.defaultLoadState = newDefaultLoadState;

    if (this.isPaged() && (
            prevDefaultLoadState !== newDefaultLoadState ||
            (prevDefaultLoadState == null) ^ (newDefaultLoadState == null)))
    {
        var prevFlag = (
                prevDefaultLoadState === isc.Tree.LOADED ||
                prevDefaultLoadState === isc.Tree.LOADED_PARTIAL_CHILDREN),
            newFlag = (
                newDefaultLoadState === isc.Tree.LOADED ||
                newDefaultLoadState === isc.Tree.LOADED_PARTIAL_CHILDREN);

        if (prevFlag ^ newFlag) {
            
            var root = this.getRoot(),
                nodesWithDefaultLoadState = this.findAll("_loadState", null);
            
            if (root._loadState == null) {
                if (nodesWithDefaultLoadState == null) {
                    nodesWithDefaultLoadState = [root];
                } else {
                    nodesWithDefaultLoadState.add(root);
                }
            }

            if (nodesWithDefaultLoadState != null) {
                for (var i = nodesWithDefaultLoadState.length; i--; ) {
                    var node = nodesWithDefaultLoadState[i],
                        parent = this.getParent(node);
                    this._setVisibleDescendantsCached(node, null, parent, true);
                }
            }
        }
    }
},


// Criteria / Filtering
// --------------------------------------------------------------------------------------------

//> @method resultTree.setCriteria()
// Set the filter criteria to use when fetching rows.
// <P>
// Depending on the result of +link{compareCriteria()} and setting for
// +link{resultTree.fetchMode}, setting criteria may cause a trip to the server to get a new
// set of nodes, or may simply cause already-fetched nodes to be re-filtered according to the
// new criteria.
// <P>
// For a basic overview on when server fetches are generally performed, see
// +link{resultTree.fetchMode}.
// However, this is not the final determination of when server fetches occur. Criteria can
// be split into local criteria and server criteria by specifying +link{serverFilterFields}.
// Thus, even when using fetchMode:"local" a new server fetch will occur if the server
// criteria changes. For details on how the criteria is split, see
// +link{dataSource.splitCriteria}.
// <P>
// Note: if criteria is being split to retrieve server criteria portion and the criteria
// is an +link{AdvancedCriteria}, the criteria must consist of a single "and" operator
// and one or more simple criteria below it. No other logical operators may be used. In
// other words, the +link{AdvancedCriteria} provided must be exactly representable by a
// simple criteria.
//
// @param newCriteria (Criteria) the filter criteria
// @visibility external
//<
// An overview on tree caching:
// We have 2 kinds of cache
// - the cache of results that matches the current criteria: this.data
// - the cache of the full tree received from the server: this.completeTree
//   * When in local filtering mode both caches are populated. New records from the
//     server always go to completeTree and then the visible tree is filtered from
//     that. Local filtering mode only occurs in fetchMode:"local" with a client-side
//     criteria. Note that if server criteria changes the completeTree cache is
//     overwritten with data from the server and the local cache rebuilt from client-side
//     criteria.
//   * When in basic filtering mode, we only populate the local cache: this.data.
//     On the first fetch we fill this local cache with the results returned from the server.
//     On subsequent changes to filter criteria, we will perform a new server fetch
//     and update the local cache.
setCriteria : function (newCriteria) {
    var oldCriteria = this.criteria || {},
        oldCriteriaIsEmpty = !this.haveCriteria(oldCriteria),
        oldServerCriteria = this._serverCriteria || {},
        ds = this.getDataSource();

    // clone the criteria passed in - avoids potential issues where a criteria object is passed in
    // and then modified outside the RS
    // Avoid this with advanced criteria - our filter builder already clones the output
    if (!ds.isAdvancedCriteria(newCriteria)) {
        // use clone to deep copy so we duplicate dates, arrays etc
        newCriteria = isc.clone(newCriteria);
    }   
    this.criteria = newCriteria;

    // If one of the criteria objects is an AdvancedCriteria, convert the other one to 
    // enable comparison
    if (ds.isAdvancedCriteria(newCriteria) && !ds.isAdvancedCriteria(oldCriteria)) {
        oldCriteria = isc.DataSource.convertCriteria(oldCriteria);
    }
    if (!ds.isAdvancedCriteria(newCriteria) && ds.isAdvancedCriteria(oldCriteria)) {
        newCriteria = isc.DataSource.convertCriteria(newCriteria);
        this.criteria = newCriteria;
    }

    // serverFilterFields is only applicable for fetchMode:"local" ResultTrees.
    if (this.isLocal() &&
        this.serverFilterFields != null &&
        this.serverFilterFields.length > 0)
    {
        this._localCriteria = isc.DataSource.copyCriteria(newCriteria);
        this._serverCriteria = ds.splitCriteria(this._localCriteria, this.serverFilterFields);
    } else {
        delete this._localCriteria;
        delete this._serverCriteria;
    }

    // See if the criteria changed
    var result = this.compareCriteria(newCriteria, oldCriteria);
    if (result != 0) {
        // Criteria changed

        if (!this.isLocal()) {
            // Not using client-side filtering, just invalidateCache
            //>DEBUG
            this.logInfo("setCriteria: filter criteria changed, invalidating cache");
            //<DEBUG

            this.invalidateCache();

            // Update the criteria on any ResultSet children.
            if (this.isPaged() && this._resultSetChildren != null) {
                var resultSetChildren = this._resultSetChildren.duplicate();
                for (var i = resultSetChildren.length; i--; ) {
                    var children = resultSetChildren[i];
                    if (this._resultSetChildren.contains(children)) {
                        var node = children._parentNode,
                            relationship = this._getRelationship(node, false),
                            childrenCriteria = this._getLoadChildrenCriteria(
                                node, relationship, false);

                        children.setCriteria(
                            isc.DataSource.combineCriteria(childrenCriteria, this.criteria));
                    }
                }
            }
        } else {
            // Filtering locally but the server part of criteria may have changed.
            // If so, we have to invalidate our cache to get a new tree.
            if (this.haveCriteria(this._serverCriteria) ?
                    (this.compareCriteria(this._serverCriteria, oldServerCriteria) != 0) :
                    this.haveCriteria(oldServerCriteria))
            {
                //>DEBUG
                this.logInfo("setCriteria: filter criteria changed, invalidating cache");
                //<DEBUG
                this.invalidateCache();
            } else {
                // No server criteria change
                var openState = this._getOpenState();

                // Make sure we have a complete tree saved if we are starting to filter locally
                if (oldCriteriaIsEmpty) {
                    this.completeTree = this.duplicate(true, true);
                }
                this.filterLocalData();
                // Local filter of existing client side data. No need to check for
                // server-specified open-state.
                this._setOpenState(openState);

                if (this.dataArrived != null) {
                    isc.Func.replaceWithMethod(this, "dataArrived", "parentNode");
                    this.dataArrived(this.getRoot());
                }
            }
        }
    }
},

filterLocalData : function (parentNode) {
    
    if (!parentNode) parentNode = this.getRoot();

    var criteria = this._localCriteria || this.criteria,
        sourceTree = this.completeTree;

    if (this.haveCriteria(criteria)) {
        

        // Filter tree
        var filterMode = (this.keepParentsOnFilter ? isc.Tree.KEEP_PARENTS : isc.Tree.STRICT),
            dataSource = this.getDataSource();

        sourceTree = this.applyFilter(this.completeTree, criteria, filterMode,
                         dataSource, this.context);
        //>DEBUG
        this.logInfo("Local filter applied: " + sourceTree.getNodeList().length
                     + " of " + this.completeTree.getNodeList().length
                     + " records matched filter:" + this.echoFull(criteria));
        //<DEBUG
    } else {
        // No criteria anymore. Drop the complete tree as there is no need
        // to keep it updated.
        //>DEBUG
        this.logInfo("Local filter applied: all " + sourceTree.getNodeList().length
                     + " records matched filter: none");
        //<DEBUG
        delete this.completeTree;
    }

    // Remove our existing tree structure (this.data),
    // and instead copy the source tree (filtered or full) into this.data
    // no need to explicitly run _linkNodes() - this will occur as part of
    // setRoot().
    var nodes = sourceTree.getAllNodes();
    nodes = sourceTree.getCleanNodeData(nodes, false, false, true);
    this.data = nodes;

    this.setRoot(this.getCleanNodeData(this.getRoot(), false, false, true));

    this._clearNodeCache(true);
},

//> @method resultTree.applyFilter() [A]
// The ResultTree will call applyFilter() when it needs to locally filter the tree using the
// current filter criteria.
// <P>
// Default behavior is to call +link{tree.getFilteredTree()} to obtain a new, filtered tree.
// <P>
// Override this method or +link{tree.getFilteredTree()} to implement your own client-side
// filtering behavior. Note that the original tree should not be affected.
//
// @param   tree        (Tree)           the source tree to be filtered
// @param   criteria    (Criteria)       the filter criteria
// @param   filterMode  (TreeFilterMode) mode to use for filtering
// @param   dataSource  (DataSource)     dataSource for filtering if the Tree does not
//                                       already have one
// @param [requestProperties] (DSRequest) Request properties block. This allows developers to specify
//  properties that would impact the filter such as +link{DSRequest.textMatchStyle}
// @return  (Tree)     the filtered tree (copy)
// @visibility external
//<
applyFilter : function (tree, criteria, filterMode, dataSource, context) {
    return tree.getFilteredTree(criteria, filterMode, dataSource, context);
},

//> @method resultTree.compareCriteria()
// Default behavior is to call +link{dataSource.compareCriteria()} to determine whether new
// criteria is equivalent to the old criteria (returns 0) or not.
// <P>
// See +link{dataSource.compareCriteria()} for a full explanation of the default behavior.
// The +link{criteriaPolicy} used is "dropOnChange".
// <P>
// Override this method or +link{dataSource.compareCriteria()} to implement your own client-side
// filtering behavior.
//
// @param   newCriteria     (Criteria)  new filter criteria
// @param   oldCriteria     (Criteria)  old filter criteria
// @param   [requestProperties]     (DSRequest Properties)  dataSource request properties
// @return  (Number)    0 if the criteria are equivalent, -1 if the criteria are different
// @see criteriaPolicy
// @visibility external
//<
compareCriteria : function (newCriteria, oldCriteria, requestProperties, policy) {
    return this.getDataSource().compareCriteria(
                newCriteria, oldCriteria,
                (requestProperties ? requestProperties : this.context),
                (policy ? policy : "dropOnChange"));
},

//> @method resultTree.willFetchData()
// Will changing the criteria for this resultTree require fetching new data from the server
// or can the new criteria be satisfied from data already cached on the client?
// <p>
// This method can be used to determine whether +link{TreeGrid.fetchData()} or 
// +link{TreeGrid.filterData()} will cause a server side fetch when passed a certain set of 
// criteria.
//
// @param newCriteria (Criteria) new criteria to test.
// @return (Boolean) true if server fetch would be required to satisfy new criteria.
// @visibility external
//<
willFetchData : function (newCriteria) {
    var undef;
    var oldCriteria = this.criteria || {},
        oldServerCriteria = this._serverCriteria || {},
        ds = this.getDataSource()
    ;

    // clone the criteria passed in - avoids potential issues where a criteria object is passed in
    // and then modified outside the RS
    // Avoid this with advanced criteria - our filter builder already clones the output
    if (!ds.isAdvancedCriteria(newCriteria)) {
        newCriteria = isc.DataSource.copyCriteria(newCriteria);
    }
        
    // If one of the criteria objects is an AdvancedCriteria, convert the other one to 
    // enable comparison
    if (ds.isAdvancedCriteria(newCriteria) && !ds.isAdvancedCriteria(oldCriteria)) {
        oldCriteria = isc.DataSource.convertCriteria(oldCriteria);
    }
    if (!ds.isAdvancedCriteria(newCriteria) && ds.isAdvancedCriteria(oldCriteria)) {
        newCriteria = isc.DataSource.convertCriteria(newCriteria);
    }

    var result = this.compareCriteria(newCriteria, oldCriteria);
    
    // If we have no change in criteria we won't perform a fetch
    if (result == 0) return false;

    // If we are not filtering locally a fetch is required
    
    if (!this.isLocal()) return true;

    // Criteria has changed but we need to see if it affects the fetches. That is the
    // case only if the server criteria portion has changed.

    // Split our criteria to obtain the new server criteria
    var newServerCriteria = {};
    if (this.isLocal() &&
        this.serverFilterFields != null &&
        this.serverFilterFields.length > 0)
    {
        var localCriteria = isc.DataSource.copyCriteria(newCriteria);
        newServerCriteria = ds.splitCriteria(localCriteria, this.serverFilterFields);
    }

    // If server criteria changed, a fetch is required
    return (this.haveCriteria(newServerCriteria) ?
                (this.compareCriteria(newServerCriteria, oldServerCriteria) != 0) :
                this.haveCriteria(oldServerCriteria));
},
    
// View state
// --------------------------------------------------------------------------------------------

 
//> @method resultTree.getOpenState() 
// Returns a snapshot of the current open state of this tree's data as
// a +link{type:TreeGridOpenState} object.
// <P>
// This object can be passed to +link{resultTree.setOpenState()} or
// +link{treeGrid.setOpenState()} to open the same set of folders
// within the tree's data (assuming the nodes are still present in the data).
// @return (TreeGridOpenState) current open state for the grid.
// @group viewState
// @see resultTree.setOpenState()
// @visibility external
//<
getOpenState : function() {
    var openState = this._getOpenState();
    return isc.Comm.serialize(openState);
},  

_getOpenState : function(isCacheRestore) {

    var root = this.getRoot(),
        openState = []
    ;
    this._addNodeToOpenState(this, root, openState, isCacheRestore);
    
    return openState;
},

//> @method resultTree.setOpenState() 
// Reset the set of open folders within this tree's data to match the 
// +link{type:TreeGridOpenState} object passed in.
// <P>
// Used to restore previous state retrieved from the tree by a call to 
// +link{resultTree.getOpenState()}.
//
// @param openState (TreeGridOpenState) Object describing the desired set of open folders.
// @group viewState
// @see resultTree.getOpenState()
// @visibility external
//<
setOpenState : function(openState) {
    openState = isc.Canvas.evalViewState(openState, "openState", false, this)
    if (!openState) return;
    this._setOpenState(openState);
},

_setOpenState : function(openState, retainServerState) {
    if (!openState) return;
    if (!retainServerState) {
        this.closeAll();
    }
    this.openFolders(openState);
},


_addNodeToOpenState : function (tree, node, openState, isCacheRestore) {
    if (!tree.isOpen(node) || !tree.isLoaded(node)) return false;
    if (isCacheRestore) {
        // explicit call to getName() will set up the "autoAssignedName" flag if this
        // method has never been called before
        tree.getName(node);
        if (this.autoPreserveOpenState == "never" || 
            (this.autoPreserveOpenState == "whenUnique" &&  node._autoAssignedName)) 
        {
//             this.logWarn("addNodeToOpenState: Skipping node::" + this.echo(node) + 
//                 " autoPreserveOpenState:" + this.autoPreserveOpenState);
             return false;
        }
    }

    var children = tree.getFolders(node),
        hasOpenChildren = false;
    if (children != null) {
        for (var i = 0; i < children.length; i++) {
            hasOpenChildren = this._addNodeToOpenState(tree, children[i], openState, isCacheRestore) 
                              || hasOpenChildren;
        }
    }
    openState[openState.length] = tree.getPath(node);
    return true;
}

});

// isc._dataModelToString and isc._dataModelLogMessage are defined in Log.js
isc.ResultTree.getPrototype().toString = isc._dataModelToString;
isc.ResultTree.getPrototype().logMessage = isc._dataModelLogMessage;

isc.ResultTree.registerStringMethods({
    
    //> @method resultTree.dataArrived
    // This callback fires whenever the resultTree receives new nodes from the server, after
    // the new nodes have been integrated into the tree.
    // 
    // @param parentNode (TreeNode) The parentNode for which children were just loaded
    //
    // @visibility external
    //<
    dataArrived: "parentNode"
});


