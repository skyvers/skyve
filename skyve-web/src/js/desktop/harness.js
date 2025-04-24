isc.BizUtil.addClassProperties({
  headerTemplate: null,

  /**
   * Initializes the layout and components for the application.
   * @param {string} headerTemplate - Template HTML for view headers
   * @param {string} logoSrc - Source URL for the logo image
   * @param {Array} menuConfig - Configuration for the menu items
   * @param {Array} dataSourceConfig - Configuration for the data sources
   */
  init(headerTemplate, logoSrc, menuConfig, dataSourceConfig) {
    isc.BizUtil.headerTemplate = headerTemplate;

    this.createBodyLayout(logoSrc);
    this.createMenuSections(menuConfig);
    this.createDataSources(dataSourceConfig);
    this.addListViewToDetails();

    this.createTextSearchComponents();
  },

  /**
   * Creates the main body layout with left-hand menu and right-hand details panel.
   * @param {string} logoSrc - Source URL for the logo image
   */
  createBodyLayout(logoSrc) {
    isc.HLayout.create({
      ID: "body",
      autoDraw: true,
      width: "100%",
      height: "100%",
      backgroundColor: "whitesmoke",
      padding: 10,
      members: [
        this.createLeftMenuPanel(logoSrc),
        this.createRightDetailsPanel(),
      ],
    });
  },

  /**
   * Creates the left-hand menu panel with logo and accordion menu.
   * @param {string} logoSrc - Source URL for the logo image
   * @returns {isc.VLayout} The left-hand menu panel
   */
  createLeftMenuPanel(logoSrc) {
    return isc.VLayout.create({
      ID: "lhsmenu",
      width: "30%",
      showResizeBar: true,
      backgroundColor: "white",
      border: "1px solid #c0c0c0",
      showShadow: true,
      shadowSoftness: 10,
      shadowOffset: 0,
      members: [
        isc.Img.create({
          imageType: "center",
          src: logoSrc,
          overflow: "clip-h",
          styleName: "logo",
          showDisabled: false,
          showDown: false,
          showFocus: false,
          showFocused: false,
          showFocusedAsOver: false,
          showRollOver: false,
        }),
        isc.SectionStack.create({
          ID: "adminStack",
          width: "100%",
          visibilityMode: "mutex",
          animateSections: false,
          overflow: "hidden",
          headerHeight: 30,
        }),
      ],
    });
  },

  /**
   * Creates the right-hand details panel.
   * @returns {isc.VLayout} The right-hand details panel
   */
  createRightDetailsPanel() {
    return isc.VLayout.create({
      ID: "details",
      width: "100%",
      height: "100%",
      backgroundColor: "whitesmoke",
      border: "1px solid #c0c0c0",
      showShadow: true,
      shadowSoftness: 10,
      shadowOffset: 0,
    });
  },

  /**
   * Creates menu sections based on the provided menu configuration.
   * @param {Array} menuConfig - Configuration for the menu items
   */
  createMenuSections(menuConfig) {
    const adminStack = isc.Canvas.getById("adminStack");

    menuConfig.forEach((item) => {
      const menu = this.createMenuTreeGrid(item);
      adminStack.addSection({
        title: item.title,
        expanded: item.open,
        items: [menu],
      });
      menu.getData().openAll();
    });
  },

  /**
   * Creates a TreeGrid for a menu item.
   * @param {Object} item - Configuration for the menu item
   * @returns {isc.TreeGrid} The created TreeGrid
   */
  createMenuTreeGrid(item) {
    return isc.TreeGrid.create({
      ID: `${item.name}Tree`,
      showHeader: false,
      showOpenIcons: false,
      showDropIcons: false,
      showConnectors: true,
      showRollOver: false,
      leaveScrollbarGap: false,
      nodeIcon: null,
      folderIcon: null,
      closedIconSuffix: "",
      canDragRecordsOut: true,
      dragDataAction: "copy",
      data: isc.Tree.create({
        modelType: "children",
        nameProperty: "desc",
        childrenProperty: "sub",
        root: item.root,
      }),
      leafClick: (viewer, leaf, recordNum) =>
        this.handleMenuItemClick(viewer, leaf, recordNum),
    });
  },

  /**
   * Handles click events on menu items.
   * @param {isc.TreeGrid} viewer - The TreeGrid that triggered the event
   * @param {Object} leaf - The clicked leaf node
   * @param {number} recordNum - The record number of the clicked leaf
   */
  handleMenuItemClick(viewer, leaf, recordNum) {
    const details = isc.Canvas.getById("details");
    if (leaf.ref === "link") {
      window.location = leaf.name;
    } else if (leaf.ref === "edit") {
      this.handleEditView(leaf, viewer, details);
    } else {
      this.handleListView(leaf, viewer, details);
    }
  },

  /**
   * Handles the display of edit views.
   * @param {Object} leaf - The clicked leaf node
   * @param {isc.TreeGrid} viewer - The TreeGrid that triggered the event
   * @param {isc.VLayout} details - The details panel
   */
  handleEditView(leaf, viewer, details) {
    if (isc.BizUtil._currentView === isc.ListView.contents) {
      details.hideMember(isc.ListView.contents);
    } else if (isc.BizUtil._currentView) {
      if (details.hasMember(isc.BizUtil._currentView)) {
        details.hideMember(isc.BizUtil._currentView);
      }
      isc.BizUtil.relinquishEditView(isc.BizUtil._currentView);
    }

    isc.BizUtil.getEditView(
      leaf.module ? leaf.module : viewer.data.root.name,
      leaf.name,
      (view) => {
        details.addMember(view);
        isc.BizUtil._currentView = view;
        view.newInstance();
      }
    );
  },

  /**
   * Handles the display of list views.
   * @param {Object} leaf - The clicked leaf node
   * @param {isc.TreeGrid} viewer - The TreeGrid that triggered the event
   * @param {isc.VLayout} details - The details panel
   */
  handleListView(leaf, viewer, details) {
    if (isc.BizUtil._currentView !== isc.ListView.contents) {
      if (isc.BizUtil._currentView) {
        if (details.hasMember(isc.BizUtil._currentView)) {
          details.hideMember(isc.BizUtil._currentView);
        }
        isc.BizUtil.relinquishEditView(isc.BizUtil._currentView);
      }
      isc.BizUtil._currentView = isc.ListView.contents;
      details.showMember(isc.ListView.contents);
    }

    const dataSourceName = `${viewer.data.root.name}_${leaf.name}`;
    switch (leaf.ref) {
      case "grid":
        isc.ListView.setGridDataSource(dataSourceName, leaf.config);
        break;
      case "cal":
        isc.ListView.setCalendarDataSource(dataSourceName, leaf.config);
        break;
      case "tree":
        isc.ListView.setTreeDataSource(dataSourceName, leaf.config);
        break;
      case "map":
        isc.ListView.setMapDataSource(dataSourceName, leaf.config);
        break;
      default:
        alert(`Menu ref of ${leaf.ref} is unknown`);
    }
  },

  /**
   * Creates data sources based on the provided configuration.
   * @param {Array} dataSourceConfig - Configuration for the data sources
   */
  createDataSources(dataSourceConfig) {
    dataSourceConfig.forEach((item) => {
      item.fields.add({ name: "operator", type: "text", hidden: true });
      item.fields.add({ name: "criteria", type: "text", hidden: true });
      item.fields.add({ name: "bizId", primaryKey: true, hidden: true });
      item.fields.add({ name: "bizLock", hidden: true });
      item.fields.addAt(
        {
          name: "bizTagged",
          title: "Tag",
          type: "boolean",
          validOperators: ["equals"],
        },
        0
      );
      item.fields.addAt(
        {
          name: "bizFlagComment",
          title: "Flag",
          canFilter: SKYVE.Util.canFlag,
        },
        1
      );

      isc.RestDataSource.create({
        ID: item.ID,
        dataFormat: "json",
        jsonPrefix: "",
        jsonSuffix: "",
        dataURL: "smartlist",
        operationBindings: [
          { operationType: "fetch", dataProtocol: "postParams" },
          { operationType: "update", dataProtocol: "postParams" },
          { operationType: "add", dataProtocol: "postParams" },
          { operationType: "remove", dataProtocol: "postParams" },
        ],
        criteriaPolicy: "dropOnChange",
        title: item.title,
        modoc: item.modoc,
        icon: item.icon,
        fontIcon: item.fontIcon,
        aggregate: item.aggregate,
        canCreate: item.canCreate,
        canUpdate: item.canUpdate,
        canDelete: item.canDelete,
        cellHeight: item.cellHeight,
        fields: item.fields,
      });
    });
  },

  /**
   * Adds the list view to the details panel.
   */
  addListViewToDetails() {
    const details = isc.Canvas.getById("details");
    isc.BizUtil._currentView = isc.ListView.contents;
    details.addMember(isc.ListView.contents);
    details.hideMember(isc.ListView.contents);
  },

  /**
   * Creates and initializes the text search components.
   */
  createTextSearchComponents() {
    isc.RestDataSource.create({
      ID: "textSearch",
      dataFormat: "json",
      dataURL: "smartsearch",
      criteriaPolicy: "dropOnChange",
      fields: [
        { name: "icon", title: "Icon" },
        { name: "doc", title: "Document" },
        { name: "bizKey", title: "Description" },
        { name: "excerpt", title: "Excerpt" },
        { name: "score", title: "Score (%)", type: "integer" },
        { name: "data", type: "link", title: "Data", target: "_self" },
        { name: "content", type: "link", title: "Content", target: "_blank" },
      ],
    });

    isc.DynamicForm.create({
      ID: "textSearchForm",
      margin: 2,
      width: 550,
      height: 40,
      numCols: 3,
      colWidths: [250, "*", 100],
      items: [
        {
          name: "query",
          title: "Type and press search or enter",
          type: "text",
          keyPress: (item, form, keyName) => {
            if (keyName === "Enter" && form.validate(false)) {
              item.form.doSearch();
            }
          },
          required: true,
          validators: [
            {
              clientOnly: true,
              type: "lengthRange",
              min: 4,
              errorMessage: "At least 4 characters are required for a search",
            },
          ],
        },
        {
          name: "searchBtn",
          startRow: false,
          title: "Search",
          type: "button",
          accessKey: "Enter",
          click() {
            this.form.doSearch();
          },
        },
        { name: "message", type: "blurb", startRow: true, colSpan: 3 },
      ],
      doSearch() {
        if (this.validate()) {
          const message = this.getItem("message");
          message.setValue("");
          textSearchResults.filterData(textSearchForm.getValuesAsCriteria());
        }
      },
    });

    isc.ListGrid.create({
      ID: "textSearchResults",
      width: "100%",
      height: "100%",
      alternateRecordStyles: true,
      dataSource: "textSearch",
      dataFetchMode: "basic", // no paging
      wrapCells: true,
      fixedRecordHeights: false,
      canEdit: false,
      canFreezeFields: false,
      canGroupBy: false,
      canPickFields: false,
      canSort: false,
      fields: [
        { name: "icon", align: "center", width: 30 },
        { name: "doc", width: "15%" },
        { name: "bizKey", width: "30%" },
        { name: "excerpt", width: "55%" },
        { name: "score", width: 75 },
        { name: "data", width: 40, align: "center", linkText: "Data" },
        { name: "content", width: 60, align: "center", linkText: "Content" },
      ],
      dataProperties: {
        transformData(newData, dsResponse) {
          if (dsResponse.status >= 0) {
            const summary = newData.pop();
            const message = textSearchForm.getItem("message");
            let response = `Query took ${summary.time} seconds.`;
            if (summary.suggestion) {
              response += `  Did you mean <a href="#" onclick="var q = textSearchForm.getItem('query'); q.setValue('${summary.suggestion}'); q.focusInItem(); return false;">${summary.suggestion}</a>`;
            }
            message.setValue(response);
          }
        },
      },
    });
  },

  /**
   * Pops up the search window.
   */
  popupSearch() {
    isc.WindowStack.popup(null, "Text Search", true, [
      textSearchForm,
      textSearchResults,
    ]);
    textSearchForm.focusInItem("query");
    textSearchForm.clearValues();
  },

  /**
   * Shows the portal view.
   */
  showPortal() {
    const details = isc.Canvas.getById("details");

    if (isc.BizUtil._currentView !== isc.ListView.contents) {
      if (isc.BizUtil._currentView) {
        if (details.hasMember(isc.BizUtil._currentView)) {
          details.hideMember(isc.BizUtil._currentView);
        }
        isc.BizUtil.relinquishEditView(isc.BizUtil._currentView);
      }
      isc.BizUtil._currentView = isc.ListView.contents;
      details.showMember(isc.ListView.contents);
    }

    isc.ListView.showPortal();
  },

  /**
   * Shows the help window.
   * @param {string} url - The URL to the help documentation
   */
  showHelp(url) {
    const helpUrl = url || "https://skyvers.github.io/skyve-user-guide/";
    isc.BizUtil.popupFrame(helpUrl, "Skyve Help", 1024, 768);
  },
});
