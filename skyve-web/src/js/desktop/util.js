/**
 * Override the `isOffline` method to ensure saving works correctly in Chrome when offline.
 * @returns {boolean} - Always returns `false`.
 */
isc.Offline.isOffline = () => false;

isc.setAutoDraw(false);
isc.RPCManager.fetchDataPrompt = "Contacting Server...";
isc.RPCManager.saveDataPrompt = "Contacting Server...";
isc.RPCManager.removeDataPrompt = "Contacting Server...";

/**
 * Handles RPC errors.
 * @param {Object} response - The RPC response object.
 * @param {Object} request - The RPC request object.
 * @returns {boolean} - Returns `false` if the error is handled, otherwise delegates to the superclass.
 */
isc.RPCManager.handleError = function (response, request) {
  if (typeof response.data === "string") {
    isc.warn(response.data);
    return false;
  }
  return this.Super("handleError", arguments);
};

isc.RPCManager.addClassMethods({
  /**
   * Redirects to the login page.
   * @param {number} transactionNum - The transaction number.
   * @param {Object} rpcRequest - The RPC request object.
   * @param {Object} rpcResponse - The RPC response object.
   */
  loginRequired(transactionNum, rpcRequest, rpcResponse) {
    window.location.assign(SKYVE.Util.CONTEXT_URL);
  },
});

Date.setShortDisplayFormat("toEuropeanShortDate");
Date.setNormalDisplayFormat("toEuropeanShortDate");
Date.setInputFormat("DMY");

/**
 * Extends ListGrid to provide custom filter editor types.
 */
isc.ListGrid.addProperties({
  /**
   * Determines the filter editor type for a given field.
   * @param {Object} field - The field configuration object.
   * @returns {string} - The filter editor type.
   */
  getFilterEditorType(field) {
    if (field.filterEditorType !== null) return field.filterEditorType;

    const isFileType =
      field.type === this._$binary ||
      field.type === this._$file ||
      field.type === this._$imageFile;

    if (isFileType && field.editorType === null) {
      const ds = this.getDataSource();
      if (
        field.filenameSuppressed ||
        (ds && ds.getFilenameField && ds.getFilenameField(field.name) === null)
      ) {
        return "StaticTextItem";
      } else {
        return "TextItem";
      }
    }

    const filterEditorConfig = {
      ...field,
      canEdit: field.canFilter !== false,
      length: null,
    };
    if (filterEditorConfig._constructor !== null)
      delete filterEditorConfig._constructor;
    if (field.filterEditorType !== null)
      filterEditorConfig.editorType = field.filterEditorType;
    Object.assign(filterEditorConfig, field.filterEditorProperties);
    return isc.DynamicForm.getEditorType(filterEditorConfig, this);
  },
});

/**
 * Extends ResultSet to handle spatial operators.
 */
isc.ResultSet.addMethods({
  skyveSetCriteria: isc.ResultSet.getPrototype().setCriteria,

  /**
   * Sets criteria and forces a fetch for spatial operators.
   * @param {Object} newCriteria - The new criteria to set.
   * @returns {Object} - The result of the original `setCriteria` method.
   */
  setCriteria(newCriteria) {
    const result = this.skyveSetCriteria(newCriteria);
    if (
      newCriteria &&
      JSON.stringify(newCriteria).match(/"operator"\s*:\s*"geo/)
    ) {
      this.invalidateCache();
    }
    return result;
  },
});

let resizeTimerEvent = null;

/**
 * Handles page resize events to resize the window stack.
 */
isc.Page.setEvent("resize", () => {
  if (isc.WindowStack) {
    if (resizeTimerEvent) {
      isc.Timer.clearTimeout(resizeTimerEvent);
    }
    resizeTimerEvent = isc.Timer.setTimeout(isc.WindowStack.resize, 50);
  }
});

/**
 * Adds search operators for spatial queries.
 */
const spatialOperators = [
  {
    ID: "geoEquals",
    title: "Equals",
    fieldTypes: ["geometry"],
    valueType: "fieldType",
    requiresServer: true,
  },
  {
    ID: "geoDisjoint",
    title: "Disjoint",
    fieldTypes: ["geometry"],
    valueType: "fieldType",
    requiresServer: true,
  },
  {
    ID: "geoIntersects",
    title: "Intersects",
    fieldTypes: ["geometry"],
    valueType: "fieldType",
    requiresServer: true,
  },
  {
    ID: "geoTouches",
    title: "Touches",
    fieldTypes: ["geometry"],
    valueType: "fieldType",
    requiresServer: true,
  },
  {
    ID: "geoCrosses",
    title: "Crosses",
    fieldTypes: ["geometry"],
    valueType: "fieldType",
    requiresServer: true,
  },
  {
    ID: "geoWithin",
    title: "Within",
    fieldTypes: ["geometry"],
    valueType: "fieldType",
    requiresServer: true,
  },
  {
    ID: "geoContains",
    title: "Contains",
    fieldTypes: ["geometry"],
    valueType: "fieldType",
    requiresServer: true,
  },
  {
    ID: "geoOverlaps",
    title: "Overlaps",
    fieldTypes: ["geometry"],
    valueType: "fieldType",
    requiresServer: true,
  },
];

spatialOperators.forEach((operator) => {
  isc.DataSource.addSearchOperator({
    ...operator,
    condition: () => true,
    compareCriteria: () => -1,
  });
});

/**
 * Utility class for business-related operations.
 */
isc.defineClass("BizUtil");

isc.BizUtil.addClassProperties({
  _modules: {}, // Map of modules loaded -> views -> used & unused views
  _unusedPickLists: [], // List of picklists to use (for pick views)

  // Data source for the "previous values" mechanism on text fields
  COMPLETE_DATA_SOURCE: isc.RestDataSource.create({
    dataFormat: "json",
    jsonPrefix: "",
    jsonSuffix: "",
    dataURL: "smartcomplete",
    fields: [{ name: "value", type: "text" }],
  }),
});

isc.BizUtil.addClassMethods({
  _currentView: null, // The view currently displayed

  /**
   * Gets the current view.
   * @returns {Object} - The current view.
   */
  getCurrentView() {
    return isc.BizUtil._currentView;
  },

  /**
   * Checks if a field type is numeric.
   * @param {string} type - The field type.
   * @returns {boolean} - True if the field type is numeric.
   */
  isNumeric(type) {
    const numericTypes = [
      "integer",
      "float",
      "bizDecimal0",
      "bizDecimal1",
      "bizDecimal10",
      "bizDecimal2",
      "bizDecimal5",
      "bizDollarsAndCents",
      "bizIntegerPercentage",
      "bizIntegerSeparator",
      "bizTwoDecimalPlacesPercentage",
      "bizTimeDuration",
    ];
    return numericTypes.includes(type);
  },

  /**
   * Checks if a field type is temporal.
   * @param {string} type - The field type.
   * @returns {boolean} - True if the field type is temporal.
   */
  isTemporal(type) {
    const temporalTypes = [
      "date",
      "time",
      "datetime",
      "bizDate",
      "bizTime",
      "DD_MM",
      "MM_DD",
      "MMM_DD",
      "YYYY_MM",
      "HH_MI",
      "HH24_MI",
    ];
    return temporalTypes.some((temporalType) => type.startsWith(temporalType));
  },

  /**
   * Adds filter request parameters.
   * @param {Object} requestParams - The request parameters to populate.
   * @param {Array} filterParams - The filter parameters.
   * @param {Object} view - The associated view.
   */
  addFilterRequestParams(requestParams, filterParams, view) {
    const instance = view.gather(false);
    filterParams.forEach((filterParam) => {
      const value = view.toDisplay(filterParam.value, instance);
      requestParams[filterParam.name] = value;
    });
  },

  /**
   * Converts filter criteria by renaming `_display_*` fields.
   * @param {Object} criteria - The criteria to convert.
   */
  convertFilterCriteria(criteria) {
    if (criteria) {
      if (criteria.criteria) {
        criteria.criteria.forEach((criterion) =>
          isc.BizUtil.convertFilterCriteria(criterion)
        );
      } else if (
        criteria.fieldName &&
        criteria.fieldName.startsWith("_display_")
      ) {
        criteria.fieldName = criteria.fieldName.substring(9);
      } else {
        Object.keys(criteria).forEach((propertyName) => {
          if (propertyName.startsWith("_display_")) {
            criteria[propertyName.substring(9)] = criteria[propertyName];
            delete criteria[propertyName];
          }
        });
      }
    }
  },

  /**
   * Completes filter criteria by adding extra criteria from filterParams.
   * @param {Object} criteria - The base criteria.
   * @param {Array} filterParams - The filter parameters.
   * @param {Object} view - The associated view.
   * @returns {Object} - The completed criteria.
   */
  completeFilterCriteria(criteria, filterParams, view) {
    let result = { ...criteria };
    if (!result.operator) {
      result = isc.DataSource.convertCriteria(result, "substring");
    }
    result = {
      _constructor: "AdvancedCriteria",
      operator: "and",
      criteria: [result],
    };

    const instance = view.gather(false);
    filterParams.forEach((filterParam) => {
      const value = view.toDisplay(filterParam.value, instance);
      result.criteria.push({
        fieldName: filterParam.name,
        operator: filterParam.operator,
        value,
      });
    });

    return result;
  },

  /**
   * Creates an image button.
   * @param {string} icon - The icon path.
   * @param {boolean} hasDisabledIcon - Whether the button has a disabled icon.
   * @param {string} tooltip - The button tooltip.
   * @param {Function} click - The click handler.
   * @returns {Object} - The created button.
   */
  createImageButton(icon, hasDisabledIcon, tooltip, click) {
    return isc.ToolStripButton.create({
      icon,
      iconAlign: "center",
      showDisabledIcon: hasDisabledIcon,
      showDownIcon: false,
      canHover: true,
      getHoverHTML: () => tooltip,
      click,
    });
  },

  /**
   * Creates a CodeMirror editor for syntax highlighting.
   * @param {string} scVar - The smart client variable.
   * @param {string} binding - The binding for the text area.
   * @param {string} languageMimeType - The language mime type for syntax highlighting.
   *
   * @see https://codemirror.net/LICENSE
   */
  createCodeMirror(scVar, binding, languageMimeType) {
    const loadResources = async () => {
      await SKYVE.Util.loadCSS(`codemirror/codemirror.css?v=${SKYVE.Util.v}`);
      await SKYVE.Util.loadCSS(`codemirror/base16-dark.css?v=${SKYVE.Util.v}`);
      await SKYVE.Util.loadJS(`codemirror/codemirror.js?v=${SKYVE.Util.v}`);
      await SKYVE.Util.loadJS(`codemirror/css/css.js?v=${SKYVE.Util.v}`);
      await SKYVE.Util.loadJS(
        `codemirror/htmlmixed/htmlmixed.js?v=${SKYVE.Util.v}`
      );
      await SKYVE.Util.loadJS(`codemirror/sql/sql.js?v=${SKYVE.Util.v}`);
      await SKYVE.Util.loadJS(`codemirror/xml/xml.js?v=${SKYVE.Util.v}`);

      const templates = document.getElementsByName(binding);
      const templateElement =
        templates &&
        templates.length > 0 &&
        templates[0].nodeName.toLowerCase() === "textarea"
          ? templates[0]
          : undefined;

      if (templateElement) {
        templateElement.setAttribute("autocapitalize", "off");
        templateElement.setAttribute("spellcheck", "false");

        const editor = CodeMirror.fromTextArea(templateElement, {
          mode: languageMimeType,
          lineNumbers: true,
          theme: "base16-dark",
        });
        editor.setSize(600, null);
        editor.on("change", () => editor.save());
      }
    };

    loadResources();
  },

  /**
   * Creates a split button.
   * @param {string} buttonTitle - The title of the main button.
   * @param {string} buttonIcon - The icon for the main button.
   * @param {boolean} buttonHasDisabledIcon - Whether the main button has a disabled icon.
   * @param {string} buttonTooltip - The tooltip for the main button.
   * @param {Function} buttonClick - The click handler for the main button.
   * @param {string} splitTooltip - The tooltip for the split button.
   * @param {Object} splitTarget - The target for the split button.
   * @param {Array} splitItems - The menu items for the split button.
   * @returns {Object} - The created split button.
   */
  createSplitButton(
    buttonTitle,
    buttonIcon,
    buttonHasDisabledIcon,
    buttonTooltip,
    buttonClick,
    splitTooltip,
    splitTarget,
    splitItems
  ) {
    return isc.HLayout.create({
      align: "right",
      height: 1,
      membersMargin: 1,
      members: [
        isc.IButton.create({
          autoFit: true,
          title: buttonTitle,
          icon: buttonIcon,
          showDisabledIcon: buttonHasDisabledIcon,
          canHover: true,
          getHoverHTML: () => buttonTooltip,
          click: buttonClick,
        }),
        isc.MenuButton.create({
          title: null,
          width: 26,
          alignMenuLeft: false,
          canHover: true,
          getHoverHTML: () => splitTooltip,
          menu: isc.Menu.create({
            autoDraw: false,
            showShadow: true,
            shadowDepth: 10,
            target: splitTarget,
            data: splitItems,
          }),
        }),
      ],
    });
  },

  /**
   * Creates an upload button.
   * @param {Object} contentFormItem - The form item for the upload.
   * @param {boolean} image - Whether the upload is for an image.
   * @param {boolean} showMarkup - Whether markup is enabled.
   * @returns {Object} - The created upload button.
   */
  createUploadButton(contentFormItem, image, showMarkup) {
    const menu = [
      {
        title: "Clear",
        icon: "icons/delete.png",
        click: () => contentFormItem.setValue(null),
        enableIf: () => contentFormItem.getValue() !== null,
      },
    ];

    if (showMarkup) {
      menu.push({
        title: "Mark Up",
        icon: "icons/edit.png",
        click: () => {
          const instance = contentFormItem.form._view.gather(false);
          let url = `imageMarkup.xhtml?_n=${contentFormItem.name.replaceAll(
            "_",
            "."
          )}&_c=${instance._c}&_id=${contentFormItem.getValue()}`;
          if (contentFormItem.form._view._b) {
            url += `&_b=${contentFormItem.form._view._b.replaceAll("_", ".")}`;
          }
          isc.WindowStack.popup(null, "Mark Up Image", true, [
            isc.HTMLPane.create({
              contentsType: "page",
              contents: "Loading Page...",
              contentsURL: url,
            }),
          ]);
        },
        enableIf: () => contentFormItem.getValue() !== null,
      });
    }

    return isc.BizUtil.createSplitButton(
      "Upload",
      null,
      false,
      "Upload content",
      () => {
        const instance = contentFormItem.form._view.gather(false);
        let url = `${
          image ? "image" : "content"
        }Upload.xhtml?_n=${contentFormItem.name.replaceAll("_", ".")}&_c=${
          instance._c
        }`;
        if (contentFormItem.form._view._b) {
          url += `&_b=${contentFormItem.form._view._b.replaceAll("_", ".")}`;
        }
        isc.WindowStack.popup(
          null,
          image ? "Upload Image" : "Upload Content",
          true,
          [
            isc.HTMLPane.create({
              contentsType: "page",
              contents: "Loading Page...",
              contentsURL: url,
            }),
          ]
        );
      },
      "Other Options",
      null,
      menu
    );
  },

  /**
   * Gets an edit view for a module and document.
   * @param {string} moduleName - The module name.
   * @param {string} documentName - The document name.
   * @param {Function} onViewCreated - The callback when the view is created.
   */
  getEditView(moduleName, documentName, onViewCreated) {
    if (!isc.BizUtil._modules[moduleName]) {
      isc.BizUtil._modules[moduleName] = {};
      window[moduleName] = {};
    }

    const documentEntry = isc.BizUtil._modules[moduleName][documentName];
    if (documentEntry) {
      const view =
        documentEntry._unused.pop() ||
        eval(`${moduleName}.create${documentName}()`);
      view._moduleName = moduleName;
      view._documentName = documentName;
      documentEntry._used.push(view);
      onViewCreated(view);
    } else {
      isc.RPCManager.sendRequest({
        showPrompt: true,
        evalResult: true,
        httpMethod: "GET",
        actionURL: `${SKYVE.Util.CONTEXT_URL}smartgen?_mod=${moduleName}&_doc=${documentName}`,
        callback: () => {
          isc.BizUtil._modules[moduleName][documentName] = {
            _used: [],
            _unused: [],
          };
          isc.BizUtil.getEditView(moduleName, documentName, onViewCreated);
        },
      });
    }
  },

  /**
   * Relinquishes an edit view.
   * @param {Object} view - The view to relinquish.
   */
  relinquishEditView(view) {
    const documentEntry =
      isc.BizUtil._modules[view._moduleName][view._documentName];
    documentEntry._used.remove(view);
    documentEntry._unused.push(view);
  },

  /**
   * Gets a pick list.
   * @param {Object} lookupDescription - The lookup description.
   * @param {Array} filterParams - The filter parameters.
   * @param {Object} view - The associated view.
   * @returns {Object} - The pick list.
   */
  getPickList(lookupDescription, filterParams, view) {
    const result =
      isc.BizUtil._unusedPickLists.pop() ||
      isc.BizListGrid.create({ isPickList: true });
    result.setLookup(lookupDescription, filterParams, view);
    return result;
  },

  /**
   * Relinquishes a pick list.
   * TODO - Why arent the fields defined when I used a cached BizListGrid?
   * @param {Object} pickList - The pick list to relinquish.
   */
  relinquishPickList(pickList) {
    pickList.destroy();
  },

  /**
   * Creates a list grid.
   * @returns {Object} - The created list grid.
   */
  createListGrid() {
    return isc.BizListGrid.create({ margin: 2 });
  },

  /**
   * Creates a calendar.
   * @returns {Object} - The created calendar.
   */
  createCalendar() {
    return isc.Calendar.create({
      width: "100%",
      height: "100%",
      scrollToWorkDay: true,
      data: [],
    });
  },

  /**
   * Creates a tree grid.
   * @returns {Object} - The created tree grid.
   */
  createTreeGrid() {
    return isc.BizListGrid.create({ margin: 2, isTree: true });
  },

  /**
   * Creates a map.
   * @returns {Object} - The created map.
   */
  createMap() {
    return isc.BizMap.create();
  },

  /**
   * Opens a popup frame.
   * @param {string} url - The URL to open.
   * @param {string} name - The name of the popup.
   * @param {number} width - The width of the popup.
   * @param {number} height - The height of the popup.
   */
  popupFrame(url, name, width, height) {
    const win = window.open(
      url,
      name,
      `width=${width},height=${height},resizable=yes,scrollbars=no,toolbar=no,location=no,directories=no,status=yes,menubar=no,copyhistory=no`
    );
    win.focus();
  },

  /**
   * Displays growl notifications.
   * @param {Array} msgs - The messages to display.
   * @param {number} life - The duration of the notification.
   * @param {boolean} sticky - Whether the notification is sticky.
   */
  growl(msgs, life, sticky) {
    PrimeFaces.cw("Growl", "growl", {
      id: "growl",
      widgetVar: "growl",
      life: life || 6000,
      sticky: sticky || false,
      msgs,
    });
  },

  /**
   * Handles push messages.
   * @param {Array} pushMessage - The push messages to handle.
   */
  onPushMessage(pushMessage) {
    const growls = [];
    const messages = [];
    let warn = false;

    pushMessage.forEach((m) => {
      if (m.type === "g") {
        growls.push({ severity: m.severity, summary: m.message });
      } else if (m.type === "m") {
        if (m.severity !== "info") warn = true;
        messages.push(m.message);
      } else if (m.type === "r") {
        const view = isc.BizUtil.getCurrentView();
        if (view && view.rerender) view.rerender();
      } else if (m.type === "j") {
        window[m.method](m.argument);
      }
    });

    if (growls.length > 0) isc.BizUtil.growl(growls);
    if (messages.length > 0) {
      const markup =
        messages.length > 1
          ? `<ul>${messages.map((msg) => `<li>${msg}</li>`).join("")}</ul>`
          : messages[0];
      if (warn) isc.warn(markup);
      else isc.say(markup);
    }
  },
});
