/**
 * Implements the BizContainer UI component.
 * Extends VLayout from the SmartClient library.
 */
isc.ClassFactory.defineClass("BizContainer", "VLayout");

isc.BizContainer.addMethods({
	initWidget: function () {
		this.backgroundImage = "background.png";
		this.backgroundRepeat = "repeat";
		this.Super("initWidget", arguments);
		this.contained = [];
	},

	/**
	 * Adds a widget to the container.
	 * @param {isc.Widget} contained - the widget to be contained.
	 */
	addContained: function (contained) {
		this.contained.add(contained);
		this.addMember(contained);
	},

	/**
	 * Removes a widget from the container.
	 * @param {isc.Widget} contained - the widget to remove.
	 */
	removeContained: function (contained) {
		this.contained.remove(contained);
		this.removeMember(contained);
	},
});

/**
 * Implements the EditView UI component.
 * Extends BizContainer implemented above.
 */
isc.ClassFactory.defineClass("EditView", "BizContainer");

isc.EditView.addClassProperties({
	_DATA_SOURCE: isc.RestDataSource.create({
		dataFormat: "json",
		jsonPrefix: "",
		jsonSuffix: "",
		dataURL: "smartedit",
		sendExtraFields: false, // Ensure that only datasource defined fields are sent
	}),
});

isc.EditView.addMethods({
	initWidget: function () {
		this.overflow = "hidden";
		this.membersMargin = 2;
		this.layoutMargin = 2;
		this.margin = 2;
		this._grids = {}; // Map of binding -> (map of ID -> dataGrid/comparisonEditor/listMembership/map widget)
		this._refreshedGrids = {}; // Map of dataGrid/comparisonEditor/listMembership ID -> boolean (true if refreshed)
		this.Super("initWidget", arguments);

		this._heading = isc.HTMLFlow.create();
		this.addMember(this._heading);

		// The action panel
		this._actionPanel = isc.ToolStrip.create({
			name: "actionPanel",
			layoutMargin: 2,
			membersMargin: 5,
			width: "100%",
		});
		this.addMember(this._actionPanel);

		// The edit panel
		this._editPanel = isc.BizContainer.create({
			layoutMargin: 2,
			membersMargin: 10,
			overflow: "auto",
		});
		this.addMember(this._editPanel);

		// Initialize the values manager
		this._vm = isc.ValuesManager.create({
			dataSource: isc.EditView._DATA_SOURCE,
			handleHiddenValidationErrors: (errors) => {
				const messages = [];
				for (const binding in errors) {
					const message = errors[binding];
					const tokens = binding.split("__");
					if (tokens.length === 2) {
						const lastUnderscore = tokens[0].lastIndexOf("_");
						if (lastUnderscore > -1) {
							const gridBinding = tokens[0].substring(0, lastUnderscore);
							const grids = this._grids[gridBinding];
							if (grids) {
								const rowNum = parseInt(
									tokens[0].substring(lastUnderscore + 1),
								);
								if (isc.isA.Number(rowNum)) {
									for (const gridID in grids) {
										const grid = grids[gridID];
										if (grid?.grid) {
											grid.grid.setFieldError(rowNum, tokens[1], message);
											grid.grid.markForRedraw();
										}
									}
								}
							}
						}
					}

					if (!messages.contains(message)) {
						messages.add(message);
					}
				}

				if (messages.length > 0) {
					let warning = "<ul>";
					for (const message of messages) {
						warning += `<li>${message}</li>`;
					}
					warning += "</ul>";
					isc.warn(warning, null, { title: "Problems" });
				}

				return false; // Don't log to console
			},

			// Overload to remove oldValues from the request context
			saveEditorValues: function (values, saveOperation, callback, context) {
				context.oldValues = null;
				this.Super("saveEditorValues", arguments);
			},
		});
	},

	/**
	 * Adds a widget to the edit panel.
	 * @param {isc.Widget} contained - the widget to be contained.
	 */
	addContained: function (contained) {
		this._editPanel.addContained(contained);
	},

	/**
	 * Adds an edit action to the action panel.
	 * @param {isc.IButton} button - the button to add.
	 */
	addEditAction: function (button) {
		button.forCreate = false;
		this._actionPanel.addMember(button);
	},

	/**
	 * Adds a create action to the action panel.
	 * @param {isc.IButton} button - the button to add.
	 */
	addCreateAction: function (button) {
		button.forCreate = true;
		this._actionPanel.addMember(button);
	},

	/**
	 * Adds an action to the action panel.
	 * @param {isc.IButton} button - the button to add.
	 */
	addAction: function (button) {
		button.forCreate = null;
		this._actionPanel.addMember(button);
	},

	/**
	 * Replaces {bindings} in a string expression with values from the instance.
	 * @param {string} expression - the expression to parse.
	 * @param {Object} instance - the bean instance to get values from.
	 * @returns {string} - the parsed expression.
	 */
	toDisplay: function (expression, instance) {
		if (expression) {
			const tokens = expression.match(/\{[A-Za-z0-9._]+\}/g);
			if (tokens) {
				for (const token of tokens) {
					const binding = token.substring(1, token.length - 1);
					let evaluation = binding
						.split(".")
						.reduce((obj, key) => obj?.[key], instance);
					if (typeof evaluation === "undefined") {
						evaluation = "";
					} else if (
						evaluation == null ||
						evaluation === "null" ||
						evaluation === "undefined"
					) {
						evaluation = "";
					} else if (evaluation.toDateStamp) {
						evaluation = evaluation.toDateStamp();
						evaluation = `${evaluation.substring(0, 4)}-${evaluation.substring(
							4,
							6,
						)}-${evaluation.substring(6, 11)}:${evaluation.substring(
							11,
							13,
						)}:${evaluation.substring(13, 15)}.000`;
					} else if (evaluation.toString) {
						evaluation = evaluation.toString();
					}
					expression = expression.replace(token, evaluation);
				}
			}
		}

		return expression;
	},

	/**
	 * Re-renders the edit view.
	 */
	rerender: function () {
		const instance = this.gather(false); // No validation
		if (instance) {
			this._editInstance(
				"ZoomOut",
				instance.bizId,
				this._b,
				instance._c,
				this._openedFromDataGrid,
			);
		}
	},

	/**
	 * Resumes controls that get paused when they lose focus.
	 */
	resume: function () {
		for (const gridBinding in this._grids) {
			const grids = this._grids[gridBinding];
			for (const gridID in grids) {
				const grid = grids[gridID];
				if (grid.webmap) {
					grid.resume();
				}
			}
		}
	},

	/**
	 * Creates a new instance.
	 * @param {Object} newParams - parameters to seed the new instance.
	 * @param {string} formBinding - the binding of the datagrid or lookup.
	 * @param {string} parentContext - the parent web context.
	 * @param {boolean} openedFromDataGrid - whether the view was opened from a data grid row.
	 * @param {Function} successCallback - callback function on success.
	 */
	newInstance: function (
		newParams,
		formBinding,
		parentContext,
		openedFromDataGrid,
		successCallback,
	) {
		this._openedFromDataGrid = openedFromDataGrid;
		if (this._vm.members) {
			this.hide();
			this._b = formBinding;

			const params = {
				_mod: this._mod,
				_doc: this._doc,
				_ecnt: this._ecnt,
				_ccnt: this._ccnt,
			};
			if (this._csrf) {
				params._csrf = this._csrf;
			}
			if (formBinding) {
				params._b = formBinding;
			}
			if (parentContext) {
				params._c = parentContext;
			}
			if (newParams) {
				for (const binding in newParams) {
					params[binding] = newParams[binding];
				}
			}

			this._vm.fetchData(
				null, // No criteria required
				(dsResponse, data) => {
					let values = {};
					if (dsResponse.status >= 0) {
						this._csrf = dsResponse.httpHeaders["x-csrf-token"];
						this._vm.setSaveOperationType("add");
						values = data[0];
						this.scatter(values);

						if (openedFromDataGrid) {
							this._b += `ElementById(${values.bizId})`;
						}

						if (successCallback) {
							successCallback(data);
						}
					} else if (dsResponse.status === -1) {
						isc.warn(data, null, { title: "Problems" });
					}

					this.show();
					this.refreshListGrids(true, true, values);

					if (this.opened) {
						this.opened(data);
					}
				},
				{ httpMethod: "POST", params, willHandleError: true },
			);
		}
	},

	/**
	 * Edits an instance.
	 * @param {string} bizId - the ID of the bean to edit.
	 * @param {string} formBinding - the binding of the datagrid or lookup.
	 * @param {string} parentContext - the parent context.
	 * @param {boolean} openedFromDataGrid - whether the view was opened from a data grid row.
	 * @param {Function} successCallback - callback function on success.
	 */
	editInstance: function (
		bizId,
		formBinding,
		parentContext,
		openedFromDataGrid,
		successCallback,
	) {
		this._saved = false;
		this._editInstance(
			null,
			bizId,
			formBinding,
			parentContext,
			openedFromDataGrid,
			successCallback,
		);
	},

	/**
	 * Handles editing an instance of a bean, including fetching data, scattering values, and refreshing the UI.
	 * @param {string} action - the action name associated with this edit call (e.g., "ZoomOut").
	 * @param {string} bizId - the ID of the bean to edit.
	 * @param {string} formBinding - the binding of the data grid or lookup (can be null).
	 * @param {string} parentContext - the parent context (can be null).
	 * @param {boolean} openedFromDataGrid - indicates whether the view was opened from a data grid row.
	 * @param {Function} successCallback - a function to call when the operation is successful.
	 */
	_editInstance: function (
		action,
		bizId,
		formBinding,
		parentContext,
		openedFromDataGrid,
		successCallback,
	) {
		this._openedFromDataGrid = openedFromDataGrid;

		// Skip if the view model has no members (not data-bound)
		if (!this._vm.members) {
			this._source = null;
			return;
		}

		// Hide the view while fetching data
		this.hide();
		this._b = formBinding;

		// Prepare request parameters
		const params = {
			bizId,
			_mod: this._mod,
			_doc: this._doc,
			_ecnt: this._ecnt,
			_ccnt: this._ccnt,
		};

		// Add optional parameters if they exist
		if (this._csrf) params._csrf = this._csrf;
		if (action) params._a = action;
		if (formBinding) params._b = formBinding;
		if (parentContext) params._c = parentContext;
		if (this._source) {
			params._s = this._source;
			this._source = null;
		}

		// Fetch data from the server
		this._vm.fetchData(
			null, // No criteria required
			(dsResponse, data, dsRequest) => {
				let values = {};
				if (dsResponse.status >= 0) {
					// Success: Extract CSRF token and scatter values
					this._csrf = dsResponse.httpHeaders["x-csrf-token"];
					values = data[0];

					// Update form binding if opened from a data grid
					if (openedFromDataGrid && this._b && !this._b.endsWith(")")) {
						this._b += `ElementById(${values.bizId})`;
					}

					// Scatter the fetched values into the form
					this.scatter(values);

					// Execute the success callback if provided
					if (successCallback) {
						successCallback(data);
					}
				} else if (dsResponse.status === -1) {
					// Display a warning if there are issues
					isc.warn(data, null, { title: "Problems" });
				}

				// Handle ZoomOut or new document scenarios
				if (action === "ZoomOut" || !bizId) {
					const changes = this._vm.valuesHaveChanged();
					this._vm.setValue("_apply", true);
					if (!changes) {
						this._vm.rememberValues();
					}
				}

				// Show the view and refresh UI components
				this.show();
				this.refreshListGrids(true, !action, values);

				// Trigger the `opened` callback if defined
				if (this.opened) {
					this.opened(data);
				}
			},
			{
				httpMethod: "POST",
				params,
				willHandleError: true,
			},
		);
	},

	/**
	 * Re-renders the edit view following an action.
	 */
	rerenderAction: function (validate, source) {
		this._source = source;
		this.saveInstance(validate, null);
		this._saved = true;
	},

	/**
	 * Re-renders the edit view following a blur event.
	 */
	rerenderBlurryAction: function (validate, source) {
		this.delayCall("_rerenderBlurryAction", [validate, source], 100);
	},

	/**
	 * Re-renders the edit view following a blur action.
	 */
	_rerenderBlurryAction: function (validate, source) {
		this._source = source;
		this.saveInstance(validate, null, () => {
			if (this._blurry) {
				const blurry = this._blurry;
				this._blurry = null;

				// If it's a BizButton, call its action as this was the lost click event
				if (blurry.action) {
					blurry.delayCall("action");
				} else if (blurry.click) {
					blurry.click();
				}
			}
		});
		this._saved = true;
	},

	/**
	 * Saves the instance.
	 * @param {boolean} validate - whether to validate.
	 * @param {string} action - the action name.
	 * @param {Function} successCallback - callback function on success.
	 */
	saveInstance: function (validate, action, successCallback) {
		const instance = this.gather(validate);
		if (instance) {
			const context = instance._c;
			delete instance._c;

			const params = {
				_mod: this._mod,
				_doc: this._doc,
				_ecnt: this._ecnt,
				_ccnt: this._ccnt,
				bean: instance,
				bizId: instance.bizId,
				_c: context,
			};
			if (this._csrf) {
				params._csrf = this._csrf;
			}
			if (action) {
				params._a = action;
				if (action === "_PUSH") {
					action = null;
				}
			}
			if (this._b) {
				params._b = this._b;
			}
			if (this._source) {
				params._s = this._source;
				this._source = null;
			}

			this._vm.disableValidation = true;
			this._vm.saveData(
				(dsResponse, data) => {
					if (dsResponse.status >= 0) {
						this._csrf = dsResponse.httpHeaders["x-csrf-token"];
						if (action === "ZoomOut") {
							const opener = isc.WindowStack.getOpener();
							const openerValues = opener.gather(false);
							openerValues._c = data._c;

							const childBinding = this._b;
							const index = childBinding.lastIndexOf(".");
							const parentBinding = opener._b;

							if (childBinding.endsWith(")")) {
								const lastIndex = childBinding.lastIndexOf("ElementById");
								if (lastIndex >= 0) {
									const gridBinding = childBinding.substring(0, lastIndex);
									const openerValue = openerValues[gridBinding];
									if (isc.isAn.Array(openerValue)) {
										isc.WindowStack.popoff(true);
										opener._source = null;
										return;
									} else {
										openerValues[gridBinding] = data.bizId;
										const lookupDescription = opener._vm.getItem(gridBinding);
										if (lookupDescription) {
											if (instance.bizId) {
												lookupDescription.bizEdited(
													lookupDescription.form,
													lookupDescription,
													data.bizId,
												);
											} else {
												lookupDescription.bizAdded(
													lookupDescription.form,
													lookupDescription,
													data.bizId,
												);
											}
										}
									}
								}
							}

							opener._vm.setValues(openerValues);
							delete data._c;
							delete data._title;
						}

						if (action === "Save") {
							this._saved = true;
							this.scatter(data);
							this.refreshListGrids(true, false, data);
						} else if (action) {
							const opener = isc.WindowStack.getOpener();
							if (opener) {
								isc.WindowStack.popoff(true);
								opener._source = null;
							}
						} else {
							this.scatter(data);
							this.refreshListGrids(true, false, data);
						}

						if (successCallback) {
							successCallback(data);
						}
					} else if (dsResponse.status === -1) {
						isc.warn(data, null, { title: "Problems" });
					}
				},
				{ params, willHandleError: true },
			);
			this._vm.disableValidation = false;
		} else {
			this._source = null;
		}
	},

	/**
	 * Deletes the instance.
	 * @param {boolean} validate - whether to validate.
	 * @param {Function} successCallback - callback function on success.
	 */
	deleteInstance: function (validate, successCallback) {
		const instance = this.gather(validate);
		if (instance) {
			const context = instance._c;
			delete instance._c;

			const params = { _mod: this._mod, _doc: this._doc, _c: context };
			if (this._csrf) {
				params._csrf = this._csrf;
			}

			isc.EditView._DATA_SOURCE.removeData(
				instance,
				(dsResponse, data) => {
					if (dsResponse.status >= 0) {
						this._csrf = dsResponse.httpHeaders["x-csrf-token"];
						isc.WindowStack.popoff(true);
						if (successCallback) {
							successCallback(data);
						}
					} else if (dsResponse.status === -1) {
						isc.warn(data, null, { title: "Problems" });
					}
				},
				{ params, willHandleError: true },
			);
		}
	},

	/**
	 * Executes a specified action, optionally validating and handling grid context.
	 *
	 * @param {string} action - the name of the action to execute.
	 * @param {boolean} validate - whether to validate data before executing the action.
	 * @param {string} gridBinding - grid binding relative to the current view.
	 * @param {string} gridModule - the module name driving the grid.
	 * @param {string} gridDocument - the document name driving the grid.
	 * @param {string} gridRowBizId - the business ID of the grid row.
	 * @param {Function} successCallback - callback function for a successful operation.
	 */
	doAction: function (
		action,
		validate,
		gridBinding,
		gridModule,
		gridDocument,
		gridRowBizId,
		successCallback,
	) {
		const instance = this.gather(validate);
		if (!instance) return;

		// Extract and remove context from instance for server reconstruction
		const { _c: context, ...instanceWithoutContext } = instance;

		// Build request parameters using shorthand and default values
		const params = {
			_mod: gridModule || this._mod,
			_doc: gridDocument || this._doc,
			_ecnt: this._ecnt,
			_ccnt: this._ccnt,
			bean: instanceWithoutContext,
			bizId: gridRowBizId || instance.bizId,
			_c: context,
			...(this._csrf && { _csrf: this._csrf }),
			...(action && { _a: action }),
			...(this._b && { _b: this._b }),
			...(gridBinding && { _g: gridBinding }),
		};

		// Disable validation temporarily, as it is handled during gathering
		this._vm.disableValidation = true;

		// Save data and handle the response
		this._vm.saveData(
			(dsResponse, data) => {
				if (dsResponse.status >= 0) {
					// Update CSRF token from response header
					this._csrf = dsResponse.httpHeaders["x-csrf-token"];

					// Handle successful response
					this._saved = true;
					this.scatter(data);
					this.refreshListGrids(true, false, data);

					if (successCallback) {
						successCallback(data);
					}

					// Redirect if a download URL is provided
					if (data._redirectUrl) {
						window.location.assign(data._redirectUrl);
					}
				} else if (dsResponse.status === -1) {
					isc.warn(data, null, { title: "Problems" });
				}

				return true;
			},
			{ params, willHandleError: true },
		);

		// Re-enable validation immediately after the call
		this._vm.disableValidation = false;
	},

	/**
	 * Schedules a blurry action with a slight delay to prevent lost click events during blur.
	 * Typically triggered from a blur event to handle server-side rerender actions.
	 *
	 * @param {string} action - the name of the action to execute.
	 * @param {boolean} validate - whether validation should occur before executing the action.
	 */
	doBlurryAction: function (action, validate) {
		this.delayCall("_doBlurryAction", [action, validate], 100);
	},

	/**
	 * Executes the blurry action and re-triggers any lost click or action events.
	 *
	 * @param {string} action - the name of the action to execute.
	 * @param {boolean} validate - whether to validate before executing the action.
	 */
	_doBlurryAction: function (action, validate) {
		this.doAction(action, validate, null, null, null, null, () => {
			if (this._blurry) {
				const blurryElement = this._blurry;
				this._blurry = null;

				// If the blurred element has an action (likely a button), trigger it
				if (blurryElement.action) {
					// Delay call to ensure saveData callback has completed
					blurryElement.delayCall("action");
				} else if (blurryElement.click) {
					// Fallback for generic click events
					blurryElement.click();
				}
			}
		});
	},

	/**
	 * Scatter method for distributing values into corresponding UI components and grids.
	 *
	 * @param {Object} values - the values to be scattered into the UI components and grids.
	 * @param {boolean} values.persisted - whether the data has been persisted.
	 * @param {boolean} values.created - whether the data has been created.
	 * @param {string} values._title - the title for the header.
	 * @param {Object} values._valueMaps - a mapping of values for processing widgets.
	 * @param {Array} values._growls - growl messages to be displayed.
	 * @param {Array} values._messages - informational or warning messages to be displayed.
	 * @param {string} values.bizId - business identifier associated with the values.
	 */
	scatter: function (values) {
		// Clear any errors and reset values
		this._vm.clearErrors(true);
		this._vm.clearValues();

		// Prepare link and header template
		let link = "";
		if (values.persisted) {
			link = `<a target="_top" href="?a=e&m=${this._mod}&d=${this._doc}&i=${values.bizId}" title="Link" class="dhtmlPageButton"><i class="fa-solid fa-2x fa-thumbtack"></i></a>`;
		}

		let header = isc.BizUtil.headerTemplate;
		let icon = "";
		let help = "";

		if (values.created) {
			icon = this._createIconMarkup(this._editIcon, this._editFontIcon);
			help = this._createHelpMarkup(this._editHelpFile, this._editHelpURL);
		} else {
			icon = this._createIconMarkup(this._createIcon, this._createFontIcon);
			help = this._createHelpMarkup(this._createHelpFile, this._createHelpURL);
		}

		header = header
			.replace("{icon}", icon)
			.replace("{title}", values._title)
			.replace("{link}", link)
			.replace("{help}", help);
		this._heading.setContents(header);

		// Remove sensitive data before submitting
		const { _title, _valueMaps, _growls, _messages, ...cleanedValues } = values;

		const valueMaps = _valueMaps;
		const growls = _growls;
		const messages = _messages;

		// Process widgets
		const toRerender = [];
		this._processWidgets(
			this._editPanel,
			false,
			cleanedValues,
			valueMaps,
			toRerender,
		);

		// Set values
		this._vm.setValues(cleanedValues);

		// Rerender components
		toRerender.forEach((widget) => widget.rerender());

		// Scatter the list and membership values
		Object.entries(this._grids).forEach(([gridBinding, grids]) => {
			const data = cleanedValues[gridBinding];
			Object.entries(grids).forEach(([gridID, grid]) => {
				if (Array.isArray(data)) {
					if (grid._comparisonTree && grid._comparisonForm) {
						grid.setData(data);
					} else if (grid._candidateList && grid._memberList) {
						this._handleListMembership(data, valueMaps, gridBinding, grid);
					} else if (grid.grid) {
						this._handleDataGrid(data, valueMaps, gridBinding, grid);
					}
				}
			});
		});

		const onlyView = isc.BizUtil.getCurrentView() === this;

		// Manage action panel visibility and enable/disable controls
		this._manageActionPanel(values, onlyView);

		// Display growls and messages
		this._displayGrowlsAndMessages(growls, messages);
	},

	/**
	 * Creates an icon markup based on the provided icon file or font icon class.
	 *
	 * @param {string} iconFile - the file name for the icon image.
	 * @param {string} fontIconClass - the class name for a font icon.
	 * @returns {string} the HTML markup for the icon.
	 */
	_createIconMarkup: function (iconFile, fontIconClass) {
		if (iconFile) {
			return `<img style="width:32px;height:32px" src="resources?_doc=${this._mod}.${this._doc}&_n=${iconFile}&v=${SKYVE.Util.v}"/>`;
		} else if (fontIconClass) {
			return `<i style="padding-left:5px;font-size:28px;width:32px !important" class="titleBar bizhubFontIcon ${fontIconClass}"></i>`;
		}
		return "";
	},

	/**
	 * Creates a help markup based on the provided help file or URL.
	 *
	 * @param {string} helpFile - the help file name.
	 * @param {string} helpURL - the URL for external help.
	 * @returns {string} the HTML markup for the help link.
	 */
	_createHelpMarkup: function (helpFile, helpURL) {
		if (helpFile) {
			return `'resources?_doc=${this._mod}.${this._doc}&_n=${helpFile}&v=${SKYVE.Util.v}'`;
		} else if (helpURL) {
			return `'${helpURL}'`;
		}
		return "";
	},

	/**
	 * Handles list membership by associating candidate data with existing value maps.
	 *
	 * @param {Array} data - the data to be processed for membership.
	 * @param {Object} valueMaps - the value maps containing existing mappings.
	 * @param {string} gridBinding - the binding name of the grid.
	 * @param {Object} grid - the grid object to be updated.
	 */
	_handleListMembership: function (data, valueMaps, gridBinding, grid) {
		const candidates = [];
		const valueMap = valueMaps && valueMaps[gridBinding];
		if (valueMap) {
			Object.entries(valueMap).forEach(([key, value]) => {
				const element = data.find("bizId", key);
				if (element) {
					element.bizKey = value;
				} else {
					candidates.push({ bizId: key, bizKey: value });
				}
			});
		}
		grid.setData(candidates, data);
	},

	/**
	 * Handles data grid processing by setting up necessary fields, value maps, and selection.
	 *
	 * @param {Array} data - the data to be set in the grid.
	 * @param {Object} valueMaps - the value maps associated w
	 * @param {Object} gridBinding - the binding name of the grid.
	 * @param {Object} grid - the grid object to be updated.
	 */
	_handleDataGrid: function (data, valueMaps, gridBinding, grid) {
		const gridFields = grid.grid.fields;
		gridFields.forEach((gridField) => {
			const { type, name } = gridField;
			const isDate = type.includes("YYYY");
			const isTime = type.includes("HH");

			if (isDate || isTime) {
				this._processDateTimeFields(data, name, isDate, isTime);
			} else if (gridField.valueMap && valueMaps) {
				const valueMap = valueMaps[`${gridBinding}_${gridField.name}`];
				if (valueMap) {
					gridField.valueMap = valueMap;
				}
			}
		});

		// Handle selection from server
		if (grid.selectedIdBinding) {
			this._handleGridSelection(data, grid);
		} else {
			grid.grid.setData(data);
		}
	},

	/**
	 * Processes date and time fields in the data for proper formatting.
	 *
	 * @param {Array} data - the data to be processed.
	 * @param {string} name - the field name to be processed.
	 * @param {boolean} isDate - flag indicating if the field is a date type.
	 * @param {boolean} isTime - flag indicating if the field is a time type.
	 */
	_processDateTimeFields: function (data, name, isDate, isTime) {
		data.forEach((row) => {
			const value = row[name];
			if (value) {
				if (isDate) {
					row[name] = isc.DateUtil.parseSchemaDate(value);
				} else if (isTime) {
					row[name] = isc.Time.parseInput(value);
				}
			}
		});
	},

	/**
	 * Handles grid selection based on the data provided.
	 *
	 * @param {Array} data - the data to be set in the grid.
	 * @param {Object} grid - the grid object to be updated.
	 */
	_handleGridSelection: function (data, grid) {
		const method = grid.grid.selectionUpdated;
		try {
			grid.grid.selectionUpdated = null;
			grid.grid.setData(data);

			const selectedBizId = values[grid.selectedIdBinding];
			const index = data.findIndex("bizId", selectedBizId);
			if (index >= 0) {
				grid.grid.selectSingleRecord(index);
			} else {
				grid.grid.deselectAllRecords();
			}
		} finally {
			grid.grid.selectionUpdated = method;
		}
	},

	/**
	 * Manages the visibility and enabling/disabling of the action panel tools.
	 *
	 * @param {Object} values - the values that determine the action panel visibility.
	 * @param {boolean} onlyView - flag indicating if the current view is read-only.
	 */
	_manageActionPanel: function (values, onlyView) {
		const members = this._actionPanel.getMembers();
		if (members.length === 0) {
			this._actionPanel.hide();
		}

		members.forEach((tool) => {
			const shouldHide =
				(values.created && tool.forCreate != null && tool.forCreate) ||
				(values.notCreated && tool.forCreate != null && !tool.forCreate);

			if (shouldHide) {
				this._showHide(tool, this._actionPanel, values, true);
				return;
			}

			this._enableDisable(tool, this._actionPanel, values);

			if (tool.actionName) {
				this._handleToolVisibility(tool, values, onlyView);
			}
		});
	},

	/**
	 * Manages the visibility of action panel tools based on the current state and context.
	 *
	 * @param {Object} tool - the tool object from the action panel.
	 * @param {Object} values - the current values object containing state information (e.g., `persisted`).
	 * @param {boolean} onlyView - indicates if the current view is the only active view.
	 */
	_handleToolVisibility: function (tool, values, onlyView) {
		if (this._b) {
			this._showHide(
				tool,
				this._actionPanel,
				values,
				["OK", "Save", "Cancel", "Delete"].includes(tool.type) ||
					(tool.type === "Remove" &&
						(this._openedFromDataGrid === undefined ||
							!this._openedFromDataGrid ||
							(!tool._canDelete && values.persisted))),
			);
		} else {
			this._showHide(
				tool,
				this._actionPanel,
				values,
				["ZoomOut", "Remove"].includes(tool.type) ||
					(!values["persisted"] && tool.type === "Delete") ||
					(onlyView && ["Delete", "OK", "Cancel"].includes(tool.type)),
			);
		}
	},

	/**
	 * Displays growls and messages based on the provided data.
	 *
	 * @param {Array} growls - the growl messages to be displayed.
	 * @param {Array} messages - the informational or warning messages to be displayed.
	 */
	_displayGrowlsAndMessages: function (growls, messages) {
		if (growls) {
			isc.BizUtil.growl(growls);
		}

		if (messages) {
			const markup =
				messages.length > 1
					? `<ul>${messages.map((message) => `<li>${message.summary}</li>`).join("")}</ul>`
					: messages[0].summary;
			const warn = messages.some((message) => message.severity !== "info");
			warn ? isc.warn(markup) : isc.say(markup);
		}
	},

	/**
	 * Refreshes all visible list grids, with options for forced refresh and post-refresh conditions.
	 *
	 * @param {boolean} forceRefresh - if true, clears the list of already refreshed grids to force a full refresh.
	 * @param {boolean} forcePostRefresh - if true, forces refresh even for grids that normally skip post-refresh.
	 * @param {Object} values - the values used for evaluating grid refresh conditions.
	 */
	refreshListGrids: function (forceRefresh, forcePostRefresh, values) {
		if (forceRefresh) {
			this._refreshedGrids = {};
		}

		// Iterate through all grid bindings
		for (const gridBinding in this._grids) {
			const grids = this._grids[gridBinding];

			for (const gridID in grids) {
				const grid = grids[gridID];

				if (this._refreshedGrids[gridID]) {
					continue; // Skip already refreshed grids
				}

				if (grid.isVisible()) {
					// Only refresh visible grids

					const shouldRefresh =
						forcePostRefresh ||
						grid.postRefreshConditionName === undefined ||
						this._evaluateConditionName(grid.postRefreshConditionName, values);

					if (!shouldRefresh) {
						continue; // Skip if the condition doesn't allow refresh
					}

					if (grid.webmap) {
						// Refresh map grids
						grid.rerender();
					} else if (grid.rootIdBinding) {
						// Handle tree grids with root binding
						if (grid.hasDataSource()) {
							const existingRootValue = grid.grid
								.getDataSource()
								.getField("bizParentId").rootValue;
							const newRootValue = `_${grid._view._vm.getValue(
								grid.rootIdBinding,
							)}`;

							if (existingRootValue !== newRootValue) {
								grid.setDataSource(grid.dataSource);
							} else if (grid.autoPopulate) {
								grid.refresh();
							}
						} else {
							grid.setDataSource(grid.dataSource);
						}
					} else if (grid.dataSource) {
						// Handle list grids or tree grids
						if (grid.hasDataSource()) {
							if (grid.autoPopulate) {
								grid.refresh();
							}
						} else {
							grid.setDataSource(grid.dataSource);
						}
					}

					this._refreshedGrids[gridID] = true; // Mark grid as refreshed
				}
			}
		}
	},

	/**
	 * Extracts values from view controls into the instance.
	 * If validation errors exist, returns null.
	 *
	 * @param {boolean} validate - determines whether to perform validation before gathering data.
	 * @returns {Object|null} - the gathered data object or null if validation fails.
	 */
	gather: function (validate) {
		if (validate && !this._vm.validate()) {
			return null; // Return early if validation fails
		}

		// TODO: Add extra list validation here if necessary

		return this._gather();
	},

	/**
	 * Gathers and transforms scalar and list membership values from grids.
	 * - Collects scalar values.
	 * - Converts list memberships to arrays of bizIds.
	 * - Transforms comparison trees into object hierarchies.
	 * - Reorders data in grids for accurate representation.
	 *
	 * @returns {Object} - the gathered and transformed result object.
	 */
	_gather: function () {
		// Collect all scalar values from the values manager
		const result = this._vm.getValues();

		// Iterate through each grid binding and process the data
		for (const gridBinding in this._grids) {
			let data = result[gridBinding];

			if (Array.isArray(data) && data.length > 0) {
				if (data[0].properties) {
					// Handle comparison widgets
					const comparisons = this._grids[gridBinding];
					for (const comparisonID in comparisons) {
						result[gridBinding] = comparisons[comparisonID].getData();
					}
				} else {
					const grids = this._grids[gridBinding];

					for (const gridID in grids) {
						const grid = grids[gridID];
						const smartClientGrid = grid.grid;

						if (smartClientGrid) {
							// Ensure correct ordering when user reorders data via drag-and-drop
							smartClientGrid.reorderData();
							data = data.sortByProperty(grid._ordinal, true);
						}

						// Process list memberships or aggregated references in data grids
						if (
							(grid._candidateList && grid._memberList) ||
							(smartClientGrid?.fields?.length === 1 &&
								smartClientGrid.fields[0].name === "bizId" &&
								smartClientGrid.fields[0].type === "enum")
						) {
							if (grid._ordinal) {
								grid._memberList.reorderData();
								data = data.sortByProperty(grid._ordinal, true);
							}

							// Extract bizIds from list memberships
							result[gridBinding] = data.map((item) => item.bizId);
						}
					}
				}
			}
		}

		return result;
	},

	/**
	 * Evaluates a condition name against provided values.
	 * - Returns true if conditionName is "true".
	 * - Returns false if conditionName is "false".
	 * - Otherwise, looks up the conditionName in the values object.
	 *
	 * @param {string} conditionName - the name of the condition to evaluate.
	 * @param {Object} values - the values object to evaluate the condition against.
	 * @returns {boolean} - the result of the condition evaluation.
	 */
	_evaluateConditionName: function (conditionName, values) {
		if (!conditionName) return false;

		switch (conditionName) {
			case "true":
				return true;
			case "false":
				return false;
			default:
				return Boolean(values[conditionName]);
		}
	},

	/**
	 * Processes and updates widgets in a container based on visibility, conditions, and value mappings.
	 *
	 * @param {Object} container - the BizContainer containing widgets.
	 * @param {boolean} invisible - whether the current container is invisible.
	 * @param {Object} values - ViewModel values with evaluated conditions.
	 * @param {Object} valueMaps - value maps for select inputs.
	 * @param {Array} toRerender - list of items to rerender after processing.
	 */
	_processWidgets: function (
		container,
		invisible,
		values,
		valueMaps,
		toRerender,
	) {
		for (const contained of container.contained) {
			this._enableDisable(contained, container, values);
			const containedInvisible =
				invisible || this._showHide(contained, container, values, false);

			// Recursively process nested containers
			if (isc.isA.Function(contained.addContained)) {
				this._processWidgets(
					contained,
					containedInvisible,
					values,
					valueMaps,
					toRerender,
				);
			}

			// Process tab panes
			if (isc.isA.Function(contained.addBizTab)) {
				let selectedTabNumber = contained.getSelectedTabNumber();

				for (const bizTab of contained.bizTabs) {
					const tabInvisible =
						containedInvisible ||
						this._showHide(bizTab, contained, values, false);
					this._enableDisable(bizTab, contained, values);
					this._processWidgets(
						bizTab.pane,
						tabInvisible,
						values,
						valueMaps,
						toRerender,
					);
				}

				// Restore selected tab if specified in values
				const selectedTabIndex = values[contained.selectedTabIndexBinding];
				if (selectedTabIndex !== undefined && selectedTabIndex != null) {
					selectedTabNumber = selectedTabIndex;
				}
				if (selectedTabNumber != null) {
					contained.selectTab(selectedTabNumber);
				}
			}

			// Process form widgets
			if (isc.isA.Function(contained.getItems)) {
				// Manage form membership based on visibility
				if (containedInvisible) {
					this._vm.members?.contains(contained) &&
						this._vm.removeMember(contained);
				} else {
					(!this._vm.members || !this._vm.members.contains(contained)) &&
						this._vm.addMember(contained);
				}

				// Process each form item
				for (const item of contained.items) {
					this._enableDisable(item, contained, values);
					if (!this._showHide(item, contained, values, false)) {
						// Manage value maps for lookup and selection inputs
						if (item.type === "bizLookupDescription") {
							item.setValueMapFromEditView(values);
						} else if (["select", "enum", "comboBox"].includes(item.type)) {
							this._updateValueMap(item, values, valueMaps);
						}
					}
				}
			}

			// Add to rerender list if necessary
			if (isc.isA.Function(contained.rerender)) {
				toRerender.add(contained);
			}
		}
	},

	/**
	 * Updates the value map for a form item.
	 *
	 * @param {Object} item - the form item to update.
	 * @param {Object} values - the evaluated form values.
	 * @param {Object} valueMaps - predefined value maps.
	 */
	_updateValueMap: function (item, values, valueMaps) {
		let valueMap = {};

		// If the item has an option data source, fetch the data
		if (item.optionDataSource) {
			if (item.optionDataSource === isc.BizUtil.COMPLETE_DATA_SOURCE) {
				valueMap[values[item.name]] = values[item.name];
				item.setValueMap(valueMap);
			} else {
				item.fetchData();
				valueMap[values[item.name]] =
					values[`${item.name}_${item.displayField}`];
				item.setValueMap(valueMap);
			}
		}
		// Domain values must be in the bean
		else if (valueMaps && valueMaps[item.name]) {
			valueMap = valueMaps[item.name];
			item.setValueMap(valueMap);
		}
	},

	/**
	 * Updates the enabled/disabled state of widget actions based on provided conditions.
	 *
	 * @param {Object} widget - the widget whose state needs updating.
	 * @param {Object} parent - the parent container of the widget.
	 * @param {Object} values - the values to evaluate against conditions.
	 */
	_enableDisable: function (widget, parent, values) {
		// Helper function to update a widget's capability based on a condition name
		const updateCapability = (capabilityKey, conditionName) => {
			if (widget[conditionName]) {
				widget[capabilityKey] = !this._evaluateConditionName(
					widget[conditionName],
					values,
				);
			}
		};

		updateCapability("canPick", "disablePickConditionName");
		updateCapability("canAdd", "disableAddConditionName");
		updateCapability("canZoom", "disableZoomConditionName");
		updateCapability("canEdit", "disableEditConditionName");
		updateCapability("canRemove", "disableRemoveConditionName");
		updateCapability("canClear", "disableClearConditionName");

		// Set overall disabled state based on a specific condition
		this._setDisabled(widget, parent, widget.disabledConditionName, values);
	},

	/**
	 * Updates the disabled state of a widget and its related parent components based on a condition.
	 *
	 * @param {Object} widget - the widget to enable or disable.
	 * @param {Object} parent - the parent container of the widget.
	 * @param {string} disabledConditionName - the condition name used for evaluation.
	 * @param {Object} values - the values to evaluate the condition against.
	 */
	_setDisabled: function (widget, parent, disabledConditionName, values) {
		// Evaluate whether the widget should be disabled
		const isDisabled = this._evaluateConditionName(
			disabledConditionName,
			values,
		);

		// Set required state if applicable (visibility takes precedence)
		if (
			widget.bizRequired &&
			widget.setRequired &&
			widget.isVisible &&
			widget.isVisible()
		) {
			widget.setRequired(!isDisabled);
		}

		// Directly set disabled state if the method exists
		if (widget.setDisabled) {
			widget.setDisabled(isDisabled);
		}
		// Fallback to enable/disable methods if available
		else if (widget.enable && widget.disable) {
			isDisabled ? widget.disable() : widget.enable();
		}
		// Handle tab disabling in parent container if applicable
		else if (parent && parent.enableTab && parent.disableTab && widget.pane) {
			const tab = parent.tabForPane(widget.pane);
			const tabIndex = parent.getTabNumber(tab);

			if (tabIndex >= 0) {
				isDisabled ? parent.disableTab(tab) : parent.enableTab(tab);
			}
		}
	},

	/**
	 * Applies a disabled state to a widget based on the provided binding and condition.
	 *
	 * @param {string} binding - the binding key used to retrieve the widget.
	 * @param {string} disabledConditionName - the condition name used to determine if the widget should be disabled.
	 */
	setDisabled: function (binding, disabledConditionName) {
		const widget = this._vm.getItem(binding);

		if (!widget) {
			return; // Early exit if widget is not found
		}

		const values = this.gather(false);
		this._updateDisabledState(widget, null, disabledConditionName, values);
	},

	/**
	 * Toggles the disabled state of a widget based on its current state.
	 * If the widget is disabled, it will be enabled, and vice versa.
	 *
	 * @param {string} binding - the binding key used to retrieve the widget.
	 */
	toggleDisabled: function (binding) {
		const widget = this._vm.getItem(binding);

		if (!widget) {
			return; // Early return if widget is not found
		}

		const disabledState = widget.isDisabled() ? "false" : "true";
		this._setDisabled(widget, null, disabledState, null);
	},

	/**
	 * Toggles the visibility state of a widget based on its current state.
	 * If the widget is visible, it will be hidden, and vice versa.
	 *
	 * @param {string} binding - the binding key used to retrieve the widget.
	 */
	toggleVisibility: function (binding) {
		const widget = this._vm.getItem(binding);

		if (!widget) {
			return; // Early return if widget is not found
		}

		const visibilityState = widget.isVisible() ? "true" : "false";
		this._setInvisible(widget, null, visibilityState, null);
	},

	/**
	 * Determines whether a widget should be visible or not, and applies the visibility state.
	 * If forced to be invisible, the widget will be hidden regardless of its condition.
	 *
	 * @param {Object} widget - the widget whose visibility will be updated.
	 * @param {Object} parent - the parent container of the widget.
	 * @param {Object} values - the values to evaluate against the widget's condition.
	 * @param {boolean} forceInvisible - whether the widget should be forced to be invisible (optional).
	 * @returns {boolean} - the result of the `_setInvisible` method, which determines the widget's visibility.
	 */
	_showHide: function (widget, parent, values, forceInvisible) {
		// Apply forced invisibility if specified
		return this._setInvisible(
			widget,
			parent,
			widget.invisibleConditionName,
			values,
			forceInvisible,
		);
	},

	/**
	 * Determines whether a widget should be visible or invisible based on conditions and forced invisibility.
	 * Applies the visibility state and updates the widget or parent container accordingly.
	 *
	 * @param {Object} widget - the widget whose visibility will be updated.
	 * @param {Object} parent - the parent container of the widget.
	 * @param {string} invisibleConditionName - the condition to evaluate for widget invisibility.
	 * @param {Object} values - the values used to evaluate the invisibility condition.
	 * @param {boolean} [forceInvisible=false] - if set to `true`, forces the widget to be invisible regardless of the condition.
	 * @returns {boolean} - the final invisibility state (`true` if invisible, `false` otherwise).
	 */
	_setInvisible: function (
		widget,
		parent,
		invisibleConditionName,
		values,
		forceInvisible = false,
	) {
		let invisible = forceInvisible;

		if (!invisible) {
			invisible = this._evaluateConditionName(invisibleConditionName, values);
		}

		// Adjust 'required' state based on visibility
		if (widget.bizRequired && widget.setRequired) {
			if (invisible) {
				widget.setRequired(false);
			} else if (widget.isDisabled && !widget.isDisabled()) {
				widget.setRequired(true);
			}
		}

		// Apply visibility changes based on widget type and conditions
		if (widget.show && widget.hide) {
			invisible ? widget.hide() : widget.show();
		} else if (parent) {
			invisible ? parent.hideMember(widget) : parent.showMember(widget);
		}

		return invisible;
	},

	/**
	 * Sets the visibility of a widget based on a condition name. If the widget exists, it will evaluate the invisibility condition and apply the corresponding visibility state.
	 *
	 * @param {string} binding - the binding key used to retrieve the widget.
	 * @param {string} invisibleConditionName - the condition to evaluate for widget invisibility.
	 */
	setInvisible: function (binding, invisibleConditionName) {
		const widget = this._vm.getItem(binding);

		if (!widget) {
			return; // Early return if widget is not found
		}

		const values = this.gather(false);
		this._setInvisible(widget, null, invisibleConditionName, values);
	},
});

/**
 * Implements the BizButton UI component.
 * Extends IButton from the SmartClient library.
 */
isc.ClassFactory.defineClass("BizButton", "IButton");

isc.BizButton.addMethods({
	initWidget: function () {
		// Determines whether the button should auto fit based on provided width and height arguments
		this.autoFit = !(arguments[0].width || arguments[0].height);
		this.hasDisabledIcon = false;

		// Sets the title of the button (display name)
		this.title = this.displayName || null;

		// Sets up the tooltip if provided
		if (this.tooltip) {
			this.canHover = true;
			this.getHoverHTML = function () {
				return this.tooltip;
			};
		}

		// Configures the disabled icon behavior if an icon is set
		if (this.icon) {
			this.showDisabledIcon = this.hasDisabledIcon;
		}

		// Defines the action triggered on button click
		this.action = function () {
			if (this._view && this._view._blurry) {
				this._view._blurry = this;
				return;
			}
			if (this.confirm) {
				isc.ask(
					this.confirm,
					(value) => {
						if (value) {
							this._action();
						}
					},
					{ title: "Confirm" },
				);
			} else {
				this._action();
			}
		};

		/**
		 * Executes the appropriate action based on the `type` property of the button.
		 * Supports actions like 'OK', 'Save', 'Cancel', 'Delete', 'Export', 'Import', etc.
		 *
		 * @method _action
		 * @private
		 * @returns {void}
		 */
		this._action = function () {
			const validate = this.validate === undefined ? true : this.validate;

			// Declare variables outside the switch to avoid redefinition
			let instance, opener, gridBinding, bizId, url;

			// Handle different button types
			switch (this.type) {
				case "OK":
					// Save action on edit view
					this._view.saveInstance(validate, this.actionName);
					break;

				case "Save":
					// Save action on edit view with success message
					this._view.saveInstance(validate, this.actionName, () => {
						isc.BizUtil.growl(
							[{ severity: "info", summary: "Saved", detail: "Changes Saved" }],
							3000,
						);
					});
					break;

				case "Add":
					// Add action on child edit view
					break;

				case "ZoomOut": {
					// Handle ZoomOut action with changes validation
					instance = this._view.gather(false);
					const { _apply: apply, _changed: changedOnServer } = instance;
					if (apply || changedOnServer || this._view._vm.valuesHaveChanged()) {
						this._view.saveInstance(validate, this.actionName);
					} else {
						opener = isc.WindowStack.getOpener();
						isc.WindowStack.popoff(this._view._saved);
						opener._source = null;
					}
					break;
				}

				case "Cancel": {
					// Cancel action with unsaved changes check
					instance = this._view.gather(false);
					const { _changed: changedOnServerCancel } = instance;
					opener = isc.WindowStack.getOpener();

					if (changedOnServerCancel || this._view._vm.valuesHaveChanged()) {
						isc.ask(
							`There are unsaved changes in the ${this._view._singular}. Do you wish to cancel?`,
							(value) => {
								if (value) {
									isc.WindowStack.popoff(this._view._saved);
									opener._source = null;
								}
							},
							{ title: "Discard Unsaved Changes?" },
						);
					} else {
						isc.WindowStack.popoff(this._view._saved);
						opener._source = null;
					}
					break;
				}

				case "Delete":
					// Delete action on edit view
					this._view.deleteInstance(validate);
					break;

				case "Remove":
					// Remove action on child edit view
					instance = this._view.gather(false);
					bizId = instance.bizId;
					gridBinding = this._view._b.substring(
						this._view._b.lastIndexOf(".") + 1,
						this._view._b.lastIndexOf("ElementById"),
					);
					opener = isc.WindowStack.getOpener();

					if (opener) {
						const openerListGrids = opener._grids[gridBinding];
						if (openerListGrids) {
							for (const openerListGridID in openerListGrids) {
								const openerListGrid = openerListGrids[openerListGridID];
								openerListGrid.remove(bizId);
								if (openerListGrid.bizRemoved) {
									openerListGrid.bizRemoved();
								}
							}
						}
						opener._vm.setValue("_apply", true);
					}
					isc.WindowStack.popoff(false);
					break;

				case "Report":
					// Report action
					isc.ReportDialog.popupReport(this._view, this.params);
					break;

				case "BizExport":
					// BizExport action
					instance = this._view.gather(false);
					if (instance) {
						this._view.saveInstance(validate, null, () => {
							window.location.assign(
								`bizexport.xls?_n=${this.actionName}&_doc=${this._view._mod}.${
									this._view._doc
								}&_c=${instance._c}&_ctim=${new Date().getTime()}`,
							);
						});
					}
					break;

				case "BizImport":
					// BizImport action
					instance = this._view.gather(false);
					if (instance) {
						url = `bizImport.xhtml?_a=${this.actionName}&_c=${instance._c}`;
						if (this._view._b) {
							url += `&_b=${this._view._b.replaceAll("_", ".")}`;
						}
						this._view.saveInstance(validate, null, () => {
							isc.WindowStack.popup(null, "BizPort Import", true, [
								isc.HTMLPane.create({
									contentsType: "page",
									contents: "Loading Page...",
									contentsURL: url,
								}),
							]);
						});
					}
					break;

				case "Download":
					// Download action
					this._view.doAction(this.actionName, validate);
					break;

				case "Upload":
					// Upload action
					instance = this._view.gather(false);
					if (instance) {
						url = `fileUpload.xhtml?_a=${this.actionName}&_c=${instance._c}`;
						if (this._view._b) {
							url += `&_b=${this._view._b.replaceAll("_", ".")}`;
						}
						this._view.saveInstance(validate, null, () => {
							isc.WindowStack.popup(null, "Upload", true, [
								isc.HTMLPane.create({
									contentsType: "page",
									contents: "Loading Page...",
									contentsURL: url,
								}),
							]);
						});
					}
					break;

				case "Navigate":
					// Navigate to a binding within a conversation
					break;

				case "Print":
					// Print action
					isc.ReportDialog.popupReport(this._view, {
						_mod: this._view._mod,
						_doc: this._view._doc,
					});
					break;

				default:
					// Default action
					this._view.doAction(this.actionName, validate);
					break;
			}
		};

		this.Super("initWidget", arguments);
	},
});

/**
 * Implements the BizZoomIn UI component.
 * Extends IButton from the SmartClient library.
 */
isc.ClassFactory.defineClass("BizZoomIn", "IButton");

isc.BizZoomIn.addMethods({
	initWidget: function () {
		this.autoFit = !(arguments[0].width || arguments[0].height);
		this.hasDisabledIcon = false;

		this.title = this.displayName || null;

		if (this.tooltip) {
			this.canHover = true;
			this.getHoverHTML = function () {
				return this.tooltip;
			};
		}

		if (this.icon) {
			this.showDisabledIcon = this.hasDisabledIcon;
		}

		/**
		 * Action triggered when the zoom in button is clicked.
		 */
		this.action = () => {
			// Validate here so we can show the zoom message if required
			const instance = this._view.gather(true);

			if (instance) {
				const bizId = instance[this.binding];
				if (bizId) {
					// Get the view polymorphically
					isc.BizUtil.getEditView(
						instance[this.binding + "_bizModule"],
						instance[this.binding + "_bizDocument"],
						(view) => {
							// Determine the view binding
							const viewBinding = this._view._b
								? `${this._view._b}.${this.binding}`
								: this.binding;

							const fromRect = this.getPageRect();

							if (instance._apply || this._view._vm.valuesHaveChanged()) {
								delete instance._apply;
								// Apply changes to the current form before zooming in
								this._view.saveInstance(true, null, () => {
									isc.WindowStack.popup(fromRect, "Edit", false, [view]);
									view.editInstance(bizId, viewBinding, instance._c, false);
								});
							} else {
								isc.WindowStack.popup(fromRect, "Edit", false, [view]);
								view.editInstance(bizId, viewBinding, instance._c, false);
							}
						},
					);
				} else {
					isc.warn("You cannot zoom in to an empty reference");
				}
			} else {
				isc.warn("You cannot zoom in until you fix the problems found");
			}
		};

		this.Super("initWidget", arguments);
	},
});

// TODO: Dialog button

/**
 * Implements the BizVBox UI component.
 * Extends VLayout from the SmartClient library.
 */
isc.ClassFactory.defineClass("BizVBox", "VLayout");

isc.BizVBox.addMethods({
	initWidget: function () {
		this.contained = [];
		this.Super("initWidget", arguments);
	},

	/**
	 * Adds a widget to the contained array and to the layout's members.
	 *
	 * @function
	 * @name BizVBox#addContained
	 * @param {Widget} contained - the widget to add
	 * @returns {void}
	 */
	addContained: function (contained) {
		this.contained.add(contained);
		this.addMember(contained);
	},
});

/**
 * Implements the BizHBox UI component.
 * Extends HLayout from the SmartClient library.
 */
isc.ClassFactory.defineClass("BizHBox", "HLayout");

isc.BizHBox.addMethods({
	initWidget: function () {
		this.contained = [];
		this.Super("initWidget", arguments);
	},

	/**
	 * Adds a widget to the contained array and to the layout's members.
	 *
	 * @function
	 * @name BizHBox#addContained
	 * @param {Widget} contained - the widget to add
	 * @returns {void}
	 */
	addContained: function (contained) {
		this.contained.add(contained);
		this.addMember(contained);
	},
});

/**
 * Implements the BizCollapsible UI component.
 * Extends VLayout fromthe SmartClient library.
 */
isc.ClassFactory.defineClass("BizCollapsible", "VLayout");

isc.BizCollapsible.addMethods({
	initWidget: function (config) {
		this.contained = [];
		this.minimized = config.minimized;

		this.Super("initWidget", { width: "100%", height: "100%" });

		const me = this; // Required as parent and child scope is required

		this.guts = isc.Window.create({
			title: config.title,
			autoDraw: true, // Required for render in tab panes
			autoSize: true, // Required for render in tab panes
			height: "100%", // Width set in draw method below
			canDragReposition: false,
			canDragResize: false,
			showCloseButton: false,
			animateMinimize: false,
			headerLabelProperties: {
				width: "100%",
				click: () => {
					// Toggle between minimized and restored state
					this.guts.minimized ? this.guts.restore() : this.guts.minimize();
				},
			},
			restore: function () {
				if (me._view.isVisible()) {
					me._view.delayCall("refreshListGrids", [
						false,
						false,
						me._view.gather(false),
					]);
				}
				// reset max height to default
				me.setProperty("maxHeight", 10000);
				this.Super("restore", arguments);
			},
			minimize: function () {
				me.setHeight(30);
				me.setProperty("maxHeight", 30);
				this.Super("minimize", arguments);
			},
		});
		this.addMember(this.guts);

		// Timer to throttle resize callbacks and prevent infinite loops
		this._resizeTimer = null;
	},

	/**
	 * Sets the width of the window to match the parent's width at draw time.
	 *
	 * @function
	 * @name BizCollapsible#draw
	 * @returns {void}
	 */
	draw: function () {
		if (this.guts) {
			this.guts.setWidth(this.getWidth());
		}
		return this.Super("draw", arguments);
	},

	/**
	 * Resizes the window and throttles resize events using a timer.
	 *
	 * @function
	 * @name BizCollapsible#resized
	 * @returns {void}
	 */
	resized: function () {
		if (this.guts) {
			if (this._resizeTimer) {
				clearTimeout(this._resizeTimer);
			}
			this._resizeTimer = setTimeout(() => {
				this._resize();
			}, 100);
		}
		this.Super("resized", arguments);
	},

	/**
	 * Resizes the window and resets the resize timer.
	 *
	 * @function
	 * @name BizCollapsible#_resize
	 * @returns {void}
	 */
	_resize: function () {
		this._resizeTimer = null;
		this.guts.setWidth(this.getWidth());
	},

	/**
	 * Adds a widget to the collapsible container.
	 *
	 * @function
	 * @name BizCollapsible#addContained
	 * @param {Widget} contained - the widget to add to the container
	 * @returns {void}
	 */
	addContained: function (contained) {
		this.contained.add(contained);
		this.guts.addItem(contained);

		// Minimize if required after contents are added (only 1 VBox, HBox, or Form)
		if (this.minimized) {
			this.guts.minimize();
		}
	},
});

/**
 * Implements the BizTabPane UI component.
 * Extends TabSet from the SmartClient library.
 */
isc.ClassFactory.defineClass("BizTabPane", "TabSet");

isc.BizTabPane.addMethods({
	initWidget: function () {
		this.tabs = [];
		this.bizTabs = [];
		this.destroyPanes = false; // Don't destroy the tab panes when removing, just show & hide
		this.tabBarThickness = this.tabBarThickness || 30; // Default tab bar thickness (height)
		this.Super("initWidget", arguments);
	},

	/**
	 * Handles the selection of a tab.
	 *
	 * @function
	 * @param {number} tabNum - the tab number that was selected
	 * @param {Object} tabPane - the tab pane object
	 * @param {string} ID - the ID of the tab
	 * @param {Object} tab - the selected tab object
	 * @returns {void}
	 */
	tabSelected: function (tabNum, tabPane, ID, tab) {
		if (this._view.isVisible()) {
			this._view.delayCall("refreshListGrids", [
				false,
				false,
				this._view.gather(false),
			]);
		}
	},

	/**
	 * Adds a custom tab definition to the BizTabPane and adds it to the visible tab set.
	 *
	 * @function
	 * @param {BizTab} bizTab - the tab definition to add
	 * @returns {void}
	 */
	addBizTab: function (bizTab) {
		this.bizTabs.add(bizTab);
		this.addTab({
			name: bizTab.name,
			title: bizTab.title,
			icon: bizTab.icon,
			prompt: bizTab.prompt,
			pane: bizTab.pane,
			disabledConditionName: bizTab.disabledConditionName,
			invisibleConditionName: bizTab.invisibleConditionName,
			selectedConditionName: bizTab.selectedConditionName,
		});
	},

	/**
	 * Shows a hidden member tab based on the provided bizTab definition.
	 * If the tab is not already present, it attempts to find the correct position and adds it.
	 *
	 * @function
	 * @name BizTabPane#showMember
	 * @param {BizTab} bizTab - the bizTab definition to show
	 * @returns {void}
	 */
	showMember: function (bizTab) {
		let existingTabPosition = this.getTabNumber(bizTab.name);
		if (existingTabPosition < 0) {
			// Find the position to insert this tab
			let tabPosition = 0; // Default position
			let tabNumber = parseInt(bizTab.name, 10) - 1; // Start searching backwards
			while (tabNumber >= 0) {
				existingTabPosition = this.getTabNumber(String(tabNumber));
				if (existingTabPosition >= 0) {
					tabPosition = existingTabPosition + 1;
					break;
				}
				tabNumber--;
			}
			this.addTab(
				{
					name: bizTab.name,
					title: bizTab.title,
					pane: bizTab.pane,
					disabledConditionName: bizTab.disabledConditionName,
					invisibleConditionName: bizTab.invisibleConditionName,
					selectedConditionName: bizTab.selectedConditionName,
				},
				tabPosition,
			);
		}
	},

	/**
	 * Hides a member tab based on the provided bizTab definition.
	 *
	 * @function
	 * @name BizTabPane#hideMember
	 * @param {BizTab} bizTab - the bizTab definition to hide
	 * @returns {void}
	 */
	hideMember: function (bizTab) {
		if (this.getTabNumber(bizTab.name) >= 0) {
			this.removeTab(bizTab.name);
		}
	},
});

// TODO: HTML
// TODO: Radio

/**
 * Implements the BizListMembership UI component.
 * Extends HLayout from the SmartClient library.
 */
isc.ClassFactory.defineClass("BizListMembership", "HLayout");

isc.BizListMembership.addMethods({
	initWidget: function (config) {
		this.membersMargin = 10;

		const me = this; // Required as parent and child scope is required

		// Create candidate list grid
		this._candidateList = isc.ListGrid.create({
			width: "100%",
			height: "100%",
			minHeight: 100,
			canDragRecordsOut: true,
			canAcceptDroppedRecords: true,
			dragDataAction: "move",
			recordDrop: function (dropRecords, targetRecord, index, sourceWidget) {
				this.Super("recordDrop", arguments);

				me._view._vm.setValue("_changed", true); // mark the view as dirty
				me._view._vm.setValue("_apply", true); // post view changes before zooming
				me.changed();
			},
			alternateRecordStyles: true,
			autoFetchData: false,
			leaveScrollbarGap: false,
			showHeaderContextMenu: false,
			fields: [
				{
					name: "bizKey",
					title: this.candidatesHeading ? this.candidatesHeading : "Candidates",
				},
			],
		});

		// Create member list grid
		this._memberList = isc.ListGrid.create({
			width: "100%",
			height: "100%",
			minHeight: 100,
			canDragRecordsOut: true,
			canAcceptDroppedRecords: true,
			dragDataAction: "move",
			canReorderRecords: config._ordinal ? true : false,
			recordDrop: function (dropRecords, targetRecord, index, sourceWidget) {
				this.Super("recordDrop", arguments);

				if (sourceWidget === this) {
					// Handle reorder drop
					this.reorderData();
				}

				me._view._vm.setValue("_changed", true); // mark the view as dirty
				me._view._vm.setValue("_apply", true); // post view changes before zooming
				me.changed();
			},
			/**
			 * Reorders the member list if the grid is orderable.
			 */
			reorderData: function () {
				if (config._ordinal) {
					// this grid is orderable
					const data = this.getData();
					for (let i = 0, l = data.length; i < l; i++) {
						data[i][config._ordinal] = i + 1;
					}
				}
			},
			alternateRecordStyles: true,
			autoFetchData: false,
			leaveScrollbarGap: false,
			showHeaderContextMenu: false,
			fields: [
				{
					name: "bizKey",
					title: this.membersHeading ? this.membersHeading : "Members",
				},
			],
		});

		// Layout for BizListMembership widget
		this.members = [
			this._candidateList,
			isc.VLayout.create({
				layoutAlign: "center",
				membersMargin: 10,
				height: 75,
				members: [
					// Button for adding members
					isc.IButton.create({
						title: null,
						icon: "icons/memberAssign.png",
						iconWidth: 24,
						iconHeight: 24,
						iconAlign: "center",
						width: 36,
						height: 36,
						click: () => {
							this._memberList.transferSelectedData(this._candidateList);
							this._view._vm.setValue("_changed", true); // mark the view as dirty
							this._view._vm.setValue("_apply", true); // post view changes before zooming
							this.changed();
						},
						canHover: true,
						getHoverHTML: function () {
							return "Add the selected candidates.";
						},
					}),
					// Button for removing members
					isc.IButton.create({
						title: null,
						icon: "icons/memberUnassign.png",
						iconWidth: 24,
						iconHeight: 24,
						iconAlign: "center",
						width: 36,
						height: 36,
						click: () => {
							this._candidateList.transferSelectedData(this._memberList);
							this._view._vm.setValue("_changed", true); // mark the view as dirty
							this._view._vm.setValue("_apply", true); // post view changes before zooming
							this.changed();
						},
						canHover: true,
						getHoverHTML: function () {
							return "Remove the selected members.";
						},
					}),
				],
			}),
			this._memberList,
		];

		this.Super("initWidget", arguments);

		// Manage the grids in the view
		let grids = this._view._grids[this._b];
		if (!grids) {
			grids = {};
			this._view._grids[this._b] = grids;
		}
		grids[this.getID()] = this;
	},

	/**
	 * Sets the data for the candidate and member lists.
	 *
	 * @param {Array} candidates - the data for the candidates.
	 * @param {Array} members - the data for the members.
	 */
	setData: function (candidates, members) {
		this._candidateList.setData(candidates);
		this._memberList.setData(members);
	},

	/**
	 * Handles changes to the lists. This method can be overridden during view generation.
	 */
	changed: function () {},
});

/**
 * Implements the BizComparison UI component.
 * Extends HLayout from the SmartClient library.
 */
isc.ClassFactory.defineClass("BizComparison", "HLayout");

isc.BizComparison.addMethods({
	initWidget: function () {
		this.canDragResize = true;
		this.resizeFrom = ["L", "R"];

		this._comparisonTree = isc.TreeGrid.create({
			width: "50%",
			height: "100%",
			fields: [
				{ name: "bizKey", title: "Document" },
				{ name: "relationship", title: "Relationship" },
			],
			data: isc.Tree.create({
				modelType: "parent",
				idField: "bizId",
				parentIdField: "parent",
				data: [],
			}),
			selectionUpdated: (record, recordList) => {
				if (record) {
					this.setFormFields(record.properties);
				}
			},
			showOpenIcons: false,
			showDropIcons: false,
			showResizeBar: true,
		});

		/**
		 * Generates hover HTML for diffing two items.
		 * @param {Object} item - the item being hovered over.
		 * @returns {string|null} the HTML diff if applicable.
		 */
		const hoverHTML = function (item) {
			if (item.type !== "boolean") {
				if (this.diff_match_patch) {
					let newItem = item;
					let oldItem = item;
					if (item.name.startsWith("_old_")) {
						newItem = this.getItem(item.name.substring(5));
					} else {
						oldItem = this.getItem("_old_" + item.name);
					}

					let newDisplayValue = newItem.getDisplayValue();
					let oldDisplayValue = oldItem.getDisplayValue();

					if (newDisplayValue?.startsWith("<")) {
						newDisplayValue = "";
					}
					if (oldDisplayValue?.startsWith("<")) {
						oldDisplayValue = "";
					}

					const diffs = this.diff_match_patch.diff_main(
						oldDisplayValue,
						newDisplayValue,
						false,
					);
					this.diff_match_patch.diff_cleanupEfficiency(diffs);
					return this.diff_match_patch.diff_prettyHtml(diffs);
				}
			}
			return null;
		};

		this._comparisonForm = isc.PropertySheet.create({
			width: "100%",
			height: "100%",
			numCols: this.editable ? 5 : 4,
			colWidths: this.editable ? [150, "*", 50, "*", 30] : [150, "*", "*", 30],
			border: "1px solid #A7ABB4",
			titleHoverHTML: hoverHTML,
			fields: [],
		});

		SKYVE.Util.loadJS(
			"skyve/desktop/diff_match_patch.js?v=" + SKYVE.Util.v,
			() => {
				this._comparisonForm.diff_match_patch = new diff_match_patch();
				this._comparisonForm.diff_match_patch.Diff_EditCost = 4;
			},
		);

		this.members = [
			this._comparisonTree,
			isc.VLayout.create({ overflow: "auto", members: [this._comparisonForm] }),
		];

		this.Super("initWidget", arguments);

		let grids = this._view._grids[this._b];
		if (!grids) {
			grids = {};
			this._view._grids[this._b] = grids;
		}
		grids[this.getID()] = this;
	},

	/**
	 * Sets the data for the comparison tree.
	 * @param {Array} data - the data to populate the tree with.
	 */
	setData: function (data) {
		this._comparisonTree.setData(
			isc.Tree.create({
				modelType: "parent",
				idField: "bizId",
				parentIdField: "parent",
				data,
			}),
		);
		this.setFormFields([]);
	},

	/**
	 * Sets the fields for the form based on the given properties.
	 * @param {Array} properties - the properties to populate the form with.
	 */
	setFormFields: function (properties) {
		const fields = [
			{
				type: "blurb",
				align: "center",
				colSpan: 1,
				defaultValue: "Property",
				startRow: false,
				endRow: false,
				cellStyle: "propSheetTitle",
			},
			{
				type: "blurb",
				align: "center",
				colSpan: 1,
				defaultValue: "New",
				startRow: false,
				endRow: false,
				cellStyle: "propSheetTitle",
			},
		];

		if (this.editable) {
			fields.push({
				title: "<<-",
				type: "button",
				click() {
					for (let i = 5; i < form.getFields().length - 4; i += 4) {
						const old = form.getField(i + 2).getValue();
						form.getField(i).setValue(old ? old : "");
					}
				},
				startRow: false,
				endRow: false,
				align: "center",
				cellStyle: "propSheetValue",
			});
		}

		fields.push(
			{
				type: "blurb",
				align: "center",
				colSpan: 1,
				defaultValue: "Old",
				startRow: false,
				endRow: false,
				cellStyle: "propSheetTitle",
			},
			{
				type: "blurb",
				align: "center",
				colSpan: 1,
				defaultValue: "Diff",
				startRow: false,
				endRow: false,
				cellStyle: "propSheetTitle",
			},
		);

		properties.forEach((prop) => {
			const {
				name,
				title,
				type,
				editorType,
				length,
				valueMap,
				required,
				allowEmptyValue,
			} = prop;

			const field = {
				name,
				title,
				showTitle: true,
				type,
				width: "*",
				startRow: true,
				endRow: false,
				canEdit: this.editable,
				defaultValue: prop.newValue,
			};
			const oldField = {
				name: "_old_" + name,
				showTitle: false,
				type,
				width: "*",
				startRow: false,
				endRow: false,
				canEdit: false,
				defaultValue: prop.oldValue,
			};

			if (type === "richText" || editorType === "richText") {
				field.controlGroups = [];
				field.colSpan = 1;
				oldField.controlGroups = [];
				oldField.colSpan = 1;
			}

			if (editorType) {
				field.editorType = editorType;
				oldField.editorType = editorType;
			}
			if (length) {
				field.length = length;
				oldField.length = length;
			}
			if (valueMap) {
				field.valueMap = valueMap;
				oldField.valueMap = valueMap;
			}
			if (required) {
				field.required = required;
			}
			if (allowEmptyValue) {
				field.allowEmptyValue = allowEmptyValue;
			}

			fields.push(field);

			if (this.editable) {
				fields.push({
					title: "<-",
					type: "button",
					click() {
						const old = form.getField("_old_" + name).getValue();
						form.getField(name).setValue(old ? old : "");
					},
					align: "center",
					startRow: false,
					endRow: false,
					cellStyle: "propSheetValue",
				});
			}

			fields.push(oldField);

			fields.push({
				title: "...",
				type: "button",
				click: function () {
					isc.say(this.form.titleHoverHTML(this.form.getField(name)), null, {
						title: "Diff",
					});
				},
				align: "center",
				startRow: false,
				endRow: false,
				cellStyle: "propSheetValue",
			});
		});

		if (this.editable) {
			fields.push(
				{
					type: "spacer",
					colSpan: 5,
				},
				{
					title: "Apply Changes",
					type: "button",
					colSpan: 4,
					startRow: false,
					endRow: true,
					align: "right",
					click: function (form, item) {
						const values = form.getValues();
						const properties =
							this._comparisonTree.getSelectedRecord().properties;

						properties.forEach((property) => {
							property.newValue = values[property.name];
						});

						isc.showPrompt(
							'<span style="font-size:medium">Changes Applied</span>',
						);

						setTimeout(() => {
							isc.clearPrompt();
						}, 500);
					},
				},
				{
					type: "spacer",
					colSpan: 5,
				},
			);
		}

		this._comparisonForm.setFields(fields);
		this._comparisonForm.clearValues();
	},

	/**
	 * Retrieves the JSON data from the comparison editor.
	 * @returns {Object} the data in JSON format.
	 * @memberof BizComparison
	 */
	getData: function () {
		const result = this._getData(
			this._comparisonTree.getData().getRoot().children[0],
		);
		delete result._b;
		delete result._t;
		return result;
	},

	/**
	 * Recursively builds the data structure from the tree node.
	 * @param {Object} treeNode - the tree node to convert.
	 * @returns {Object} the data structure for the node.
	 * @private
	 */
	_getData: function (treeNode) {
		const result = {
			bizId: treeNode.bizId,
			_b: treeNode._b,
			_t: treeNode._t,
		};

		treeNode.properties.forEach((property) => {
			result[property.name] = property.newValue;
		});

		const children = treeNode.children;
		if (children) {
			children.forEach((child) => {
				const childResult = this._getData(child);
				const binding = childResult._b;
				const referenceType = childResult._t;
				delete childResult._b;
				delete childResult._t;

				const existing = result[binding];
				if (existing) {
					if (!Array.isArray(existing)) {
						result[binding] = [existing];
					}
					result[binding].push(childResult);
				} else {
					result[binding] = [childResult];
				}
			});
		}

		return result;
	},
});

// TODO: CheckMembership
// TODO: PickView

/**
 * Implements the BizDynamicImage UI component.
 * Extends VLayout from the SmartClient library.
 */
isc.ClassFactory.defineClass("BizDynamicImage", "VLayout");

isc.BizDynamicImage.addMethods({
	initWidget() {
		this.overflow = "hidden";
		this.wZoom = 100;
		this.hZoom = 100;

		// TODO: Initialize parameters add properties dynamically based on parameters

		this.members = [];
		this.mouseWheelTimer = null;

		// Create context menu for image controls
		this.contextMenu = isc.Menu.create({
			showShadow: true,
			shadowDepth: 10,
			data: this._createContextMenuData(),
		});

		this.Super("initWidget", arguments);
	},

	/**
	 * Creates the context menu data for zoom and other actions.
	 * @returns {Array} the context menu data structure.
	 */
	_createContextMenuData: function () {
		const zoomLevels = [50, 75, 100, 125, 150, 175, 200, 300, 400, 500];
		const zoomMenuItems = zoomLevels.map((level) => ({
			title: `${level}%`,
			click: () => this._zoom(level),
		}));

		return [
			{
				title: "Size",
				icon: "icons/mag.png",
				submenu: zoomMenuItems,
			},
			{
				title: "Enlarge",
				icon: "icons/magIn.png",
				click: () => this._zoom(this.wZoom + 10),
			},
			{
				title: "Reduce",
				icon: "icons/magOut.png",
				click: () => this._zoom(this.wZoom - 10),
			},
			{ isSeparator: true },
			{
				title: "Open",
				icon: "zoom.gif",
				click: () => this._open(),
			},
			{ isSeparator: true },
			{
				title: "Refresh",
				icon: "refresh.png",
				click: () => this.rerender(),
			},
		];
	},

	/**
	 * Adjusts the zoom level of the image.
	 * @param {number} zoomLevel - the desired zoom level.
	 * @param {boolean} [throttle=false] - whether to throttle the rerender call.
	 */
	_zoom: function (zoomLevel, throttle = false) {
		if (zoomLevel > 0) {
			this.wZoom = zoomLevel;
			this.hZoom = zoomLevel;

			if (throttle) {
				if (this.mouseWheelTimer) {
					clearTimeout(this.mouseWheelTimer);
				}
				this.mouseWheelTimer = setTimeout(() => {
					this.rerender();
				}, 250);
			} else {
				this.rerender();
			}
		}
	},

	/**
	 * Opens the image in a new window.
	 */
	_open: function () {
		const image = isc.BizDynamicImage.create({
			name: this.name,
			moduleDotDocument: this.moduleDotDocument,
			format: this.format,
			_view: this._view,
		});
		isc.WindowStack.popup(null, "Image", true, [image]);
		image.rerender();
	},

	/**
	 * Handles resizing of the widget.
	 */
	resized: function () {
		this.rerender();
	},

	/**
	 * Rerenders the image with updated dimensions and zoom level.
	 */
	rerender: function () {
		const contextValue = this._view._vm.getValue("_c");
		if (!contextValue) return; // Skip if no context value is available

		this.mouseWheelTimer = null;
		this._clearExistingImage();

		const baseValue = this._view._b;
		const { width, height } = this._calculateImageDimensions();
		const imageSrc = this._generateImageSrc(
			width,
			height,
			contextValue,
			baseValue,
		);

		this._img = this._createImageElement(width, height, imageSrc);
		if (this.members) {
			this.addMember(this._img);
		}
	},

	/**
	 * Clears the existing image from the widget.
	 */
	_clearExistingImage: function () {
		if (this.members && this.members.length === 1) {
			this.removeMember(0);
			if (this._img) {
				this._img.destroy();
			}
		}
	},

	/**
	 * Calculates the dimensions of the image.
	 * @returns {Object} An object containing the calculated width and height.
	 */
	_calculateImageDimensions: function () {
		const width = this.imageWidth
			? this.imageWidth
			: this.getVisibleWidth() - 20; // -20 for padding
		const height = this.imageHeight
			? this.imageHeight
			: this.getVisibleHeight() - 20; // -20 for padding
		return { width, height };
	},

	/**
	 * Generates the image source URL.
	 * @param {number} width - the width of the image.
	 * @param {number} height - the height of the image.
	 * @param {string} contextValue - the context value.
	 * @param {string} baseValue - the base value.
	 * @returns {string} the generated image source URL.
	 */
	_generateImageSrc: function (width, height, contextValue, baseValue) {
		let src = `dynamic.${this.format}?_doc=${this.moduleDotDocument}&_n=${this.name}`;
		if (this.imageWidth) src += `&_w=${this.imageWidth}`;
		if (this.imageHeight) src += `&_h=${this.imageHeight}`;
		src += `&_w=${width}&_h=${height}&_wz=${this.wZoom}&_hz=${this.hZoom}`;
		if (contextValue) src += `&_c=${contextValue}`;
		if (baseValue) src += `&_b=${baseValue.replaceAll("_", ".")}`;
		src += `&_ts=${new Date().getTime()}`;
		return src;
	},

	/**
	 * Creates an image element with the specified dimensions and source.
	 * @param {number} width - the width of the image.
	 * @param {number} height - the height of the image.
	 * @param {string} src - the image source URL.
	 * @returns {Object} the created image element.
	 */
	_createImageElement: function (width, height, src) {
		return isc.Img.create({
			width: "100%",
			height: "100%",
			overflow: "hidden",
			imageWidth: Math.round((width * this.wZoom) / 100.0),
			imageHeight: Math.round((height * this.hZoom) / 100.0),
			imageType: "center",
			canDrag: true,
			cursor: "all-scroll",
			dragAppearance: "none",
			dragStart: function () {
				this.startScrollLeft = this.getScrollLeft();
				this.startScrollTop = this.getScrollTop();
			},
			dragMove: function () {
				this.scrollTo(
					this.startScrollLeft -
						isc.Event.lastEvent.x +
						isc.Event.mouseDownEvent.x,
					this.startScrollTop -
						isc.Event.lastEvent.y +
						isc.Event.mouseDownEvent.y,
				);
			},
			mouseWheel: () => {
				const wheelDelta = isc.EventHandler.getWheelDelta();
				this._zoom(Math.round(this.wZoom - wheelDelta * 10.0), true); // Throttle this event
				return false;
			},
			doubleClick: () => this._open(),
			appImgDir: "../",
			src,
		});
	},
});

/**
 * Implements the BizImage UI component.
 * Extends Img from the SmartClient library.
 */
isc.ClassFactory.defineClass("BizImage", "Img");

isc.BizImage.addMethods({
	initWidget: function () {
		this.imageType = "stretch";

		// Construct the image source URL using template literals
		this.src = `resources?_n=${this.file}&_doc=${this.modoc}&_b=null`;

		// Call the parent class's initWidget method
		this.Super("initWidget", arguments);
	},
});

/**
 * Implements the BizLabel UI component.
 * Extends Label from the SmartClient library.
 */
isc.ClassFactory.defineClass("BizLabel", "Label");

isc.BizLabel.addMethods({
	initWidget: function () {
		// Determine if the label should auto-fit based on width and height arguments
		this.autoFit = !(arguments[0].width || arguments[0].height);

		// Set the label's content if a value is provided
		if (this.value) {
			this.setContents(this.value);
		}

		// Set the label's alignment if textAlign is provided
		if (this.textAlign) {
			this.setAlign(this.textAlign);
		}

		// Call the parent class's initWidget method
		this.Super("initWidget", arguments);
	},
});

/**
 * Implements the BizChart UI component.
 * Extends VLayou from the SmartClient library.
 */
isc.ClassFactory.defineClass("BizChart", "Canvas");

isc.BizChart.addClassMethods({
	loadingChartJS: false,

	/**
	 * Loads Chart.js and its dependencies asynchronously.
	 * If Chart.js is already loading, it retries after a short delay.
	 */
	loadChartJS: function () {
		if (isc.BizChart.loadingChartJS) {
			setTimeout(() => isc.BizChart.loadChartJS(), 100);
		} else if (!window.Chart) {
			isc.BizChart.loadingChartJS = true;

			// Load moment.js and then Chart.js
			SKYVE.Util.loadJS(
				`jakarta.faces.resource/moment/moment.js.xhtml?ln=primefaces&v=${SKYVE.Util.v}`,
				() => {
					SKYVE.Util.loadJS(
						`jakarta.faces.resource/chartjs/chartjs.js.xhtml?ln=primefaces&v=${SKYVE.Util.v}`,
						() => {
							isc.BizChart.loadingChartJS = false;
						},
					);
				},
			);
		}
	},

	v: 0,
});

isc.BizChart.addMethods({
	init: function (config) {
		if (!window.Chart) {
			isc.BizChart.loadChartJS();
		}

		// Set default width and height if not provided
		if (!config.width) {
			this.width = "100%";
		}
		if (!config.height) {
			this.height = "100%";
		}

		// Generate a unique ID for the chart
		this.ID = `bizChart${isc.BizChart.v++}`;
		this.redrawOnResize = false;
		this._refreshing = false; // Prevent multiple refreshes
		this._resizeChartCalled = false; // Throttle resizing events

		// Call the parent class's init method
		this.Super("init", arguments);
	},

	/**
	 * Returns the inner HTML for the chart canvas.
	 * @returns {string} the HTML string for the canvas element.
	 */
	getInnerHTML: function () {
		return `<canvas id="${this.ID}_chart" />`;
	},

	/**
	 * Handles the resized event with throttling to avoid excessive calls.
	 */
	resized: function () {
		if (!this._resizeChartCalled) {
			this.delayCall("_resizeChart", arguments, 100);
		}
		this._resizeChartCalled = true;
	},

	/**
	 * Resizes the chart canvas to fit the widget's dimensions.
	 */
	_resizeChart: function () {
		if (this.chart) {
			const width = this.getWidth();
			const height = this.getHeight();
			const canvas = this.chart.canvas;

			// Update canvas dimensions
			canvas.parentNode.style.width = `${width}px`;
			canvas.parentNode.style.height = `${height}px`;
			canvas.width = width;
			canvas.height = height;
			canvas.style.width = `${width}px`;
			canvas.style.height = `${height}px`;

			// Update the chart
			this.chart.update();
			this._resizeChartCalled = false;
		} else {
			this.delayCall("_resizeChart", arguments, 100);
		}
	},

	/**
	 * Sets the data source for the chart.
	 * @param {string} modelName - the name of the model providing the data.
	 */
	setDataSource: function (modelName) {
		if (window.Chart && this.isDrawn()) {
			this._modelName = modelName;

			// Assign this chart to the edit view's _grids property
			const grids = this._view._grids[modelName] || {};
			this._view._grids[modelName] = grids;
			grids[this.getID()] = this;

			this._refresh();
		} else {
			this.delayCall("setDataSource", arguments, 100);
		}
	},

	/**
	 * Rerenders the chart by refreshing its data.
	 */
	rerender: function () {
		this._refresh();
	},

	/**
	 * Refreshes the chart data by making an XHR request to the server.
	 */
	_refresh: function () {
		if (this._refreshing || !this.isDrawn() || !this.isVisible()) {
			return; // Skip if already refreshing, not drawn, or invisible
		}

		const url = `${SKYVE.Util.CONTEXT_URL}chart?`;
		if (!this._modelName) {
			return; // Skip if no model name is set
		}

		const instance = this._view.gather(false);
		const requestUrl = `${url}_c=${instance._c}&t=${this.chartType}&_m=${this._modelName}`;

		// Ensure only one refresh occurs at a time
		this._refreshing = true;

		isc.RPCManager.sendRequest({
			showPrompt: true,
			evalResult: true,
			actionURL: requestUrl,
			httpMethod: "GET",
			callback: (rpcResponse, data) => {
				try {
					this._update(data);
				} finally {
					this._refreshing = false;
				}
			},
		});
	},

	/**
	 * Updates the chart with new data.
	 * @param {Object} data - the data received from the server.
	 */
	_update: function (data) {
		if (data.config) {
			// Initialize chart config if not already set
			this.chartConfig = this.chartConfig || {};
			this.chartConfig.type = data.config.type;
			this.chartConfig.data = data.config.data;
			this.chartConfig.options = data.config.options || {};

			// Ensure responsive and aspect ratio settings
			this.chartConfig.options.responsive = true;
			this.chartConfig.options.maintainAspectRatio = false;

			if (this.chart) {
				this.chart.update(); // Update existing chart
			} else {
				if (!this.isDrawn()) {
					this.draw(); // Ensure the widget is drawn
				}
				const chartCanvas = document.getElementById(`${this.ID}_chart`);
				this.chart = new Chart(chartCanvas, this.chartConfig); // Create new chart
			}
		}
	},
});

/**
 * Implements the BizProgressBar UI component.
 * Extends ProgressBar from the SmartClient library.
 */
isc.ClassFactory.defineClass("BizProgressBar", "ProgressBar");
