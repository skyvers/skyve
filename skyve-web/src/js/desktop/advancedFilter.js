/**
 * Manages advanced filtering logic in relevant UI components.
 * Extends VLayout from the SmartClient library.
 */
isc.defineClass("AdvancedFilter", "VLayout");

isc.AdvancedFilter.addProperties({
	// VLayout properties
	width: "100%",
	height: 1,
	overflow: "visible",
	membersMargin: 2,
	margin: 2,

	// Filter editor and filter builder configurations (e.g., BizListGrid, BizTreeGrid)
	filterableComponent: null,
	filterableComponentConfig: null,
	toggleButton: null,
	_filterBuilder: null,
	_filterButtonDefaults: null,
});

isc.AdvancedFilter.addMethods({
	
	initWidget: function () {
		this._filterButtonDefaults = {
			_constructor: isc.IButton,
			autoFit: true,
			title: "Filter",
			icon: "icons/filter_add.png",
		};

		this.Super("initWidget", arguments);

		this.toggleButton = isc.ToolStripButton.create({
			icon: "icons/filter_add.png",
			actionType: "checkbox",
			showFocused: false,
			showDown: false,
			showSelectedIcon: false,
			selected: false,
			canHover: true,
			getHoverHTML: function () {
				return this.selected
					? "Use <b>simple</b> filtering"
					: "Use <b>advanced</b> filtering";
			},
		});

		this._styleForm = isc.DynamicForm.create({
			numCols: 2,
			width: 200,
			items: [
				{
					name: "style",
					title: "Style",
					type: "radioGroup",
					vertical: false,
					required: true,
					valueMap: { radio: "Flat", bracket: "Nested", inline: "Inline" },
					defaultValue: "radio",
					changed: (form, item, value) => {
						this.setDataSource(this._filterBuilder.getDataSource());
					},
				},
			],
		});

		this.addMember(this._styleForm);
	},

	/**
	 * Gets the current filter style.
	 * @returns {string} the current filter style.
	 */
	getStyle: function () {
		return this._styleForm.getValue("style");
	},

	/**
	 * Sets the filter style.
	 * @param {string} style - the new filter style to apply.
	 */
	setStyle: function (style) {
		this._styleForm.setValue("style", style);
		this.setDataSource(this._filterBuilder.getDataSource());
	},

	/**
	 * Handles the toggle button click event to switch between simple and advanced filtering modes.
	 *
	 * **Advanced Filtering:**
	 *   - Suppresses automatic filtering by temporarily overriding `getFilterEditorCriteria`.
	 *   - Applies advanced criteria from the filter builder.
	 *   - Ensures the filter panel is visible.
	 *
	 * **Simple Filtering:**
	 *   - Clears advanced criteria.
	 *   - Resets the grid's filter editor.
	 *   - Hides the advanced filter panel.
	 */
	toggleButtonClick: function () {
		const { grid } = this.filterableComponent;

		if (this.toggleButton.selected) {
			// Suppress SmartClient's automatic filtering during filter editor toggle
			const originalGetCriteria = grid.getFilterEditorCriteria;
			grid.getFilterEditorCriteria = () => null;
			grid.setFilterEditorCriteria(isc.emptyObject);
			grid.setShowFilterEditor(false);
			grid.getFilterEditorCriteria = originalGetCriteria;

			// Apply advanced filtering using criteria from the filter builder
			grid.filterData(this._filterBuilder.getCriteria());

			// Ensure the panel is visible for proper interaction
			this.show();
		} else {
			// Clear advanced criteria and reset the grid filter editor
			grid.setShowFilterEditor(true);
			grid.setFilterEditorCriteria({});
			grid.filterData({});

			// Hide the advanced filter panel
			this.hide();
		}
	},

	/**
	 * Sets the data source for the filter builder.
	 * @param {Object} dataSource - the data source for the filter.
	 */
	setDataSource: function (dataSource) {
		if (this._filterBuilder) {
			// Clear existing filter builder
			this.removeMember(this._filterBuilder);
			this._filterBuilder.destroy();
			this._filterBuilder = null;
		} else {
			// Initialise the filter button button
			this._filterButton = this.createAutoChild("_filterButton", {
				click: () => {
					this.filterableComponent.grid.filterData(
						this._filterBuilder.getCriteria(),
						null,
						{ params: { _summary: this.filterableComponent.summaryType } },
					);
				},
			});
			this.addMember(this._filterButton);
		}

		// Get selected filter style (or default)
		const style = this.getStyle() || "radio";

		// Create new filter builder with the provided data source and selected style
		this._filterBuilder = isc.FilterBuilder.create({
			dataSource,
			topOperatorAppearance: style,
			allowEmpty: true,
		});

		// Add the new filter builder to the layout and manage visibility
		this.addMember(this._filterBuilder, 1);
		this.toggleButton.selected ? this.show() : this.hide();
	},

	/**
	 * Retrieves the filter criteria.
	 * @param {boolean} includeEmptyValues - whether to include empty values.
	 * @returns {Object|null} the filter criteria or null if not set.
	 */
	getCriteria: function (includeEmptyValues) {
		return this._filterBuilder
			? this._filterBuilder.getCriteria(includeEmptyValues)
			: null;
	},

	/**
	 * Clears all filter criteria.
	 */
	clearCriteria: function () {
		if (this._filterBuilder) {
			this._filterBuilder.clearCriteria();
		}
	},

	/**
	 * Sets the filter criteria.
	 * @param {Object} criteria - the criteria to set.
	 */
	setCriteria: function (criteria) {
		if (this._filterBuilder) {
			this._filterBuilder.setCriteria(criteria);
		}
	},
});
