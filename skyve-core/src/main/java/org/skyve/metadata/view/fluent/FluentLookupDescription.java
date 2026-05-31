package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;

/**
 * Builds {@link LookupDescription} widget metadata using a fluent API.
 */
public class FluentLookupDescription extends FluentInputWidget<FluentLookupDescription>
		implements FluentAbsoluteWidth<FluentLookupDescription> {
	private LookupDescription lookup = null;

	/**
	 * Creates a fluent builder backed by a new {@link LookupDescription} metadata instance.
	 */
	public FluentLookupDescription() {
		lookup = new LookupDescription();
	}

	/**
	 * Creates a fluent builder backed by the supplied {@link LookupDescription} metadata instance.
	 *
	 * @param lookup the metadata instance to mutate
	 */
	public FluentLookupDescription(LookupDescription lookup) {
		this.lookup = lookup;
	}

	/**
	 * Copies lookup metadata, actions, parameters, and drop-down columns into this builder.
	 */
	public FluentLookupDescription from(@SuppressWarnings("hiding") LookupDescription lookup) {
		descriptionBinding(lookup.getDescriptionBinding());
		query(lookup.getQuery());
		disableEditConditionName(lookup.getDisableEditConditionName());
		disableAddConditionName(lookup.getDisableAddConditionName());
		disableClearConditionName(lookup.getDisableClearConditionName());
		disablePickConditionName(lookup.getDisablePickConditionName());
		Boolean b = lookup.getEditable();
		if (b != null) {
			editable(b.booleanValue());
		}

		absoluteWidth(lookup, this);

		lookup.getAddedActions().forEach(a -> addAddedAction(FluentEventAction.from(a)));
		lookup.getClearedActions().forEach(c -> addClearedAction(FluentEventAction.from(c)));
		lookup.getEditedActions().forEach(e -> addEditedAction(FluentEventAction.from(e)));
		lookup.getPickedActions().forEach(p -> addPickedAction(FluentEventAction.from(p)));

		lookup.getParameters().forEach(p -> addParameter(new FluentParameter().from(p)));
		lookup.getFilterParameters().forEach(f -> addFilterParameter(new FluentFilterParameter().from(f)));
		lookup.getDropDownColumns().forEach(d -> addDropDownColumn(new FluentLookupDescriptionColumn().from(d)));

		super.from(lookup);
		return this;
	}

	/**
	 * Sets the binding that provides the description text.
	 *
	 * @param descriptionBinding the description binding expression
	 * @return this builder
	 */
	public FluentLookupDescription descriptionBinding(String descriptionBinding) {
		lookup.setDescriptionBinding(descriptionBinding);
		return this;
	}

	/**
	 * Sets the query name used to populate this lookup.
	 *
	 * @param query the query name
	 * @return this builder
	 */
	public FluentLookupDescription query(String query) {
		lookup.setQuery(query);
		return this;
	}

	/**
	 * Sets the condition name that disables the edit button.
	 *
	 * @param disableEditConditionName the condition name
	 * @return this builder
	 */
	public FluentLookupDescription disableEditConditionName(String disableEditConditionName) {
		lookup.setDisableEditConditionName(disableEditConditionName);
		return this;
	}

	/**
	 * Sets the condition name that disables the add button.
	 *
	 * @param disableAddConditionName the condition name
	 * @return this builder
	 */
	public FluentLookupDescription disableAddConditionName(String disableAddConditionName) {
		lookup.setDisableAddConditionName(disableAddConditionName);
		return this;
	}

	/**
	 * Sets the condition name that disables the clear button.
	 *
	 * @param disableClearConditionName the condition name
	 * @return this builder
	 */
	public FluentLookupDescription disableClearConditionName(String disableClearConditionName) {
		lookup.setDisableClearConditionName(disableClearConditionName);
		return this;
	}

	/**
	 * Sets the condition name that disables the pick button.
	 *
	 * @param disablePickConditionName the condition name
	 * @return this builder
	 */
	public FluentLookupDescription disablePickConditionName(String disablePickConditionName) {
		lookup.setDisablePickConditionName(disablePickConditionName);
		return this;
	}

	/**
	 * Sets whether this lookup description widget is editable.
	 *
	 * @param editable {@code true} to allow editing
	 * @return this builder
	 */
	public FluentLookupDescription editable(boolean editable) {
		lookup.setEditable(editable ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Appends a drop-down column definition to the wrapped lookup metadata.
	 */
	public FluentLookupDescription addDropDownColumn(FluentLookupDescriptionColumn column) {
		lookup.getDropDownColumns().add(column.get());
		return this;
	}

	/**
	 * Appends an edited action definition to the wrapped lookup metadata.
	 */
	public FluentLookupDescription addEditedAction(FluentEventAction action) {
		lookup.getEditedActions().add(action.get());
		return this;
	}

	/**
	 * Appends an added action definition to the wrapped lookup metadata.
	 */
	public FluentLookupDescription addAddedAction(FluentEventAction action) {
		lookup.getAddedActions().add(action.get());
		return this;
	}

	/**
	 * Appends a cleared action definition to the wrapped lookup metadata.
	 */
	public FluentLookupDescription addClearedAction(FluentEventAction action) {
		lookup.getClearedActions().add(action.get());
		return this;
	}

	/**
	 * Appends a picked action definition to the wrapped lookup metadata.
	 */
	public FluentLookupDescription addPickedAction(FluentEventAction action) {
		lookup.getPickedActions().add(action.get());
		return this;
	}

	/**
	 * Appends a filter parameter definition to the wrapped lookup metadata.
	 */
	public FluentLookupDescription addFilterParameter(FluentFilterParameter filterParameter) {
		lookup.getFilterParameters().add(filterParameter.get());
		return this;
	}

	/**
	 * Appends a parameter definition to the wrapped lookup metadata.
	 */
	public FluentLookupDescription addParameter(FluentParameter parameter) {
		lookup.getParameters().add(parameter.get());
		return this;
	}

	/**
	 * Sets the pixel width of this lookup description widget.
	 *
	 * @param width the pixel width
	 * @return this builder
	 */
	@Override
	public FluentLookupDescription pixelWidth(int width) {
		lookup.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	/**
	 * Returns the wrapped {@link LookupDescription} metadata instance.
	 *
	 * @return the mutable lookup-description metadata being configured
	 */
	@Override
	public LookupDescription get() {
		return lookup;
	}
}
