package org.skyve.metadata.view.fluent;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

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
		modelName(lookup.getModelName());
		BindUtil.copyAuthoredCondition(lookup.getXmlDisableEditCondition(), lookup.getXmlEnableEditCondition(), this::disableEditCondition, this::enableEditCondition);
		BindUtil.copyAuthoredCondition(lookup.getXmlDisableAddCondition(), lookup.getXmlEnableAddCondition(), this::disableAddCondition, this::enableAddCondition);
		BindUtil.copyAuthoredCondition(lookup.getXmlDisableClearCondition(), lookup.getXmlEnableClearCondition(), this::disableClearCondition, this::enableClearCondition);
		BindUtil.copyAuthoredCondition(lookup.getXmlDisablePickCondition(), lookup.getXmlEnablePickCondition(), this::disablePickCondition, this::enablePickCondition);
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
	 * Sets the list model owned by the enclosing view's document.
	 *
	 * @param modelName the model name
	 * @return this builder
	 */
	public FluentLookupDescription modelName(String modelName) {
		lookup.setModelName(modelName);
		return this;
	}

	/**
	 * Sets the {@code disableEdit} condition as authored; the opposite sense of {@link #enableEditCondition(String)}.
	 *
	 * @param disableEditCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	public @Nonnull FluentLookupDescription disableEditCondition(@Nullable String disableEditCondition) {
		lookup.setXmlDisableEditCondition(disableEditCondition);
		return this;
	}

	/**
	 * Sets the {@code enableEdit} condition as authored; the opposite sense of {@link #disableEditCondition(String)}.
	 *
	 * @param enableEditCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	public @Nonnull FluentLookupDescription enableEditCondition(@Nullable String enableEditCondition) {
		lookup.setXmlEnableEditCondition(enableEditCondition);
		return this;
	}

	/**
	 * Sets the {@code disableAdd} condition as authored; the opposite sense of {@link #enableAddCondition(String)}.
	 *
	 * @param disableAddCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	public @Nonnull FluentLookupDescription disableAddCondition(@Nullable String disableAddCondition) {
		lookup.setXmlDisableAddCondition(disableAddCondition);
		return this;
	}

	/**
	 * Sets the {@code enableAdd} condition as authored; the opposite sense of {@link #disableAddCondition(String)}.
	 *
	 * @param enableAddCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	public @Nonnull FluentLookupDescription enableAddCondition(@Nullable String enableAddCondition) {
		lookup.setXmlEnableAddCondition(enableAddCondition);
		return this;
	}

	/**
	 * Sets the {@code disableClear} condition as authored; the opposite sense of {@link #enableClearCondition(String)}.
	 *
	 * @param disableClearCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	public @Nonnull FluentLookupDescription disableClearCondition(@Nullable String disableClearCondition) {
		lookup.setXmlDisableClearCondition(disableClearCondition);
		return this;
	}

	/**
	 * Sets the {@code enableClear} condition as authored; the opposite sense of {@link #disableClearCondition(String)}.
	 *
	 * @param enableClearCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	public @Nonnull FluentLookupDescription enableClearCondition(@Nullable String enableClearCondition) {
		lookup.setXmlEnableClearCondition(enableClearCondition);
		return this;
	}

	/**
	 * Sets the {@code disablePick} condition as authored; the opposite sense of {@link #enablePickCondition(String)}.
	 *
	 * @param disablePickCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	public @Nonnull FluentLookupDescription disablePickCondition(@Nullable String disablePickCondition) {
		lookup.setXmlDisablePickCondition(disablePickCondition);
		return this;
	}

	/**
	 * Sets the {@code enablePick} condition as authored; the opposite sense of {@link #disablePickCondition(String)}.
	 *
	 * @param enablePickCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	public @Nonnull FluentLookupDescription enablePickCondition(@Nullable String enablePickCondition) {
		lookup.setXmlEnablePickCondition(enablePickCondition);
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
