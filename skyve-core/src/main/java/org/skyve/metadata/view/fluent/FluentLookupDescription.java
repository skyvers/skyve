package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;

public class FluentLookupDescription extends FluentInputWidget<FluentLookupDescription>
		implements FluentAbsoluteWidth<FluentLookupDescription> {
	private LookupDescription lookup = null;

	public FluentLookupDescription() {
		lookup = new LookupDescription();
	}

	public FluentLookupDescription(LookupDescription lookup) {
		this.lookup = lookup;
	}

	public FluentLookupDescription from(@SuppressWarnings("hiding") LookupDescription lookup) {
		descriptionBinding(lookup.getDescriptionBinding());
		query(lookup.getQuery());
		disableEditConditionName(lookup.getDisableEditConditionName());
		disableAddConditionName(lookup.getDisableAddConditionName());
		disableClearConditionName(lookup.getDisableClearConditionName());
		disablePickConditionName(lookup.getDisablePickConditionName());
		editable(lookup.getEditable());

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

	public FluentLookupDescription descriptionBinding(String descriptionBinding) {
		lookup.setDescriptionBinding(descriptionBinding);
		return this;
	}

	public FluentLookupDescription query(String query) {
		lookup.setQuery(query);
		return this;
	}

	public FluentLookupDescription disableEditConditionName(String disableEditConditionName) {
		lookup.setDisableEditConditionName(disableEditConditionName);
		return this;
	}

	public FluentLookupDescription disableAddConditionName(String disableAddConditionName) {
		lookup.setDisableAddConditionName(disableAddConditionName);
		return this;
	}

	public FluentLookupDescription disableClearConditionName(String disableClearConditionName) {
		lookup.setDisableClearConditionName(disableClearConditionName);
		return this;
	}

	public FluentLookupDescription disablePickConditionName(String disablePickConditionName) {
		lookup.setDisablePickConditionName(disablePickConditionName);
		return this;
	}

	public FluentLookupDescription editable(boolean editable) {
		lookup.setEditable(editable ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentLookupDescription addDropDownColumn(FluentLookupDescriptionColumn column) {
		lookup.getDropDownColumns().add(column.get());
		return this;
	}

	public FluentLookupDescription addEditedAction(FluentEventAction action) {
		lookup.getEditedActions().add(action.get());
		return this;
	}

	public FluentLookupDescription addAddedAction(FluentEventAction action) {
		lookup.getAddedActions().add(action.get());
		return this;
	}

	public FluentLookupDescription addClearedAction(FluentEventAction action) {
		lookup.getClearedActions().add(action.get());
		return this;
	}

	public FluentLookupDescription addPickedAction(FluentEventAction action) {
		lookup.getPickedActions().add(action.get());
		return this;
	}

	public FluentLookupDescription addFilterParameter(FluentFilterParameter filterParameter) {
		lookup.getFilterParameters().add(filterParameter.get());
		return this;
	}

	public FluentLookupDescription addParameter(FluentParameter parameter) {
		lookup.getParameters().add(parameter.get());
		return this;
	}

	@Override
	public FluentLookupDescription pixelWidth(int width) {
		lookup.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	@Override
	public LookupDescription get() {
		return lookup;
	}
}
