package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.ListMembership;

public class FluentListMembership extends FluentInputWidget<FluentListMembership> {
	private ListMembership list = null;

	public FluentListMembership() {
		list = new ListMembership();
	}

	public FluentListMembership(ListMembership list) {
		this.list = list;
	}

	public FluentListMembership from(@SuppressWarnings("hiding") ListMembership list) {

		list.getChangedActions().forEach(c -> addChangedAction(FluentEventAction.from(c)));

		super.from(list);
		return this;
	}

	public FluentListMembership addChangedAction(FluentEventAction action) {
		list.getChangedActions().add(action.get());
		return this;
	}

	@Override
	public ListMembership get() {
		return list;
	}
}
