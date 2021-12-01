package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.ListMembership;

public class FluentListMembership extends FluentWidget {
	private ListMembership list = null;
	
	public FluentListMembership() {
		list = new ListMembership();
	}

	public FluentListMembership(ListMembership list) {
		this.list = list;
	}

	public FluentListMembership from(@SuppressWarnings("hiding") ListMembership list) {
		return this;
	}

	@Override
	public ListMembership get() {
		return list;
	}
}
