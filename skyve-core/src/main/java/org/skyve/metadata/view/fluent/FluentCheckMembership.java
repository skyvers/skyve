package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.CheckMembership;

public class FluentCheckMembership extends FluentWidget {
	private CheckMembership check = null;
	
	public FluentCheckMembership() {
		check = new CheckMembership();
	}

	public FluentCheckMembership(CheckMembership check) {
		this.check = check;
	}

	public FluentCheckMembership from(@SuppressWarnings("hiding") CheckMembership check) {
		return this;
	}

	@Override
	public CheckMembership get() {
		return check;
	}
}
