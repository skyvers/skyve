package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;

public class FluentLookupDescription extends FluentInputWidget<FluentLookupDescription> {
	private LookupDescription lookup = null;
	
	public FluentLookupDescription() {
		lookup = new LookupDescription();
	}

	public FluentLookupDescription(LookupDescription lookup) {
		this.lookup = lookup;
	}

	public FluentLookupDescription from(@SuppressWarnings("hiding") LookupDescription lookup) {
		super.from(lookup);
		return this;
	}

	@Override
	public LookupDescription get() {
		return lookup;
	}
}
