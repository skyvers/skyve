package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Markup;

public class FluentMarkup extends FluentConstrainableField<FluentMarkup> {
	private Markup markup = new Markup();
	
	public FluentMarkup() {
		// nothing to see
	}

	public FluentMarkup(Markup markup) {
		super(markup);
	}
	
	@Override
	public Markup get() {
		return markup;
	}
}
