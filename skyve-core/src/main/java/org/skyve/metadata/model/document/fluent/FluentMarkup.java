package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Markup;

public class FluentMarkup extends FluentConstrainableField<FluentMarkup> {
	private Markup markup = null;
	
	public FluentMarkup() {
		markup = new Markup();
	}

	public FluentMarkup(Markup markup) {
		this.markup = markup;
	}

	public FluentMarkup from(@SuppressWarnings("hiding") Markup markup) {
		super.from(markup);
		return this;
	}
	
	@Override
	public Markup get() {
		return markup;
	}
}
