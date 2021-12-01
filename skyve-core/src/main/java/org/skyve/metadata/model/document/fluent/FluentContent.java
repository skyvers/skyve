package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Content;

public class FluentContent extends FluentConstrainableField<FluentContent> {
	private Content content = null;
	
	public FluentContent() {
		content = new Content();
	}

	public FluentContent(Content content) {
		this.content = content;
	}

	public FluentContent from(@SuppressWarnings("hiding") Content content) {
		super.from(content);
		return this;
	}
	
	@Override
	public Content get() {
		return content;
	}
}
