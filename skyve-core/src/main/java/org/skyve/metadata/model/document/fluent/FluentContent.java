package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Content;

public class FluentContent extends FluentConstrainableField<FluentContent> {
	private Content content = new Content();
	
	public FluentContent() {
		// nothing to see
	}
	
	public FluentContent(Content content) {
		super(content);
	}
	
	@Override
	public Content get() {
		return content;
	}
}
