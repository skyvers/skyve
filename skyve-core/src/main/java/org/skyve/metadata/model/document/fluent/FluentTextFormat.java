package org.skyve.metadata.model.document.fluent;

import org.skyve.domain.types.converters.Format.TextCase;
import org.skyve.impl.metadata.model.document.field.TextFormat;

public class FluentTextFormat {
	private TextFormat format = null;
	
	public FluentTextFormat() {
		format = new TextFormat();
	}

	public FluentTextFormat(TextFormat format) {
		this.format = format;
	}

	public FluentTextFormat from(@SuppressWarnings("hiding") TextFormat format) {
		mask(format.getMask());
		textCase(format.getCase());
		return this;
	}
	
	public FluentTextFormat mask(String mask) {
		format.setMask(mask);
		return this;
	}
	
	public FluentTextFormat textCase(TextCase textCase) {
		format.setCase(textCase);
		return this;
	}
	
	public TextFormat get() {
		return format;
	}
}
