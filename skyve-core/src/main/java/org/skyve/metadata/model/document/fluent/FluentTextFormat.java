package org.skyve.metadata.model.document.fluent;

import org.skyve.domain.types.converters.Format.TextCase;
import org.skyve.impl.metadata.model.document.field.TextFormat;

public class FluentTextFormat {
	private TextFormat format = new TextFormat();
	
	public FluentTextFormat() {
		// nothing to see
	}
	
	public FluentTextFormat(TextFormat format) {
		mask(format.getMask());
		textCase(format.getCase());
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
