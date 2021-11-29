package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.impl.metadata.model.document.field.TextFormat;
import org.skyve.impl.metadata.model.document.field.validator.TextValidator;

public class FluentText extends FluentConvertableField<FluentText> {
	private Text text = new Text();
	
	public FluentText() {
		// nothing to see
	}
	
	public FluentText(Text text) {
		super(text);
		length(text.getLength());
		TextFormat format = text.getFormat();
		if (format != null) {
			format(new FluentTextFormat(format));
		}
		TextValidator validator = text.getValidator();
		if (validator != null) {
			validator(new FluentTextValidator(validator));
		}
	}
	
	public FluentText length(int length) {
		text.setLength(length);
		return this;
	}
	
	public FluentText format(FluentTextFormat format) {
		text.setFormat(format.get());
		return this;
	}
	
	public FluentText validator(FluentTextValidator validator) {
		text.setValidator(validator.get());
		return this;
	}

	@Override
	public Text get() {
		return text;
	}
}
