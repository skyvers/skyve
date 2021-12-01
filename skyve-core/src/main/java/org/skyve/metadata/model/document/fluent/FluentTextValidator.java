package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.validator.TextValidator;
import org.skyve.impl.metadata.model.document.field.validator.TextValidator.ValidatorType;

public class FluentTextValidator {
	private TextValidator validator = null;
	
	public FluentTextValidator() {
		validator = new TextValidator();
	}

	public FluentTextValidator(TextValidator validator) {
		this.validator = validator;
	}

	public FluentTextValidator from(@SuppressWarnings("hiding") TextValidator validator) {
		type(validator.getType());
		regularExpression(validator.getRegularExpression());
		validationMessage(validator.getValidationMessage());
		return this;
	}
	
	public FluentTextValidator type(ValidatorType type) {
		validator.setType(type);
		return this;
	}
	
	public FluentTextValidator regularExpression(String regularExpression) {
		validator.setRegularExpression(regularExpression);
		return this;
	}
	
	public FluentTextValidator validationMessage(String validationMessage) {
		validator.setValidationMessage(validationMessage);
		return this;
	}
	
	public TextValidator get() {
		return validator;
	}
}
