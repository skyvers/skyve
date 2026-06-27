package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.validator.TextValidator;
import org.skyve.impl.metadata.model.document.field.validator.TextValidator.ValidatorType;

/**
 * Provides a fluent builder for FluentTextValidator metadata.
 */
public class FluentTextValidator {
	private TextValidator validator = null;
	
	/**
	 * Creates a fluent builder instance.
	 */
	public FluentTextValidator() {
		validator = new TextValidator();
	}

	/**
	 * Creates a fluent builder instance.
	 */
	public FluentTextValidator(TextValidator validator) {
		this.validator = validator;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public FluentTextValidator from(@SuppressWarnings("hiding") TextValidator validator) {
		type(validator.getType());
		regularExpression(validator.getRegularExpression());
		validationMessage(validator.getValidationMessage());
		return this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentTextValidator type(ValidatorType type) {
		validator.setType(type);
		return this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentTextValidator regularExpression(String regularExpression) {
		validator.setRegularExpression(regularExpression);
		return this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentTextValidator validationMessage(String validationMessage) {
		validator.setValidationMessage(validationMessage);
		return this;
	}
	/**
	 * Returns the mutable metadata instance represented by this builder.
	 */
	public TextValidator get() {
		return validator;
	}
}
