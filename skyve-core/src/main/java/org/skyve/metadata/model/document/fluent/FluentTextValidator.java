package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.validator.TextValidator;
import org.skyve.impl.metadata.model.document.field.validator.TextValidator.ValidatorType;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

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
	public FluentTextValidator(@Nonnull TextValidator validator) {
		this.validator = validator;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public @Nonnull FluentTextValidator from(@SuppressWarnings("hiding") @Nonnull TextValidator validator) {
		type(validator.getType());
		regularExpression(validator.getRegularExpression());
		validationMessage(validator.getValidationMessage());
		return this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	public @Nonnull FluentTextValidator type(@Nullable ValidatorType type) {
		validator.setType(type);
		return this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	public @Nonnull FluentTextValidator regularExpression(@Nullable String regularExpression) {
		validator.setRegularExpression(regularExpression);
		return this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	public @Nonnull FluentTextValidator validationMessage(@Nullable String validationMessage) {
		validator.setValidationMessage(validationMessage);
		return this;
	}
	/**
	 * Returns the mutable metadata instance represented by this builder.
	 */
	public @Nonnull TextValidator get() {
		return validator;
	}
}
