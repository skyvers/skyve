package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.UniqueConstraintImpl;
import org.skyve.metadata.model.document.UniqueConstraint;
import org.skyve.metadata.model.document.UniqueConstraint.DocumentScope;

/**
 * Provides a fluent builder for FluentCollectionUniqueConstraint metadata.
 */
public class FluentCollectionUniqueConstraint {
	private UniqueConstraintImpl constraint = null;
	
	/**
	 * Creates a fluent builder instance.
	 */
	public FluentCollectionUniqueConstraint() {
		constraint = new UniqueConstraintImpl();
	}

	/**
	 * Creates a fluent builder instance.
	 */
	public FluentCollectionUniqueConstraint(UniqueConstraintImpl constraint) {
		this.constraint = constraint;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public FluentCollectionUniqueConstraint from(@SuppressWarnings("hiding") UniqueConstraint constraint) {
		name(constraint.getName());
		description(constraint.getDescription());
		scope(constraint.getScope());
		message(constraint.getMessage());
		constraint.getFieldNames().forEach(f -> addFieldName(f));
		return this;
	}	

	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentCollectionUniqueConstraint name(String name) {
		constraint.setName(name);
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentCollectionUniqueConstraint description(String description) {
		constraint.setDescription(description);
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentCollectionUniqueConstraint scope(DocumentScope scope) {
		constraint.setScope(scope);
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentCollectionUniqueConstraint message(String message) {
		constraint.setMessage(message);
		return this;
	}

	/**
	 * Appends a constrained field name.
	 */
	public FluentCollectionUniqueConstraint addFieldName(String fieldName) {
		constraint.getFieldNames().add(fieldName);
		return this;
	}

	/**
	 * Removes constrained field names matching {@code fieldName}.
	 */
	public FluentCollectionUniqueConstraint removeFieldName(String fieldName) {
		constraint.getFieldNames().remove(fieldName);
		return this;
	}

	/**
	 * Clears all constrained field names.
	 */
	public FluentCollectionUniqueConstraint clearFieldNames() {
		constraint.getFieldNames().clear();
		return this;
	}

	/**
	 * Returns the mutable collection unique-constraint metadata being built.
	 */
	public UniqueConstraintImpl get() {
		return constraint;
	}
}
