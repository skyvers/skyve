package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.UniqueConstraintImpl;
import org.skyve.metadata.model.document.UniqueConstraint;
import org.skyve.metadata.model.document.UniqueConstraint.DocumentScope;

import jakarta.annotation.Nonnull;

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
		constraint.getProperties().entrySet().forEach(p -> putProperty(p.getKey(), p.getValue()));

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
	 * Adds or replaces a custom property on the wrapped metadata.
	 * Properties keep the order in which they were first added.
	 *
	 * @param k the property name
	 * @param v the property value
	 * @return this builder
	 */
	public @Nonnull FluentCollectionUniqueConstraint putProperty(@Nonnull String k, @Nonnull String v) {
		constraint.getProperties().put(k, v);
		return this;
	}

	/**
	 * Returns the mutable collection unique-constraint metadata being built.
	 */
	public UniqueConstraintImpl get() {
		return constraint;
	}
}
