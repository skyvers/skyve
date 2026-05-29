package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.repository.document.FieldReference;
import org.skyve.impl.metadata.repository.document.UniqueConstraint;
import org.skyve.metadata.model.document.UniqueConstraint.DocumentScope;

/**
 * Provides a fluent builder for FluentDocumentUniqueConstraint metadata.
 */
public class FluentDocumentUniqueConstraint {
	private UniqueConstraint constraint = null;
	
	/**
	 * Creates a fluent builder instance.
	 */
	public FluentDocumentUniqueConstraint() {
		constraint = new UniqueConstraint();
	}

	/**
	 * Creates a fluent builder instance.
	 */
	public FluentDocumentUniqueConstraint(UniqueConstraint constraint) {
		this.constraint = constraint;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public FluentDocumentUniqueConstraint from(@SuppressWarnings("hiding") org.skyve.metadata.model.document.UniqueConstraint constraint) {
		name(constraint.getName());
		description(constraint.getDescription());
		scope(constraint.getScope());
		message(constraint.getMessage());
		constraint.getFieldNames().forEach(this::addFieldName);
		return this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentDocumentUniqueConstraint name(String name) {
		constraint.setName(name);
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentDocumentUniqueConstraint description(String description) {
		constraint.setDescription(description);
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentDocumentUniqueConstraint scope(DocumentScope scope) {
		constraint.setScope(scope);
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentDocumentUniqueConstraint message(String message) {
		constraint.setMessage(message);
		return this;
	}

	/**
	 * Appends a constrained field reference by attribute name.
	 */
	public FluentDocumentUniqueConstraint addFieldName(String fieldName) {
		FieldReference ref = new FieldReference();
		ref.setRef(fieldName);
		constraint.getFieldReferences().add(ref);
		return this;
	}

	/**
	 * Removes constrained field references with the supplied name.
	 */
	public FluentDocumentUniqueConstraint removeFieldName(String name) {
		constraint.getFieldReferences().removeIf(f -> name.equals(f.getRef()));
		return this;
	}

	/**
	 * Clears all constrained field references.
	 */
	public FluentDocumentUniqueConstraint clearFieldNames() {
		constraint.getFieldReferences().clear();
		return this;
	}

	/**
	 * Returns the mutable document unique-constraint metadata being built.
	 */
	public UniqueConstraint get() {
		return constraint;
	}
}
