package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.repository.document.FieldReference;
import org.skyve.impl.metadata.repository.document.UniqueConstraint;
import org.skyve.metadata.model.document.UniqueConstraint.DocumentScope;

public class FluentDocumentUniqueConstraint {
	private UniqueConstraint constraint = null;
	
	public FluentDocumentUniqueConstraint() {
		constraint = new UniqueConstraint();
	}

	public FluentDocumentUniqueConstraint(UniqueConstraint constraint) {
		this.constraint = constraint;
	}

	public FluentDocumentUniqueConstraint from(@SuppressWarnings("hiding") org.skyve.metadata.model.document.UniqueConstraint constraint) {
		name(constraint.getName());
		description(constraint.getDescription());
		scope(constraint.getScope());
		message(constraint.getMessage());
		constraint.getFieldNames().forEach(c -> addFieldName(c));
		return this;
	}
	
	public FluentDocumentUniqueConstraint name(String name) {
		constraint.setName(name);
		return this;
	}

	public FluentDocumentUniqueConstraint description(String description) {
		constraint.setDescription(description);
		return this;
	}

	public FluentDocumentUniqueConstraint scope(DocumentScope scope) {
		constraint.setScope(scope);
		return this;
	}

	public FluentDocumentUniqueConstraint message(String message) {
		constraint.setMessage(message);
		return this;
	}
	
	public FluentDocumentUniqueConstraint addFieldName(String fieldName) {
		FieldReference ref = new FieldReference();
		ref.setRef(fieldName);
		constraint.getFieldReferences().add(ref);
		return this;
	}

	public UniqueConstraint get() {
		return constraint;
	}
}
