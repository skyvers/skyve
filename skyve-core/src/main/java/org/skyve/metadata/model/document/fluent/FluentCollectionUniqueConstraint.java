package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.UniqueConstraintImpl;
import org.skyve.metadata.model.document.UniqueConstraint;
import org.skyve.metadata.model.document.UniqueConstraint.DocumentScope;

public class FluentCollectionUniqueConstraint {
	private UniqueConstraintImpl constraint = null;
	
	public FluentCollectionUniqueConstraint() {
		constraint = new UniqueConstraintImpl();
	}

	public FluentCollectionUniqueConstraint(UniqueConstraintImpl constraint) {
		this.constraint = constraint;
	}

	public FluentCollectionUniqueConstraint from(@SuppressWarnings("hiding") UniqueConstraint constraint) {
		description(constraint.getDescription());
		scope(constraint.getScope());
		message(constraint.getMessage());
		constraint.getFieldNames().forEach(f -> addFieldName(f));
		return this;
	}	

	public FluentCollectionUniqueConstraint description(String description) {
		constraint.setDescription(description);
		return this;
	}

	public FluentCollectionUniqueConstraint scope(DocumentScope scope) {
		constraint.setScope(scope);
		return this;
	}

	public FluentCollectionUniqueConstraint message(String message) {
		constraint.setMessage(message);
		return this;
	}
	
	public FluentCollectionUniqueConstraint addFieldName(String fieldName) {
		constraint.getFieldNames().add(fieldName);
		return this;
	}

	public UniqueConstraintImpl get() {
		return constraint;
	}
}
