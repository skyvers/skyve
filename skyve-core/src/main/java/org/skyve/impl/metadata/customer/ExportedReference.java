package org.skyve.impl.metadata.customer;

import java.io.Serializable;

import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.model.document.Reference.ReferenceType;
import org.skyve.util.Util;

import jakarta.annotation.Nullable;

/**
 * Records a cross-module referential dependency from one document to another.
 *
 * <p>An {@code ExportedReference} is created for every {@link org.skyve.metadata.model.document.Association}
 * or {@link org.skyve.metadata.model.document.Collection} defined in a module that
 * points at a document owned by a different module.  The repository uses these records
 * at delete time to check whether outstanding references would be violated before
 * allowing a bean to be removed.
 *
 * <p>Threading: not thread-safe.  Instances are populated once during metadata
 * loading and are read-only thereafter.
 *
 * @see CustomerImpl
 */
public class ExportedReference implements Serializable {
	private static final long serialVersionUID = 3672667027101941186L;

	private String moduleName;
	private String documentName;
	private Persistent persistent;
	private String documentAlias;
	private String referenceFieldName;
	private ReferenceType type;
	private boolean required;
	
	public @Nullable String getDocumentAlias() {
		return documentAlias;
	}
	
	public @Nullable String getLocalisedDocumentAlias() {
		return Util.i18n(documentAlias);
	}

	public void setDocumentAlias(@Nullable String documentAlias) {
		this.documentAlias = documentAlias;
	}

	public @Nullable String getReferenceFieldName() {
		return referenceFieldName;
	}

	public void setReferenceFieldName(@Nullable String referenceFieldName) {
		this.referenceFieldName = referenceFieldName;
	}

	public @Nullable Persistent getPersistent() {
		return persistent;
	}

	public void setPersistent(@Nullable Persistent persistent) {
		this.persistent = persistent;
	}

	public @Nullable String getDocumentName() {
		return documentName;
	}

	public void setDocumentName(@Nullable String documentName) {
		this.documentName = documentName;
	}

	public @Nullable String getModuleName() {
		return moduleName;
	}

	public void setModuleName(@Nullable String moduleName) {
		this.moduleName = moduleName;
	}

	public boolean isCollection() {
		return (type instanceof CollectionType);
	}

	public @Nullable ReferenceType getType() {
		return type;
	}

	public void setType(@Nullable ReferenceType type) {
		this.type = type;
	}

	public boolean isRequired() {
		return required;
	}

	public void setRequired(boolean required) {
		this.required = required;
	}
}
