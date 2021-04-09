package org.skyve.impl.metadata.customer;

import java.io.Serializable;

import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.model.document.Reference.ReferenceType;
import org.skyve.util.Util;

public class ExportedReference implements Serializable {
	private static final long serialVersionUID = 3672667027101941186L;

	private String moduleName;
	private String documentName;
	private Persistent persistent;
	private String documentAlias;
	private String referenceFieldName;
	private ReferenceType type;
	private boolean required;
	
	public String getDocumentAlias() {
		return documentAlias;
	}
	
	public String getLocalisedDocumentAlias() {
		return Util.i18n(documentAlias);
	}

	public void setDocumentAlias(String documentAlias) {
		this.documentAlias = documentAlias;
	}

	public String getReferenceFieldName() {
		return referenceFieldName;
	}

	public void setReferenceFieldName(String referenceFieldName) {
		this.referenceFieldName = referenceFieldName;
	}

	public Persistent getPersistent() {
		return persistent;
	}

	public void setPersistent(Persistent persistent) {
		this.persistent = persistent;
	}

	public String getDocumentName() {
		return documentName;
	}

	public void setDocumentName(String documentName) {
		this.documentName = documentName;
	}

	public String getModuleName() {
		return moduleName;
	}

	public void setModuleName(String moduleName) {
		this.moduleName = moduleName;
	}

	public boolean isCollection() {
		return (type instanceof CollectionType);
	}

	public ReferenceType getType() {
		return type;
	}

	public void setType(ReferenceType type) {
		this.type = type;
	}

	public boolean isRequired() {
		return required;
	}

	public void setRequired(boolean required) {
		this.required = required;
	}
}
