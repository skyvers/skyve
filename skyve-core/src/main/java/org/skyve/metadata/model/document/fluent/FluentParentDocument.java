package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.repository.document.ParentDocument;

public class FluentParentDocument {
	private ParentDocument parent = null;
	
	public FluentParentDocument() {
		parent = new ParentDocument();
	}

	public FluentParentDocument(ParentDocument parent) {
		this.parent = parent;
	}

	public FluentParentDocument from(@SuppressWarnings("hiding") ParentDocument parent) {
		parentDocumentName(parent.getParentDocumentName());
		databaseIndex(! Boolean.FALSE.equals(parent.getDatabaseIndex()));
		return this;
	}
	
	public FluentParentDocument parentDocumentName(String name) {
		parent.setParentDocumentName(name);
		return this;
	}
	
	public FluentParentDocument databaseIndex(boolean index) {
		parent.setDatabaseIndex(index ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}
	
	public ParentDocument get() {
		return parent;
	}
}
