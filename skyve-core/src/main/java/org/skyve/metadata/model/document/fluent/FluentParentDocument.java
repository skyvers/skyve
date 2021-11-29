package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.repository.document.ParentDocument;

public class FluentParentDocument {
	private ParentDocument parent = new ParentDocument();
	
	public FluentParentDocument() {
		// nothing to see
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
