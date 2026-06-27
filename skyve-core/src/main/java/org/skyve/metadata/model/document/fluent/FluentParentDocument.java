package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.repository.document.ParentDocument;

/**
 * Provides a fluent builder for FluentParentDocument metadata.
 */
public class FluentParentDocument {
	private ParentDocument parent = null;
	
	/**
	 * Creates a fluent builder instance.
	 */
	public FluentParentDocument() {
		parent = new ParentDocument();
	}

	/**
	 * Creates a fluent builder instance.
	 */
	public FluentParentDocument(ParentDocument parent) {
		this.parent = parent;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public FluentParentDocument from(@SuppressWarnings("hiding") ParentDocument parent) {
		parentDocumentName(parent.getParentDocumentName());
		databaseIndex(! Boolean.FALSE.equals(parent.getDatabaseIndex()));
		return this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentParentDocument parentDocumentName(String name) {
		parent.setParentDocumentName(name);
		return this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentParentDocument databaseIndex(boolean index) {
		parent.setDatabaseIndex(index ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}
	/**
	 * Returns the mutable metadata instance represented by this builder.
	 */
	public ParentDocument get() {
		return parent;
	}
}
