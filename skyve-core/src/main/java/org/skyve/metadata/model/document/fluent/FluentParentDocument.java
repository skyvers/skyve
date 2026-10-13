package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.repository.document.ParentDocument;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

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
	public FluentParentDocument(@Nonnull ParentDocument parent) {
		this.parent = parent;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public @Nonnull FluentParentDocument from(@SuppressWarnings("hiding") @Nonnull ParentDocument parent) {
		parentDocumentName(parent.getParentDocumentName());
		databaseIndex(! Boolean.FALSE.equals(parent.getDatabaseIndex()));
		return this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	public @Nonnull FluentParentDocument parentDocumentName(@Nullable String name) {
		parent.setParentDocumentName(name);
		return this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	public @Nonnull FluentParentDocument databaseIndex(boolean index) {
		parent.setDatabaseIndex(index ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}
	/**
	 * Returns the mutable metadata instance represented by this builder.
	 */
	public @Nonnull ParentDocument get() {
		return parent;
	}
}
