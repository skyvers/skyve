package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.AssociationImpl;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Association.AssociationType;

/**
 * Provides a fluent builder for FluentAssociation metadata.
 */
public class FluentAssociation extends FluentReference<FluentAssociation> {
	private AssociationImpl association = null;
	
	/**
	 * Creates a fluent builder instance.
	 */
	public FluentAssociation() {
		association = new AssociationImpl();
	}
	
	/**
	 * Creates a fluent builder instance.
	 */
	public FluentAssociation(AssociationImpl association) {
		this.association = association;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public FluentAssociation from(@SuppressWarnings("hiding") Association association) {
		super.from(association);
		required(association.isRequired());
		requiredMessage(association.getRequiredMessage());
		type(association.getType());
		embeddedColumnsPrefix(association.getEmbeddedColumnsPrefix());
		Boolean index = association.getDatabaseIndex();
		if (index != null) {
			databaseIndex(index.booleanValue());
		}
		return this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentAssociation required(boolean required) {
		association.setRequired(required);
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentAssociation requiredMessage(String requiredMessage) {
		association.setRequiredMessage(requiredMessage);
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentAssociation type(AssociationType type) {
		association.setType(type);
		return this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentAssociation embeddedColumnsPrefix(String embeddedColumnsPrefix) {
		association.setEmbeddedColumnsPrefix(embeddedColumnsPrefix);
		return this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentAssociation databaseIndex(boolean databaseIndex) {
		association.setDatabaseIndex(databaseIndex ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Returns the mutable metadata instance represented by this builder.
	 */
	@Override
	public AssociationImpl get() {
		return association;
	}
}
