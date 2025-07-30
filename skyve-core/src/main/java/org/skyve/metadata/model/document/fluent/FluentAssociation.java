package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.AssociationImpl;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Association.AssociationType;

public class FluentAssociation extends FluentReference<FluentAssociation> {
	private AssociationImpl association = null;
	
	public FluentAssociation() {
		association = new AssociationImpl();
	}
	
	public FluentAssociation(AssociationImpl association) {
		this.association = association;
	}

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
	
	public FluentAssociation required(boolean required) {
		association.setRequired(required);
		return this;
	}

	public FluentAssociation requiredMessage(String requiredMessage) {
		association.setRequiredMessage(requiredMessage);
		return this;
	}

	public FluentAssociation type(AssociationType type) {
		association.setType(type);
		return this;
	}
	
	public FluentAssociation embeddedColumnsPrefix(String embeddedColumnsPrefix) {
		association.setEmbeddedColumnsPrefix(embeddedColumnsPrefix);
		return this;
	}
	
	public FluentAssociation databaseIndex(boolean databaseIndex) {
		association.setDatabaseIndex(databaseIndex ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	@Override
	public AssociationImpl get() {
		return association;
	}
}
