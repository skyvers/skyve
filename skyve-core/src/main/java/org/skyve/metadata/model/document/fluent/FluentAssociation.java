package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.AssociationImpl;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Association.AssociationType;

public class FluentAssociation extends FluentReference<FluentAssociation> {
	private AssociationImpl association = new AssociationImpl();
	
	public FluentAssociation() {
		// nothing to see
	}

	public FluentAssociation(Association association) {
		super(association);
		required(association.isRequired());
		type(association.getType());
		embeddedColumnsPrefix(association.getEmbeddedColumnsPrefix());
		databaseIndex(! Boolean.FALSE.equals(association.getDatabaseIndex()));
	}
	
	public FluentAssociation required(boolean required) {
		association.setRequired(required);
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
