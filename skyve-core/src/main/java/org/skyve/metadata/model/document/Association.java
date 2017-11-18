package org.skyve.metadata.model.document;

import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;

/**
 * 
 */
public interface Association extends Reference {
	/**
	 * 
	 */
	@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public enum AssociationType implements ReferenceType {
		/**
		 * 
		 */
		composition, 
		
		/**
		 * 
		 */
		aggregation;
	}

	/**
	 * 
	 */
	@Override
	public AssociationType getType();
	
	/**
	 * 
	 */
	@Override
	public boolean isRequired();
	
	/**
	 * This determines whether to create an index on the association foreign key column.
	 * Set to true if the database does not implement foreign keys with indexes - ie SQLServer.
	 */
	public Boolean getDatabaseIndex();
}
