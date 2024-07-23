package org.skyve.metadata.model.document;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlType;

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
		 * The associated bean is persisted to another table but is cascade deleted when the owning bean is deleted.
		 */
		composition, 
		
		/**
		 * The associated bean is persisted to another table but is orphaned when the referencing bean is deleted.
		 */
		aggregation,
		
		/**
		 * The associated bean is persisted in the owning document's table and does not exist when the owning bean is deleted.
		 */
		embedded;
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
	 * Used only for embedded associations to introduce a column prefix for all embedded document attributes
	 * that are inlined in the owning document's database table.
	 * This allows columns that would otherwise have the same name to be created with unique names.
	 * @return	The prefix (with no underscores).
	 */
	public String getEmbeddedColumnsPrefix();

	/**
	 * This determines whether to create an index on the association foreign key column.
	 * Set to true if the database does not implement foreign keys with indexes - ie SQLServer.
	 */
	public Boolean getDatabaseIndex();
}
