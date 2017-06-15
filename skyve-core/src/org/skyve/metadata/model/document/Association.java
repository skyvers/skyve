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

	/**
	 * Cascade type 'merge' makes many-many relationships within the association
	 * target object update (without the collection being dirty)
	 * and thus causes optimistic lock exceptions when the bizLock 
	 * is up-revved from the update statement.
	 * Case in point is Staff --many-to-one--> User --many-to-many--> Groups,
	 * all groups are up-revved, even though the collection is not dirty,
	 * causing optimistic lock when Staff are saved.
	 * So if lots of Staff use the same user, we're screwed.
	 */
	public Boolean getAllowCascadeMerge();
}
