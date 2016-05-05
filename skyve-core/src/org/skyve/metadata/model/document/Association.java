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
}
