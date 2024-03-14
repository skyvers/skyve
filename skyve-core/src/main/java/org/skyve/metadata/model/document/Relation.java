package org.skyve.metadata.model.document;

import org.skyve.metadata.model.Attribute;

public interface Relation extends Attribute {
	/**
	 * 
	 * @return
	 */
	public String getDocumentName();
	
	@Override
	default boolean isScalar() {
		return false;
	}
}
