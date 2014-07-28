package org.skyve.metadata.model.document;

/**
 * 
 */
public interface Reference extends Relation {
	/**
	 * 
	 */
	// For Reference specialisations
	public static interface ReferenceType {
		// tagging interface
	}

	/**
	 * 
	 * @return
	 */
	public String getQueryName();
	
	/**
	 * 
	 * @return
	 */
	public ReferenceType getType();
}
