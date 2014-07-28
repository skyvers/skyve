package org.skyve.metadata;

/**
 * 
 */
public interface NamedMetaData extends MetaData {
	/**
	 * The programmatic name given to this piece of metadata - field name, converter name etc.
	 * 
	 * @return The converter name.
	 */
	public String getName();
}
