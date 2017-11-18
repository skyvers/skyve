package org.skyve.metadata.module;

import org.skyve.metadata.NamedMetaData;

/**
 * 
 */
public interface JobMetaData extends NamedMetaData {
	/**
	 * 
	 * @return
	 */
	public String getDisplayName();

	/**
	 * 
	 * @return
	 */
	public String getClassName();
	
	/**
	 * 
	 * @return
	 */
	public String getDescription();
	
	/**
	 * Derived property
	 * @return
	 */
	public String getOwningModuleName();
}
