package org.skyve.metadata.user;

import org.skyve.metadata.NamedMetaData;
import org.skyve.metadata.module.Module;

/**
 * 
 */
public interface Role extends NamedMetaData {
	/**
	 * 
	 * @return
	 */
	public String getDescription();
	
	/**
	 * 
	 * @return
	 */
	public Module getOwningModule();
	
	/**
	 * 
	 * @return
	 */
	public String getDocumentation();
}
