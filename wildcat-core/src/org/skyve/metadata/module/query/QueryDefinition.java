package org.skyve.metadata.module.query;

import org.skyve.metadata.NamedMetaData;
import org.skyve.metadata.module.Module;

/**
 * 
 */
public interface QueryDefinition extends NamedMetaData {
	/**
	 * 
	 * @return
	 */
	public Module getOwningModule();

	/**
	 * 
	 * @return
	 */
	public String getDescription();
	
	/**
	 * 
	 * @return
	 */
	public String getDocumentation();
}
