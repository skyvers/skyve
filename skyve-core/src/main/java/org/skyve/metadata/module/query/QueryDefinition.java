package org.skyve.metadata.module.query;

import org.skyve.metadata.NamedMetaData;
import org.skyve.metadata.module.Module;
import org.skyve.util.Util;

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
	
	public default String getLocalisedDescription() {
		return Util.i18n(getDescription());
	}
	
	/**
	 * 
	 * @return
	 */
	public String getDocumentation();
}
