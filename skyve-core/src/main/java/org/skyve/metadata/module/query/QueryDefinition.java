package org.skyve.metadata.module.query;

import org.skyve.metadata.NamedMetaData;
import org.skyve.metadata.SerializableMetaData;
import org.skyve.metadata.module.Module;
import org.skyve.util.Util;

/**
 * 
 */
public interface QueryDefinition extends NamedMetaData, SerializableMetaData {
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
	
	/**
	 * 0 means no timeout (ie defer to Skyve timeouts)
	 * @return	query timeout in seconds
	 */
	public int getTimeoutInSeconds();
}
