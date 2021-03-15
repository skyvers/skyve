package org.skyve.metadata.user;

import org.skyve.metadata.NamedMetaData;
import org.skyve.metadata.module.Module;
import org.skyve.util.Util;

/**
 * 
 */
public interface Role extends NamedMetaData {
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
	public Module getOwningModule();
	
	/**
	 * 
	 * @return
	 */
	public String getDocumentation();
}
