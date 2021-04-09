package org.skyve.metadata.module;

import org.skyve.metadata.NamedMetaData;
import org.skyve.util.Util;

/**
 * 
 */
public interface JobMetaData extends NamedMetaData {
	/**
	 * 
	 * @return
	 */
	public String getDisplayName();

	public default String getLocalisedDisplayName() {
		return Util.i18n(getDisplayName());
	}
	
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
	
	public default String getLocalisedDescription() {
		return Util.i18n(getDescription());
	}
	
	/**
	 * Derived property
	 * @return
	 */
	public String getOwningModuleName();
}
