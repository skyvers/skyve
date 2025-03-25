package org.skyve.metadata.module.query;

import org.skyve.metadata.NamedMetaData;
import org.skyve.metadata.module.Module;
import org.skyve.util.Util;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * 
 */
public interface QueryDefinition extends NamedMetaData {
	/**
	 * 
	 * @return
	 */
	@Nonnull Module getOwningModule();

	/**
	 * 
	 * @return
	 */
	@Nonnull String getDescription();
	
	default @Nonnull String getLocalisedDescription() {
		return Util.nullSafeI18n(getDescription());
	}
	
	/**
	 * 
	 * @return
	 */
	@Nullable String getDocumentation();
	
	/**
	 * 0 means no timeout (ie defer to Skyve timeouts)
	 * @return	query timeout in seconds
	 */
	int getTimeoutInSeconds();
}
