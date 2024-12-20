package org.skyve.metadata.module.query;

import jakarta.annotation.Nonnull;

/**
 * 
 */
public interface BizQLDefinition extends QueryDefinition {
	/**
	 * 
	 * @return
	 */
	@Nonnull String getQuery();
}
