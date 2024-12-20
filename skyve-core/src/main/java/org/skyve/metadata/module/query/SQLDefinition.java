package org.skyve.metadata.module.query;

import jakarta.annotation.Nonnull;

/**
 * 
 */
public interface SQLDefinition extends QueryDefinition {
	/**
	 * 
	 * @return
	 */
	@Nonnull String getQuery();
}
