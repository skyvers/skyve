package org.skyve.impl.metadata.repository;

import org.skyve.metadata.PersistentMetaData;

import jakarta.annotation.Nonnull;

/**
 * Allows conversion of some persistent meta data to its "runtime" equivalent.
 * @param <T>	The meta data type to convert to.
 */
public interface ConvertibleMetaData <T extends PersistentMetaData> extends PersistentMetaData {
	/**
	 * Convert the persistent meta data.
	 * @param metaDataName	The descriptive name of the metadata to use in validation messages
	 * @return	The converted meta data
	 */
	@Nonnull T convert(@Nonnull String metaDataName);
}
