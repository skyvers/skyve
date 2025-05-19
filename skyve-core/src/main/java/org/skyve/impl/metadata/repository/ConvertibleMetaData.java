package org.skyve.impl.metadata.repository;

import org.skyve.metadata.PersistentMetaData;

import jakarta.annotation.Nonnull;

public interface ConvertibleMetaData <T extends PersistentMetaData> extends PersistentMetaData {
	@Nonnull T convert(@Nonnull String metaDataName);
}
