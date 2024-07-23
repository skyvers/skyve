package org.skyve.impl.metadata.repository;

import org.skyve.metadata.PersistentMetaData;
import org.skyve.metadata.repository.ProvidedRepository;

import jakarta.annotation.Nonnull;

public interface ConvertableMetaData <T extends PersistentMetaData> extends PersistentMetaData {
	@Nonnull T convert(@Nonnull String metaDataName, @Nonnull ProvidedRepository repository);
}
