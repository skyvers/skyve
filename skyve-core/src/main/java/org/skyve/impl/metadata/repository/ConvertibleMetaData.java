package org.skyve.impl.metadata.repository;

import org.skyve.metadata.PersistentMetaData;
import org.skyve.metadata.repository.ProvidedRepository;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

public interface ConvertibleMetaData <T extends PersistentMetaData> extends PersistentMetaData {
	@Nonnull T convert(@Nonnull String metaDataName,
						@Nullable String owningMetaDataName,
						@Nonnull ProvidedRepository repository);
}
