package org.skyve.impl.metadata.repository;

import javax.annotation.Nonnull;

import org.skyve.metadata.PersistentMetaData;
import org.skyve.metadata.repository.ProvidedRepository;

public interface ConvertableMetaData <T extends PersistentMetaData> extends PersistentMetaData {
	@Nonnull T convert(@Nonnull String metaDataName, @Nonnull ProvidedRepository repository);
}
