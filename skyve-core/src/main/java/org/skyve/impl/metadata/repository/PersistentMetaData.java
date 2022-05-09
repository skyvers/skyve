package org.skyve.impl.metadata.repository;

import javax.annotation.Nonnull;

import org.skyve.metadata.SerializableMetaData;
import org.skyve.metadata.repository.ProvidedRepository;

public interface PersistentMetaData<T extends SerializableMetaData> extends SerializableMetaData {
	public @Nonnull T convert(@Nonnull String metaDataName, @Nonnull ProvidedRepository repository);
}
