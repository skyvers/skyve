package org.skyve.impl.metadata.repository;

import org.skyve.metadata.SerializableMetaData;
import org.skyve.metadata.repository.ProvidedRepository;

public interface PersistentMetaData<T extends SerializableMetaData> extends SerializableMetaData {
	public T convert(String metaDataName, ProvidedRepository repository);
}
