package org.skyve.impl.metadata.repository;

import org.skyve.metadata.MetaData;
import org.skyve.metadata.repository.ProvidedRepository;

public interface PersistentMetaData<T extends MetaData> extends MetaData {
	public T convert(String metaDataName, ProvidedRepository repository);
}
