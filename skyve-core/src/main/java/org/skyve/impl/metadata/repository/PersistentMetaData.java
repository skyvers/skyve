package org.skyve.impl.metadata.repository;

import org.skyve.metadata.MetaData;

public interface PersistentMetaData<T extends MetaData> extends MetaData {
	public T convert(String metaDataName, AbstractRepository repository);
}
