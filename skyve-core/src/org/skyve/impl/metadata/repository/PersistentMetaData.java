package org.skyve.impl.metadata.repository;

import org.skyve.metadata.MetaData;
import org.skyve.metadata.MetaDataException;

public interface PersistentMetaData<T extends MetaData> {
	public T convert(String metaDataName) throws MetaDataException;
}
