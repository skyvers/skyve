package org.skyve.wildcat.metadata;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.metadata.MetaData;

public abstract class AbstractMetaDataMap implements MetaData {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -6544574263967307394L;

	private Map<String, MetaData> metaDataMap = new TreeMap<>();

	protected MetaData getMetaData(String id) {
		return metaDataMap.get(id);
	}

	protected void putMetaData(String id, MetaData metaData) {
		MetaData oldMetaData = metaDataMap.put(id, metaData);
		if (oldMetaData != null) {
			throw new IllegalStateException("MetaData " + id + " with class " + metaData.getClass() +
												" redefines the name used for " + oldMetaData.getClass());
		}
	}

	@SuppressWarnings("unchecked")
	protected <T extends MetaData> List<T> getMetaDataOfType(Class<T> type) {
		List<T> result = new ArrayList<>();

		for (MetaData metaData : metaDataMap.values()) {
			if (type.isAssignableFrom(metaData.getClass())) {
				result.add((T) metaData);
			}
		}

		return result;
	}
}
