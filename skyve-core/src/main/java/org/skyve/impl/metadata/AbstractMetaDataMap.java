package org.skyve.impl.metadata;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.metadata.SerializableMetaData;

/**
 * Stores named metadata entries in deterministic name order.
 *
 * <p>This base type backs implementation objects that aggregate nested metadata elements
 * by identifier while preserving stable iteration order.
 */
public abstract class AbstractMetaDataMap implements SerializableMetaData {
	private static final long serialVersionUID = -6544574263967307394L;

	private Map<String, SerializableMetaData> metaDataMap = new TreeMap<>();

	/**
	 * Returns the metadata entry stored under the supplied identifier.
	 *
	 * @param id the metadata identifier
	 * @return the stored metadata entry, or {@code null} when no entry exists
	 */
	protected SerializableMetaData getMetaData(String id) {
		return metaDataMap.get(id);
	}

	/**
	 * Stores a metadata entry under the supplied identifier.
	 *
	 * <p>Side effects: mutates the internal metadata map and rejects duplicate keys.
	 *
	 * @param id the metadata identifier
	 * @param metaData the metadata entry to store; must implement {@link SerializableMetaData}
	 * @throws IllegalStateException if an entry with the same identifier already exists
	 */
	protected void putMetaData(String id, SerializableMetaData metaData) {
		SerializableMetaData oldMetaData = metaDataMap.put(id, metaData);
		if (oldMetaData != null) {
			throw new IllegalStateException("MetaData " + id + " with class " + metaData.getClass() +
												" redefines the name used for " + oldMetaData.getClass());
		}
	}

	/**
	 * Returns all stored metadata entries assignable to the requested type.
	 *
	 * <p>Complexity: O(n) time where n is the number of stored metadata entries.
	 *
	 * @param type the metadata subtype to filter by
	 * @param <T> the requested metadata subtype
	 * @return a new list containing the matching entries in key order; never {@code null}
	 */
	@SuppressWarnings("unchecked")
	protected <T extends SerializableMetaData> List<T> getMetaDataOfType(Class<T> type) {
		List<T> result = new ArrayList<>();

		for (SerializableMetaData metaData : metaDataMap.values()) {
			if (type.isAssignableFrom(metaData.getClass())) {
				result.add((T) metaData);
			}
		}

		return result;
	}
}
