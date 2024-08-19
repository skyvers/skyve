package org.skyve.impl.snapshot;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Represents a simple filter criterion or a compound recursive filter criteria.
 */
abstract class SnapshotFilter {
	/**
	 * Convert the filter state to a map of maps that can be marshalled to JSON.
	 * @return	A Map of Maps and Lists - LinkedHashMap is used to preserve insertion order in the JSON.
	 */
	protected abstract LinkedHashMap<String, Object> toMap();

	/**
	 * Called by the static fromMap(), this method populates the newly constructed filter with its state from the map.
	 * @param map	A Map of Maps and Lists.
	 */
	protected abstract void populate(Map<String, Object> map);
	
	/**
	 * Create a Filter from the given map {calls populate()}.
	 * @param map	A Map of Maps and Lists.
	 * @return	The filter.
	 */
	static SnapshotFilter fromMap(Map<String, Object> map) {
		if (map.get(SnapshotCriterion.COLUMN_PROPERTY_NAME) != null) {
			SnapshotCriterion result = new SnapshotCriterion();
			result.populate(map);
			return result;
		}

		SnapshotCriteria result = new SnapshotCriteria();
		result.populate(map);
		return result;
	}
}
