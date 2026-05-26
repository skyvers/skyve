package org.skyve.impl.snapshot;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.SortDirection;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.util.JSON;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Captures list-grid snapshot state including sort, group, column and filter configuration.
 */
public class Snapshot {
	private static final String ADVANCED_PROPERTY_NAME = "advanced";
	private static final String SUMMARY_PROPERTY_NAME = "summary";
	private static final String SORTS_PROPERTY_NAME = "sorts";
	private static final String GROUP_PROPERTY_NAME = "group";
	private static final String COLUMNS_PROPERTY_NAME = "columns";
	private static final String FILTER_PROPERTY_NAME = "filter";
	
	public static enum AdvancedSearchType {
		radio, bracket, inline;
	}
	
	private AdvancedSearchType advanced;
	private AggregateFunction summary;
	private Map<String, SortDirection> sorts = new LinkedHashMap<>();
	private String group;
	private Map<String, Integer> columns = new LinkedHashMap<>();
	private SnapshotFilter filter;
	private Map<String, Object> sourceSmartClientCriteria;

	public @Nullable AdvancedSearchType getAdvanced() {
		return advanced;
	}
	/**
	 * Sets the advanced.
	 */
	public void setAdvanced(@Nullable AdvancedSearchType advanced) {
		this.advanced = advanced;
	}
	
	public @Nullable AggregateFunction getSummary() {
		return summary;
	}
	/**
	 * Sets the summary.
	 */
	public void setSummary(@Nullable AggregateFunction summary) {
		this.summary = summary;
	}
	
	public @Nonnull Map<String, SortDirection> getSorts() {
		return sorts;
	}
	/**
	 * Adds or replaces the sort value.
	 */
	public void putSort(@Nonnull String column) {
		putSort(column, SortDirection.ascending);
	}
	/**
	 * Adds or replaces the sort value.
	 */
	public void putSort(@Nonnull String column, @Nonnull SortDirection direction) {
		sorts.put(column, direction);
	}
	
	public @Nullable String getGroup() {
		return group;
	}
	/**
	 * Sets the group.
	 */
	public void setGroup(@Nullable String group) {
		this.group = group;
	}
	
	public @Nonnull Map<String, Integer> getColumns() {
		return columns;
	}
	/**
	 * Adds or replaces the column value.
	 */
	public void putColumn(@Nonnull String column) {
		columns.put(column, null);
	}
	/**
	 * Adds or replaces the column value.
	 */
	public void putColumn(@Nonnull String column, int width) {
		columns.put(column, Integer.valueOf(width));
	}
	/**
	 * Adds or replaces the column value.
	 */
	public void putColumn(@Nonnull String column, @Nullable Integer width) {
		columns.put(column, width);
	}
	
	public @Nullable SnapshotFilter getFilter() {
		return filter;
	}
	/**
	 * Sets the filter.
	 */
	public void setFilter(@Nullable SnapshotFilter filter) {
		this.filter = filter;
	}

	public @Nullable Map<String, Object> getSourceSmartClientCriteria() {
		return sourceSmartClientCriteria;
	}
	/**
	 * Sets the sourceSmartClientCriteria.
	 */
	public void setSourceSmartClientCriteria(@Nullable Map<String, Object> sourceSmartClientCriteria) {
		this.sourceSmartClientCriteria = sourceSmartClientCriteria;
	}
	
	/**
	 * Serializes this snapshot into JSON suitable for persistence and round-tripping.
	 *
	 * @return JSON containing non-null snapshot state.
	 */
	public String toJSON() {
		Map<String, Object> result = new LinkedHashMap<>(); // keep insertion order
		
		put(result, ADVANCED_PROPERTY_NAME, advanced);
		put(result, SUMMARY_PROPERTY_NAME, summary);
		List<String> list = new ArrayList<>(sorts.size());
		sorts.forEach((c, d) -> {
			if (SortDirection.descending.equals(d)) {
				list.add(new StringBuilder(c.length() + 1).append('-').append(c).toString());
			}
			else {
				list.add(c);
			}
		});
		put(result, SORTS_PROPERTY_NAME, list);
		put(result, GROUP_PROPERTY_NAME, group);
		put(result, COLUMNS_PROPERTY_NAME, columns);
		if (filter != null) {
			put(result, FILTER_PROPERTY_NAME, filter.toMap());
		}
		
		return JSON.marshall(result);
	}
	
	/**
	 * Rebuilds a snapshot instance from previously marshalled JSON.
	 *
	 * @param json Snapshot JSON text.
	 * @return A populated snapshot, or {@code null} when the input contains no values.
	 * @throws Exception If the JSON payload cannot be parsed into a valid snapshot structure.
	 */
	public static Snapshot fromJSON(String json) throws Exception {
		@SuppressWarnings("unchecked")
		Map<String, Object> values = (Map<String, Object>) JSON.unmarshall(json);
		if (values == null) {
			return null;
		}

		Snapshot result = new Snapshot();
		Object value = values.get(ADVANCED_PROPERTY_NAME);
		if (value != null) {
			result.setAdvanced(AdvancedSearchType.valueOf(value.toString()));
		}
		value = values.get(SUMMARY_PROPERTY_NAME);
		if (value != null) {
			result.setSummary(AggregateFunction.valueOf(value.toString()));
		}
		value = values.get(SORTS_PROPERTY_NAME);
		if (value instanceof List<?> list) {
			for (Object element : (list)) {
				String column = UtilImpl.processStringValue(element.toString());
				if (column != null) {
					if (column.charAt(0) == '-') {
						result.putSort(column.substring(1), SortDirection.descending);
					}
					else {
						result.putSort(column);
					}
				}
			}
		}
		value = values.get(GROUP_PROPERTY_NAME);
		if (value != null) {
			result.setGroup(value.toString());
		}
		value = values.get(COLUMNS_PROPERTY_NAME);
		if (value instanceof Map<?, ?> map) {
			for (Entry<?, ?> entry : map.entrySet()) {
				Object entryKey = entry.getKey();
				Object entryValue = entry.getValue();
				if (entryKey instanceof String name) {
					if (entryValue instanceof Number width) {
						result.putColumn(name, width.intValue());
					}
					else {
						result.putColumn(name);
					}
				}
			}
		}
		value = values.get(FILTER_PROPERTY_NAME);
		if (value instanceof Map map) {
			@SuppressWarnings("unchecked")
			SnapshotFilter filter = SnapshotFilter.fromMap(map);
			result.setFilter(filter);
		}

		return result;
	}

	private static void put(Map<String, Object> values, String key, Object value) {
		if (value == null) {
			return;
		}
		if ((value instanceof Collection collection) && collection.isEmpty()) {
			return;
		}
		if ((value instanceof Map map) && map.isEmpty()) {
			return;
		}

		values.put(key,  value);
	}
}
