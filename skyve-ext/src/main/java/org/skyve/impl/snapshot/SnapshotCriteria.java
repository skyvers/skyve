package org.skyve.impl.snapshot;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Represents a compound snapshot filter containing nested criteria and a logical operator.
 */
public class SnapshotCriteria extends SnapshotFilter {
	/**
	 * Map key used to persist the compound operator value.
	 */
	private static final String OPERATOR_PROPERTY_NAME = "operator";
	
	/**
	 * Map key used to persist nested filter entries.
	 */
	private static final String FILTERS_PROPERTY_NAME = "filters";
	
	/**
	 * Logical operator used to evaluate this criteria group.
	 */
	private CompoundFilterOperator operator = CompoundFilterOperator.and;
	
	/**
	 * Child filters contained by this compound criteria.
	 */
	private List<SnapshotFilter> filters = new ArrayList<>();

	/**
	 * Returns the logical operator used for this criteria group.
	 *
	 * @return The compound filter operator.
	 */
	public @Nullable CompoundFilterOperator getOperator() {
		return operator;
	}
	
	/**
	 * Sets the logical operator used for this criteria group.
	 *
	 * @param operator The compound filter operator.
	 */
	public void setOperator(@Nonnull CompoundFilterOperator operator) {
		this.operator = operator;
	}
	
	/**
	 * Returns the nested filters that make up this criteria group.
	 *
	 * @return The mutable list of child filters.
	 */
	public @Nonnull List<SnapshotFilter> getFilters() {
		return filters;
	}

	/**
	 * Converts this criteria group to its map representation.
	 *
	 * @return A map containing operator and nested filter values.
	 */
	@Override
	protected LinkedHashMap<String, Object> toMap() {
		LinkedHashMap<String, Object> result = new LinkedHashMap<>();
		
		result.put(OPERATOR_PROPERTY_NAME, operator);
		
		if (! filters.isEmpty()) {
			List<LinkedHashMap<String, Object>> filterValues = new ArrayList<>();
			for (SnapshotFilter filter : filters) {
				filterValues.add(filter.toMap());
			}
			result.put(FILTERS_PROPERTY_NAME, filterValues);
		}
		
		return result;
	}

	/**
	 * Populates this criteria from a map representation.
	 *
	 * @param map The source map containing criteria state.
	 */
	@Override
	protected void populate(Map<String, Object> map) {
		Object value = map.get(OPERATOR_PROPERTY_NAME);
		if (value != null) {
			operator = CompoundFilterOperator.valueOf(value.toString());
		}
		value = map.get(FILTERS_PROPERTY_NAME);
		if (value instanceof List<?> list) {
			for (Object element : list) {
				if (element instanceof Map<?, ?>) {
					@SuppressWarnings("unchecked")
					Map<String, Object> basis = (Map<String, Object>) element;
					filters.add(SnapshotFilter.fromMap(basis));
				}
			}
		}
	}
}
