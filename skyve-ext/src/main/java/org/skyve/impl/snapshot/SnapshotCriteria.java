package org.skyve.impl.snapshot;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

public class SnapshotCriteria extends SnapshotFilter {
	private static final String OPERATOR_PROPERTY_NAME = "operator";
	private static final String FILTERS_PROPERTY_NAME = "filters";
	
	private CompoundFilterOperator operator = CompoundFilterOperator.and;
	private List<SnapshotFilter> filters = new ArrayList<>();

	public @Nullable CompoundFilterOperator getOperator() {
		return operator;
	}
	public void setOperator(@Nonnull CompoundFilterOperator operator) {
		this.operator = operator;
	}
	public @Nonnull List<SnapshotFilter> getFilters() {
		return filters;
	}

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
