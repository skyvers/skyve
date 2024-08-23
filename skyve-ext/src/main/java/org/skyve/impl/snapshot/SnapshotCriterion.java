package org.skyve.impl.snapshot;

import java.util.LinkedHashMap;
import java.util.Map;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

public class SnapshotCriterion extends SnapshotFilter {
	static final String COLUMN_PROPERTY_NAME = "column";
	private static final String OPERATOR_PROPERTY_NAME = "operator";
	private static final String VALUE_PROPERTY_NAME = "value";
	private static final String START_PROPERTY_NAME = "start";
	private static final String END_PROPERTY_NAME = "end";
	
	private String column;
	private SmartClientFilterOperator operator;
	private Object value;
	private Object start;
	private Object end;

	public @Nullable String getColumn() {
		return column;
	}
	public void setColumn(@Nonnull String column) {
		this.column = column;
	}
	
	public @Nullable SmartClientFilterOperator getOperator() {
		return operator;
	}
	public void setOperator(@Nonnull SmartClientFilterOperator operator) {
		this.operator = operator;
	}
	
	public @Nullable Object getValue() {
		return value;
	}
	public void setValue(@Nullable Object value) {
		this.value = value;
	}
	
	public @Nullable Object getStart() {
		return start;
	}
	public void setStart(@Nullable Object start) {
		this.start = start;
	}
	
	public @Nullable Object getEnd() {
		return end;
	}
	public void setEnd(@Nullable Object end) {
		this.end = end;
	}
	
	@Override
	protected LinkedHashMap<String, Object> toMap() {
		LinkedHashMap<String, Object> result = new LinkedHashMap<>();

		result.put(COLUMN_PROPERTY_NAME, column);
		result.put(OPERATOR_PROPERTY_NAME, operator);
		if (value != null) {
			result.put(VALUE_PROPERTY_NAME, value);
		}
		if ((start != null) && (end != null)) {
			result.put(START_PROPERTY_NAME,  start);
			result.put(END_PROPERTY_NAME,  end);
		}

		return result;
	}

	@Override
	protected void populate(Map<String, Object> map) {
		Object mapValue = map.get(COLUMN_PROPERTY_NAME);
		if (mapValue != null) {
			column = mapValue.toString();
		}
		mapValue = map.get(OPERATOR_PROPERTY_NAME);
		if (mapValue != null) {
			operator = SmartClientFilterOperator.valueOf(mapValue.toString());
		}
		mapValue = map.get(VALUE_PROPERTY_NAME);
		if (mapValue != null) {
			value = mapValue;
		}
		mapValue = map.get(START_PROPERTY_NAME);
		if (mapValue != null) {
			start = mapValue;
		}
		mapValue = map.get(END_PROPERTY_NAME);
		if (mapValue != null) {
			end = mapValue;
		}
	}
}
