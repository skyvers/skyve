package org.skyve.impl.snapshot;

import java.util.LinkedHashMap;
import java.util.Map;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Represents a single field-level snapshot filter condition.
 */
public class SnapshotCriterion extends SnapshotFilter {
	/**
	 * Map key used to persist the filtered column name.
	 */
	static final String COLUMN_PROPERTY_NAME = "column";
	
	/**
	 * Map key used to persist the filter operator.
	 */
	private static final String OPERATOR_PROPERTY_NAME = "operator";
	
	/**
	 * Map key used to persist single-value criterion data.
	 */
	private static final String VALUE_PROPERTY_NAME = "value";
	
	/**
	 * Map key used to persist range start values.
	 */
	private static final String START_PROPERTY_NAME = "start";
	
	/**
	 * Map key used to persist range end values.
	 */
	private static final String END_PROPERTY_NAME = "end";
	
	/**
	 * Name of the column being filtered.
	 */
	private String column;
	
	/**
	 * Operator applied to the column value.
	 */
	private SmartClientFilterOperator operator;
	
	/**
	 * Single comparison value for non-range operators.
	 */
	private Object value;
	
	/**
	 * Inclusive start value for range-style operators.
	 */
	private Object start;
	
	/**
	 * Inclusive end value for range-style operators.
	 */
	private Object end;

	/**
	 * Returns the filtered column name.
	 *
	 * @return The criterion column name.
	 */
	public @Nullable String getColumn() {
		return column;
	}
	
	/**
	 * Sets the filtered column name.
	 *
	 * @param column The criterion column name.
	 */
	public void setColumn(@Nonnull String column) {
		this.column = column;
	}
	
	/**
	 * Returns the operator used by this criterion.
	 *
	 * @return The smart-client filter operator.
	 */
	public @Nullable SmartClientFilterOperator getOperator() {
		return operator;
	}
	
	/**
	 * Sets the operator used by this criterion.
	 *
	 * @param operator The smart-client filter operator.
	 */
	public void setOperator(@Nonnull SmartClientFilterOperator operator) {
		this.operator = operator;
	}
	
	/**
	 * Returns the criterion's single comparison value.
	 *
	 * @return The comparison value.
	 */
	public @Nullable Object getValue() {
		return value;
	}
	
	/**
	 * Sets the criterion's single comparison value.
	 *
	 * @param value The comparison value.
	 */
	public void setValue(@Nullable Object value) {
		this.value = value;
	}
	
	/**
	 * Returns the range start value for this criterion.
	 *
	 * @return The range start value.
	 */
	public @Nullable Object getStart() {
		return start;
	}
	
	/**
	 * Sets the range start value for this criterion.
	 *
	 * @param start The range start value.
	 */
	public void setStart(@Nullable Object start) {
		this.start = start;
	}
	
	/**
	 * Returns the range end value for this criterion.
	 *
	 * @return The range end value.
	 */
	public @Nullable Object getEnd() {
		return end;
	}
	
	/**
	 * Sets the range end value for this criterion.
	 *
	 * @param end The range end value.
	 */
	public void setEnd(@Nullable Object end) {
		this.end = end;
	}
	
	/**
	 * Converts this criterion to a map representation.
	 *
	 * @return A map containing criterion field values.
	 */
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

	/**
	 * Populates this criterion from a map representation.
	 *
	 * @param map The source map containing criterion field values.
	 */
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
