package org.skyve.metadata.view.widget;

import org.skyve.metadata.FilterOperator;
import org.skyve.metadata.SerializableMetaData;

/**
 * A declarative filter constraint applied to a query-backed widget before it renders.
 *
 * <p>A {@code FilterParameter} maps a filter column ({@link #getFilterBinding()}) to a
 * value that is either a literal string ({@link #getValue()}) or derived from a binding
 * on the current bean ({@link #getValueBinding()}). The comparison is performed using
 * the specified {@link #getOperator()}.
 *
 * <p>Filter parameters are evaluated at render time to restrict the result set before
 * the user applies any interactive filtering.
 *
 * @see org.skyve.metadata.view.Filterable
 */
public interface FilterParameter extends SerializableMetaData {
	/**
	 * Returns the binding path of the query column to filter on.
	 *
	 * @return the filter column binding; never {@code null}
	 */
	public String getFilterBinding();

	/**
	 * Returns the comparison operator applied between the column value and the filter value.
	 *
	 * @return the filter operator; never {@code null}
	 */
	public FilterOperator getOperator();

	/**
	 * Returns the literal string value to compare against, or {@code null} if a value
	 * binding is used instead.
	 *
	 * @return the literal filter value, or {@code null}
	 */
	public String getValue();

	/**
	 * Returns the binding path on the current bean whose value is used as the filter
	 * comparand, or {@code null} if a literal value is used instead.
	 *
	 * @return the value binding path, or {@code null}
	 */
	public String getValueBinding();
}
