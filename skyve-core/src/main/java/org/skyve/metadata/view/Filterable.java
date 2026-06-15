package org.skyve.metadata.view;

import java.util.List;

import org.skyve.metadata.view.widget.FilterParameter;

/**
 * Mixin interface for view elements that support declarative pre-filter parameters.
 *
 * <p>Filter parameters pre-populate query filter criteria before a list or lookup
 * widget renders, typically restricting the result set based on the current context
 * (e.g. the parent bean's identifier).
 *
 * @see Parameterizable
 */
public interface Filterable extends Parameterizable {
	/**
	 * Returns the list of static filter parameters applied to this element's query.
	 *
	 * <p>Each {@link FilterParameter} maps a filter column to a value derived from
	 * the current bean or a literal. These are applied before any interactive
	 * user-driven filtering.
	 *
	 * @return a non-{@code null} list of filter parameters; may be empty
	 */
	public List<FilterParameter> getFilterParameters();
}
