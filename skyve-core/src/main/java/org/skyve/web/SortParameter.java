package org.skyve.web;

import org.skyve.metadata.SortDirection;

/**
 * Encapsulates a single sort criterion: the binding path to sort by and the direction.
 *
 * <p>Sort parameters are passed from the web tier to the persistence layer to order
 * list, query, and model results. Multiple sort parameters can be applied in sequence
 * to achieve multi-column sorting.
 *
 * @see org.skyve.metadata.SortDirection
 */
public interface SortParameter {
	/**
	 * Returns the binding expression identifying the attribute to sort by.
	 * Uses dot-notation for nested attributes (e.g. {@code "contact.name"}).
	 */
	public String getBy();

	/**
	 * Sets the binding expression identifying the attribute to sort by.
	 *
	 * @param by dot-separated binding path; must not be {@code null}
	 */
	public void setBy(String by);

	/**
	 * Returns the sort direction ({@code ascending} or {@code descending}).
	 */
	public SortDirection getDirection();

	/**
	 * Sets the sort direction.
	 *
	 * @param direction must not be {@code null}
	 */
	public void setDirection(SortDirection direction);
}
