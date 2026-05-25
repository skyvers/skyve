package org.skyve.metadata.view.model.list;

import java.util.List;

import org.skyve.domain.Bean;

/**
 * A single page of rows returned by {@link ListModel#fetch()}.
 *
 * <p>A {@code Page} carries the subset of rows for the requested start/end range
 * together with the total row count for the full result set (used for pagination
 * controls). An optional summary row holds aggregate values (count, sum, avg, etc.)
 * when an {@link org.skyve.persistence.DocumentQuery.AggregateFunction} is active.
 */
public final class Page {
	private long totalRows;
	private List<Bean> rows;
	private Bean summary;

	public long getTotalRows() {
		return totalRows;
	}
	public void setTotalRows(long totalRows) {
		this.totalRows = totalRows;
	}

	public List<Bean> getRows() {
		return rows;
	}
	public void setRows(List<Bean> rows) {
		this.rows = rows;
	}

	public Bean getSummary() {
		return summary;
	}
	public void setSummary(Bean summary) {
		this.summary = summary;
	}
}
