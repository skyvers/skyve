package org.skyve.metadata.view.model.list;

import java.util.List;

import org.skyve.domain.Bean;

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
