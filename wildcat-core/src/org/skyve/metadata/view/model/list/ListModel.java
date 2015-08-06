package org.skyve.metadata.view.model.list;

import java.util.List;
import java.util.Set;

import org.skyve.domain.Bean;
import org.skyve.metadata.MetaData;
import org.skyve.metadata.module.query.QueryColumn;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.wildcat.web.SortParameter;

public abstract class ListModel<T extends Bean> implements MetaData {
	private static final long serialVersionUID = -5786617076399299709L;

	private T bean;
	public T getBean() {
		return bean;
	}
	public void setBean(T bean) {
		this.bean = bean;
	}

	private int startRow = Integer.MIN_VALUE;
	private int endRow = Integer.MIN_VALUE;
	private SortParameter[] sorts;
	private AggregateFunction summary;
	private String selectedTagId;
	
	protected final int getStartRow() {
		return startRow;
	}
	
	public final void setStartRow(int startRow) {
		this.startRow = startRow;
	}

	protected final int getEndRow() {
		return endRow;
	}

	public void setEndRow(int endRow) {
		this.endRow = endRow;
	}

	protected final SortParameter[] getSortParameters() {
		return sorts;
	}

	public void setSortParameters(SortParameter[] sorts) {
		this.sorts = sorts;
	}

	protected final AggregateFunction getSummary() {
		return summary;
	}

	public void setSummary(AggregateFunction summary) {
		this.summary = summary;
	}

	protected final String getSelectedTagId() {
		return selectedTagId;
	}

	public void setSelectedTagId(String selectedTagId) {
		this.selectedTagId = selectedTagId;
	}

	public abstract List<QueryColumn> getColumns();
	
	public abstract Set<String> getProjections();
	
	public abstract Filter getFilter() throws Exception;
	public abstract Filter newFilter() throws Exception;
	public abstract Page fetch() throws Exception;
	public abstract AutoClosingIterable<Bean> iterate() throws Exception;
}
