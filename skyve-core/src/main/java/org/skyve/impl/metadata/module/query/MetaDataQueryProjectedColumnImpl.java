package org.skyve.impl.metadata.module.query;

import org.skyve.metadata.module.query.MetaDataQueryProjectedColumn;

public class MetaDataQueryProjectedColumnImpl extends AbstractMetaDataQueryColumn implements MetaDataQueryProjectedColumn {
	private static final long serialVersionUID = -6310887576572791787L;

	private String expression;

	private boolean projected = true;

	private boolean sortable = true;

	private boolean filterable = true;

	private boolean editable = true;
	
	@Override
	public String getExpression() {
		return expression;
	}

	public void setExpression(String expression) {
		this.expression = expression;
	}

	@Override
	public boolean isProjected() {
		return projected;
	}

	public void setSelected(boolean projected) {
		this.projected = projected;
	}

	@Override
	public boolean isFilterable() {
		return filterable;
	}

	public void setFilterable(boolean filterable) {
		this.filterable = filterable;
	}

	@Override
	public boolean isSortable() {
		return sortable;
	}

	public void setSortable(boolean sortable) {
		this.sortable = sortable;
	}

	@Override
	public boolean isEditable() {
		return editable;
	}

	public void setEditable(boolean editable) {
		this.editable = editable;
	}
}
