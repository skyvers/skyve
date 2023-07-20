package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.MetaDataQueryColumnMetaData;
import org.skyve.impl.metadata.repository.module.MetaDataQueryProjectedColumnMetaData;
import org.skyve.metadata.FormatterName;
import org.skyve.metadata.module.query.MetaDataQueryProjectedColumn;
import org.skyve.metadata.view.TextOutput.Sanitisation;

public class FluentMetaDataQueryProjectedColumn extends FluentMetaDataQueryColumn<FluentMetaDataQueryProjectedColumn> {
	private MetaDataQueryProjectedColumnMetaData column = null;
	
	public FluentMetaDataQueryProjectedColumn() {
		column = new MetaDataQueryProjectedColumnMetaData();
	}

	public FluentMetaDataQueryProjectedColumn(MetaDataQueryProjectedColumnMetaData column) {
		this.column = column;
	}

	public FluentMetaDataQueryProjectedColumn from(@SuppressWarnings("hiding") MetaDataQueryProjectedColumn column) {
		super.from(column);
		projected(column.isProjected());
		expression(column.getExpression());
		sortable(column.isSortable());
		filterable(column.isFilterable());
		editable(column.isEditable());
		escape(column.isEscape());
		sanitise(column.getSanitise());
		formatter(column.getFormatterName());
		customFormatter(column.getCustomFormatterName());
		return this;
	}
	
	public FluentMetaDataQueryProjectedColumn projected(boolean projected) {
		column.setProjected(projected ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentMetaDataQueryProjectedColumn expression(String expression) {
		column.setExpression(expression);
		return this;
	}

	public FluentMetaDataQueryProjectedColumn sortable(boolean sortable) {
		column.setSortable(sortable ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentMetaDataQueryProjectedColumn filterable(boolean filterable) {
		column.setFilterable(filterable ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentMetaDataQueryProjectedColumn editable(boolean editable) {
		column.setEditable(editable ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentMetaDataQueryProjectedColumn escape(boolean escape) {
		column.setEscape(escape ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}
	
	public FluentMetaDataQueryProjectedColumn sanitise(Sanitisation sanitise) {
		column.setSanitise(sanitise);
		return this;
	}

	public FluentMetaDataQueryProjectedColumn formatter(FormatterName formatterName) {
		column.setFormatterName(formatterName);
		return this;
	}

	public FluentMetaDataQueryProjectedColumn customFormatter(String customFormatterName) {
		column.setCustomFormatterName(customFormatterName);
		return this;
	}

	@Override
	public MetaDataQueryColumnMetaData get() {
		return column;
	}
}
