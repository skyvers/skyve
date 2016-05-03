package org.skyve.impl.metadata.repository.module;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLUtil;
import org.skyve.metadata.FilterOperator;
import org.skyve.metadata.SortDirection;

@XmlType(namespace = XMLUtil.MODULE_NAMESPACE, 
			propOrder = {"binding", 
							"displayName",
							"sortOrder", 
							"hidden", 
							"sortable", 
							"filterable", 
							"editable",
							"name", 
							"expression", 
							"projected",
							"filterOperator", 
							"filterExpression"})
public class Column {
	// The name of the property within the bean list. Can be null.
	private String name;

	// The attribute name, or binding for the column
	private String binding;

	// The projection expression
	private String expression;

	// The display name or title
	private String displayName;

	// The sort direction
	private SortDirection sortOrder;

	// The conditional operator to filter with
	private FilterOperator filterOperator;

	// The conditional expression to filter with
	private String filterExpression;

	// Indicates whether the column is selected - appears in the projection.
	private Boolean projected;

	// Indicates if the column is hidden in list view by default (can be shown by the user)
	private Boolean hidden;

	// Indicates if the user can sort this column in list view
	private Boolean sortable;

	// Indicates if the user can filter this column in list view
	private Boolean filterable;

	// Indicates if the user can edit the values in this column in list view
	private Boolean editable;

	public String getName() {
		return name;
	}

	@XmlElement(namespace = XMLUtil.MODULE_NAMESPACE)
	public void setName(String name) {
		this.name = UtilImpl.processStringValue(name);
	}

	public String getBinding() {
		return binding;
	}

	@XmlAttribute
	public void setBinding(String binding) {
		this.binding = UtilImpl.processStringValue(binding);
	}

	public String getExpression() {
		return expression;
	}

	@XmlElement(namespace = XMLUtil.MODULE_NAMESPACE)
	public void setExpression(String expression) {
		this.expression = UtilImpl.processStringValue(expression);
	}

	public String getDisplayName() {
		return displayName;
	}

	@XmlAttribute
	public void setDisplayName(String displayName) {
		this.displayName = UtilImpl.processStringValue(displayName);
	}

	public FilterOperator getFilterOperator() {
		return filterOperator;
	}

	@XmlElement(namespace = XMLUtil.MODULE_NAMESPACE)
	public void setFilterOperator(FilterOperator filterOperator) {
		this.filterOperator = filterOperator;
	}

	public String getFilterExpression() {
		return filterExpression;
	}

	@XmlElement(namespace = XMLUtil.MODULE_NAMESPACE)
	public void setFilterExpression(String filterExpression) {
		this.filterExpression = UtilImpl.processStringValue(filterExpression);
	}

	public SortDirection getSortOrder() {
		return sortOrder;
	}

	@XmlAttribute
	public void setSortOrder(SortDirection sortOrder) {
		this.sortOrder = sortOrder;
	}

	public Boolean getProjected() {
		return projected;
	}

	@XmlElement(namespace = XMLUtil.MODULE_NAMESPACE)
	public void setProjected(Boolean projected) {
		this.projected = projected;
	}

	public Boolean getHidden() {
		return hidden;
	}

	@XmlAttribute
	public void setHidden(Boolean hidden) {
		this.hidden = hidden;
	}

	public Boolean getFilterable() {
		return filterable;
	}

	@XmlAttribute
	public void setFilterable(Boolean filterable) {
		this.filterable = filterable;
	}

	public Boolean getSortable() {
		return sortable;
	}

	@XmlAttribute
	public void setSortable(Boolean sortable) {
		this.sortable = sortable;
	}

	public Boolean getEditable() {
		return editable;
	}

	@XmlAttribute
	public void setEditable(Boolean editable) {
		this.editable = editable;
	}
}
