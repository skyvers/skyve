package org.skyve.impl.metadata.repository.module;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.skyve.impl.domain.types.jaxb.CDATAAdapter;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

@XmlRootElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "column")
@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE, 
			propOrder = {"projected",
							"expression",
							"sortable", 
							"filterable", 
							"editable"})
public class MetaDataQueryProjectedColumnMetaData extends MetaDataQueryColumnMetaData {
	private static final long serialVersionUID = 7831641243591117311L;

	// Indicates whether the column is selected - appears in the projection.
	private Boolean projected;

	// The projection expression
	private String expression;

	// Indicates if the user can sort this column in list view
	private Boolean sortable;

	// Indicates if the user can filter this column in list view
	private Boolean filterable;

	// Indicates if the user can edit the values in this column in list view
	private Boolean editable;

	public Boolean getProjected() {
		return projected;
	}

	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE)
	public void setProjected(Boolean projected) {
		this.projected = projected;
	}

	public String getExpression() {
		return expression;
	}

	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE)
	@XmlJavaTypeAdapter(CDATAAdapter.class)
	public void setExpression(String expression) {
		this.expression = UtilImpl.processStringValue(expression);
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
