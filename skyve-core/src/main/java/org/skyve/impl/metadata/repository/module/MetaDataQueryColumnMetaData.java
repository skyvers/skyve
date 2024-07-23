package org.skyve.impl.metadata.repository.module;

import org.skyve.impl.domain.types.jaxb.CDATAAdapter;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.FilterOperator;
import org.skyve.metadata.SerializableMetaData;
import org.skyve.metadata.SortDirection;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE, 
			propOrder = {"binding", 
							"displayName",
							"sortOrder", 
							"hidden", 
							"name", 
							"filterOperator", 
							"filterExpression",
							"pixelWidth",
							"alignment"})
public abstract class MetaDataQueryColumnMetaData implements SerializableMetaData {
	private static final long serialVersionUID = 7831641243591117311L;

	// The name of the property within the bean list. Can be null.
	private String name;

	// The attribute name, or binding for the column
	private String binding;

	// The display name or title
	private String displayName;

	// The sort direction
	private SortDirection sortOrder;

	// The conditional operator to filter with
	private FilterOperator filterOperator;

	// The conditional expression to filter with
	private String filterExpression;

	// Indicates if the column is hidden in list view by default (can be shown by the user)
	private Boolean hidden;

	// If defined, the overridden pixel width of the column
	private Integer pixelWidth;

	// If defined, the overridden column alignment
	private HorizontalAlignment alignment;
	
	public String getName() {
		return name;
	}

	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE)
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

	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE)
	public void setFilterOperator(FilterOperator filterOperator) {
		this.filterOperator = filterOperator;
	}

	public String getFilterExpression() {
		return filterExpression;
	}

	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE)
	@XmlJavaTypeAdapter(CDATAAdapter.class)
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

	public Boolean getHidden() {
		return hidden;
	}

	@XmlAttribute
	public void setHidden(Boolean hidden) {
		this.hidden = hidden;
	}

	public Integer getPixelWidth() {
		return pixelWidth;
	}

	@XmlAttribute
	public void setPixelWidth(Integer pixelWidth) {
		this.pixelWidth = pixelWidth;
	}

	public HorizontalAlignment getAlignment() {
		return alignment;
	}

	@XmlAttribute
	public void setAlignment(HorizontalAlignment alignment) {
		this.alignment = alignment;
	}
}
