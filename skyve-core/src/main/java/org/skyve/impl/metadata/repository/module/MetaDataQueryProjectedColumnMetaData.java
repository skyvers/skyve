package org.skyve.impl.metadata.repository.module;

import org.skyve.impl.domain.types.jaxb.CDATAAdapter;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.FormatterName;
import org.skyve.metadata.view.FormattedText;
import org.skyve.metadata.view.TextOutput;

import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
/**
 * JAXB-annotated descriptor for a {@code <column>} projected data column in a
 * metadata query.
 *
 * <p>Adds projected flag, MVEL expression, sortable/filterable/editable flags,
 * HTML-escape and sanitisation settings, and formatter specification to the base
 * {@link MetaDataQueryColumnMetaData} contract.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see MetaDataQueryColumnMetaData
 * @see MetaDataQueryContentColumnMetaData
 */@XmlRootElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "column")
@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE, 
			propOrder = {"projected",
							"expression",
							"sortable", 
							"filterable", 
							"editable",
							"escape",
							"sanitise",
							"formatterName",
							"customFormatterName"})
public class MetaDataQueryProjectedColumnMetaData extends MetaDataQueryColumnMetaData implements TextOutput, FormattedText {
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

	// Escape syntax relating to the view technology - <, > etc for HTML
	private Boolean escape;
	
	// Sanitise expressions relating to the view technology - <script> for HTML (to prevent XSS)
	private Sanitisation sanitise;

	// A built-in Skyve formatter
	private FormatterName formatterName;
	
	// A custom formatter for the Skyve project (added from Customisations class specified in JSON)
	private String customFormatterName;
	
	public @Nullable Boolean getProjected() {
		return projected;
	}

	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE)
	public void setProjected(@Nullable Boolean projected) {
		this.projected = projected;
	}

	public @Nullable String getExpression() {
		return expression;
	}

	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE)
	@XmlJavaTypeAdapter(CDATAAdapter.class)
	public void setExpression(@Nullable String expression) {
		this.expression = UtilImpl.processStringValue(expression);
	}

	public @Nullable Boolean getFilterable() {
		return filterable;
	}

	@XmlAttribute
	public void setFilterable(@Nullable Boolean filterable) {
		this.filterable = filterable;
	}

	public @Nullable Boolean getSortable() {
		return sortable;
	}

	@XmlAttribute
	public void setSortable(@Nullable Boolean sortable) {
		this.sortable = sortable;
	}

	public @Nullable Boolean getEditable() {
		return editable;
	}

	@XmlAttribute
	public void setEditable(@Nullable Boolean editable) {
		this.editable = editable;
	}
	
	@Override
	public @Nullable Boolean getEscape() {
		return escape;
	}

	@XmlAttribute
	public void setEscape(@Nullable Boolean escape) {
		this.escape = escape;
	}

	@Override
	public @Nullable Sanitisation getSanitise() {
		return sanitise;
	}

	@XmlAttribute
	public void setSanitise(@Nullable Sanitisation sanitise) {
		this.sanitise = sanitise;
	}

	@Override
	public @Nullable FormatterName getFormatterName() {
		return formatterName;
	}

	@XmlAttribute(name = "formatter")
	public void setFormatterName(@Nullable FormatterName formatterName) {
		this.formatterName = formatterName;
	}

	@Override
	public @Nullable String getCustomFormatterName() {
		return customFormatterName;
	}

	@XmlAttribute(name = "customFormatter")
	public void setCustomFormatterName(@Nullable String customFormatterName) {
		this.customFormatterName = UtilImpl.processStringValue(customFormatterName);
	}
}
