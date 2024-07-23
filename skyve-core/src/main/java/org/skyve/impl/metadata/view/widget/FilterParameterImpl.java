package org.skyve.impl.metadata.view.widget;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.FilterOperator;
import org.skyve.metadata.view.widget.FilterParameter;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlType;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE, propOrder = {"filterBinding", "operator", "value", "valueBinding"})
public final class FilterParameterImpl implements FilterParameter {
	private static final long serialVersionUID = -4191066734972894505L;

	/**
	 * Parameter name derived from a binding.
	 */
	private String filterBinding;
	
	/**
	 * Filter operator to apply.
	 */
	private FilterOperator operator;
	
	/**
	 * Literal value.
	 */
	private String value;

	/**
	 * Value derived from a binding.
	 */
	private String valueBinding;

	@Override
	public String getFilterBinding() {
		return filterBinding;
	}

	@XmlAttribute(required = true)
	public void setFilterBinding(String filterBinding) {
		this.filterBinding = UtilImpl.processStringValue(filterBinding);
	}

	@Override
	public FilterOperator getOperator() {
		return operator;
	}

	@XmlAttribute(required = true)
	public void setOperator(FilterOperator operator) {
		this.operator = operator;
	}

	@Override
	public String getValue() {
		return value;
	}

	@XmlAttribute(required = false)
	public void setValue(String value) {
		this.value = UtilImpl.processStringValue(value);
	}
	
	@Override
	public String getValueBinding() {
		return valueBinding;
	}

	@XmlAttribute(required = false)
	public void setValueBinding(String valueBinding) {
		this.valueBinding = UtilImpl.processStringValue(valueBinding);
	}
}
