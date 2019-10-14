package org.skyve.impl.metadata.view.widget.bound;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.widget.bound.Parameter;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE, propOrder = {"name", "value", "valueBinding"})
public class ParameterImpl implements Parameter {
	private static final long serialVersionUID = 3545853099050411888L;

	/**
	 * Parameter name.
	 */
	private String name;

	/**
	 * Literal value.
	 */
	private String value;

	/**
	 * Value derived from a binding.
	 */
	private String valueBinding;

	@Override
	public String getName() {
		return name;
	}

	@XmlAttribute(required = true)
	public void setName(String name) {
		this.name = UtilImpl.processStringValue(name);
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
