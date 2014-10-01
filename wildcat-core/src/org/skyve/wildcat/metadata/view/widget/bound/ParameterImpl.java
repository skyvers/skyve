package org.skyve.wildcat.metadata.view.widget.bound;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

import org.skyve.metadata.view.widget.bound.Parameter;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.XMLUtil;

@XmlType(namespace = XMLUtil.VIEW_NAMESPACE, propOrder = {"name", "value"})
public class ParameterImpl extends AbstractBound implements Parameter {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 3545853099050411888L;

	/**
	 * Parameter name.
	 */
	private String name;

	/**
	 * Literal value.
	 */
	private String value;

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
}
