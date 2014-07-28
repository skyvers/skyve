package org.skyve.wildcat.metadata.repository.document;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;

import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.XMLUtil;

@XmlType(namespace = XMLUtil.DOCUMENT_NAMESPACE)
public class Condition {
	private String name;

	private String expression;

	public String getName() {
		return name;
	}

	@XmlAttribute(required = true)
	public void setName(String name) {
		this.name = UtilImpl.processStringValue(name);
	}

	public String getExpression() {
		return expression;
	}

	@XmlValue
	// cannot extend NamedAttribute and use XMLValue
	public void setExpression(String expression) {
		this.expression = UtilImpl.processStringValue(expression);
	}
}
