package org.skyve.wildcat.metadata.repository.document;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;

import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.XMLUtil;

@XmlType(namespace = XMLUtil.DOCUMENT_NAMESPACE)
public class BizKey {
	private String expression;
	private String code;

	@XmlAttribute
	public String getExpression() {
		return expression;
	}
	public void setExpression(String expression) {
		this.expression = UtilImpl.processStringValue(expression);
	}
	
	@XmlValue
	public String getCode() {
		return code;
	}
	public void setCode(String code) {
		this.code = UtilImpl.processStringValue(code);
	}
}
