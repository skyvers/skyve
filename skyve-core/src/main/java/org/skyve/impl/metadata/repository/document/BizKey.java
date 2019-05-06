package org.skyve.impl.metadata.repository.document;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.skyve.impl.domain.types.jaxb.CDATAAdapter;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaData;

@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public class BizKey implements MetaData {
	private static final long serialVersionUID = -2323204335740515884L;

	private String expression;
	private String code;

	public String getExpression() {
		return expression;
	}
	@XmlAttribute
	public void setExpression(String expression) {
		this.expression = UtilImpl.processStringValue(expression);
	}
	
	public String getCode() {
		return code;
	}
	@XmlValue
	@XmlJavaTypeAdapter(CDATAAdapter.class)
	public void setCode(String code) {
		this.code = UtilImpl.processStringValue(code);
	}
}
