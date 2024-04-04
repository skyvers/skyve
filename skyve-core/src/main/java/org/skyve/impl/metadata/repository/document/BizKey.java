package org.skyve.impl.metadata.repository.document;

import org.skyve.impl.domain.types.jaxb.CDATAAdapter;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.SerializableMetaData;
import org.skyve.metadata.model.Attribute.Sensitivity;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.XmlValue;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public class BizKey implements SerializableMetaData {
	private static final long serialVersionUID = -2323204335740515884L;

	private String expression;
	private String code;
	private Sensitivity sensitivity;

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

	public Sensitivity getSensitivity() {
		return sensitivity;
	}
	@XmlAttribute
	public void setSensitivity(Sensitivity sensitivity) {
		this.sensitivity = sensitivity;
	}
}
