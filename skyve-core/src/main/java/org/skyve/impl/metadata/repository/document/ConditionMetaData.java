package org.skyve.impl.metadata.repository.document;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.skyve.impl.domain.types.jaxb.CDATAAdapter;
import org.skyve.impl.metadata.repository.NamedMetaData;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.model.Attribute.UsageType;
import org.skyve.metadata.model.document.Condition;

@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE, 
			name = "condition",
			propOrder = {"documentation", "description", "expression"})
public class ConditionMetaData extends NamedMetaData implements Condition {
	private static final long serialVersionUID = -4665946292131120118L;

	private String description;
	private String documentation;
	private String expression;
	private UsageType usage;
	
	@Override
	public String getDocumentation() {
		return documentation;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	@XmlJavaTypeAdapter(CDATAAdapter.class)
	public void setDocumentation(String documentation) {
		this.documentation = UtilImpl.processStringValue(documentation);
	}
	
	@Override
	public String getDescription() {
		return description;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	@XmlJavaTypeAdapter(CDATAAdapter.class)
	public void setDescription(String description) {
		this.description = UtilImpl.processStringValue(description);
	}
	
	@Override
	public String getExpression() {
		return expression;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE, required = true)
	@XmlJavaTypeAdapter(CDATAAdapter.class)
	public void setExpression(String expression) {
		this.expression = UtilImpl.processStringValue(expression);
	}

	@Override
	public UsageType getUsage() {
		return usage;
	}

	@XmlAttribute
	public void setUsage(UsageType usage) {
		this.usage = usage;
	}
}
