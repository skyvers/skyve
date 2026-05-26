package org.skyve.impl.metadata.repository.document;

import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.domain.types.jaxb.CDATAAdapter;
import org.skyve.impl.metadata.repository.NamedMetaData;
import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.model.Attribute.UsageType;
import org.skyve.metadata.model.document.Condition;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * JAXB-annotated descriptor for a {@code <condition>} element in a document XML file.
 *
 * <p>Declares a named boolean condition (expression) on a document that can be
 * referenced by view widgets (visibility, disability) and business rules.  Extends
 * {@link NamedMetaData} to bind the condition to its symbolic name.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see org.skyve.impl.metadata.model.document.ConditionImpl
 */
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE, 
			name = "condition",
			propOrder = {"documentation", "description", "expression", "usage", "properties"})
public class ConditionMetaData extends NamedMetaData implements Condition {
	private static final long serialVersionUID = -4665946292131120118L;

	private String description;
	private String documentation;
	private String expression;
	private UsageType usage;
	
	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

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
	
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
