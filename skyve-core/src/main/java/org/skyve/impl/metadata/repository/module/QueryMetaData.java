package org.skyve.impl.metadata.repository.module;

import org.skyve.impl.domain.types.jaxb.CDATAAdapter;
import org.skyve.impl.metadata.repository.NamedMetaData;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.util.Util;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE, 
			name = "abstractQuery",
			propOrder = {"documentation", "description", "timeoutInSeconds"})
public abstract class QueryMetaData extends NamedMetaData {
	private static final long serialVersionUID = 3163827058170250318L;

	private String documentation;
	private String description;
	private Integer timeoutInSeconds;

	public String getDocumentation() {
		return documentation;
	}

	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE)
	@XmlJavaTypeAdapter(CDATAAdapter.class)
	public void setDocumentation(String documentation) {
		this.documentation = UtilImpl.processStringValue(documentation);
	}

	public String getDescription() {
		return description;
	}

	public String getLocalisedDescription() {
		return Util.i18n(description);
	}
	
	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE, required = true)
	@XmlJavaTypeAdapter(CDATAAdapter.class)
	public void setDescription(String description) {
		this.description = UtilImpl.processStringValue(description);
	}

	public Integer getTimeoutInSeconds() {
		return timeoutInSeconds;
	}

	@XmlAttribute
	public void setTimeoutInSeconds(Integer timeoutInSeconds) {
		this.timeoutInSeconds = timeoutInSeconds;
	}
}
