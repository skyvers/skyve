package org.skyve.impl.metadata.repository.module;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.metadata.repository.NamedMetaData;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLUtil;

@XmlType(namespace = XMLUtil.MODULE_NAMESPACE, 
			name = "query",
			propOrder = {"documentation", "description"})
public abstract class QueryMetaData extends NamedMetaData {
	private String documentation;
	private String description;

	public String getDocumentation() {
		return documentation;
	}

	@XmlElement(namespace = XMLUtil.MODULE_NAMESPACE)
	public void setDocumentation(String documentation) {
		this.documentation = UtilImpl.processStringValue(documentation);
	}

	public String getDescription() {
		return description;
	}

	@XmlElement(namespace = XMLUtil.MODULE_NAMESPACE, required = true)
	public void setDescription(String description) {
		this.description = UtilImpl.processStringValue(description);
	}
}
