package org.skyve.impl.metadata.repository.module;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.SerializableMetaData;

@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE, 
			name = "document",
			propOrder = {"ref", "defaultQueryName", "moduleRef"})
public class ModuleDocument implements SerializableMetaData {
	private static final long serialVersionUID = -5925139560927455582L;

	private String ref;
	private String defaultQueryName;
	private String moduleRef;

	public String getDefaultQueryName() {
		return defaultQueryName;
	}

	@XmlAttribute
	public void setDefaultQueryName(String defaultQueryName) {
		this.defaultQueryName = UtilImpl.processStringValue(defaultQueryName);
	}

	public String getRef() {
		return ref;
	}

	@XmlAttribute(required = true)
	public void setRef(String ref) {
		this.ref = UtilImpl.processStringValue(ref);
	}

	public String getModuleRef() {
		return moduleRef;
	}

	@XmlAttribute
	public void setModuleRef(String moduleRef) {
		this.moduleRef = UtilImpl.processStringValue(moduleRef);
	}
}
