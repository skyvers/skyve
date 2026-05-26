package org.skyve.impl.metadata.repository.module;

import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.DecoratedMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * JAXB-annotated descriptor for a document registration entry within a module
 * descriptor.
 *
 * <p>Identifies a document by reference name and (optionally) an external module
 * reference, enabling a module to include documents owned by another module.
 * An optional default query name overrides the module-level default.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see ModuleMetaData
 */
@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE, 
			name = "document",
			propOrder = {"ref", "defaultQueryName", "moduleRef", "properties"})
public class ModuleDocumentMetaData implements DecoratedMetaData {
	private static final long serialVersionUID = -5925139560927455582L;

	private String ref;
	private String defaultQueryName;
	private String moduleRef;

	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

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
	
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
