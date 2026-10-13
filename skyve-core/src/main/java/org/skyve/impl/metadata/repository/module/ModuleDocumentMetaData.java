package org.skyve.impl.metadata.repository.module;

import java.util.LinkedHashMap;
import java.util.Map;

import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.DecoratedMetaData;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
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
	private Map<String, String> properties = new LinkedHashMap<>();

	public @Nullable String getDefaultQueryName() {
		return defaultQueryName;
	}

	@XmlAttribute
	public void setDefaultQueryName(@Nullable String defaultQueryName) {
		this.defaultQueryName = UtilImpl.processStringValue(defaultQueryName);
	}

	public @Nullable String getRef() {
		return ref;
	}

	@XmlAttribute(required = true)
	public void setRef(@Nullable String ref) {
		this.ref = UtilImpl.processStringValue(ref);
	}

	public @Nullable String getModuleRef() {
		return moduleRef;
	}

	@XmlAttribute
	public void setModuleRef(@Nullable String moduleRef) {
		this.moduleRef = UtilImpl.processStringValue(moduleRef);
	}
	
	@Override
	public @Nonnull Map<String, String> getProperties() {
		return properties;
	}
}
