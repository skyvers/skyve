package org.skyve.impl.metadata.module;

import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.metadata.repository.NamedMetaData;
import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.module.JobMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

@XmlType(name = "job", 
			namespace = XMLMetaData.MODULE_NAMESPACE,
			propOrder = {"displayName", "className", "description", "properties"})
public class JobMetaDataImpl extends NamedMetaData implements JobMetaData {
	private static final long serialVersionUID = 5214082047746890555L;

	/**
	 * The name of the module that owns this job.
	 */
	private String owningModuleName;
	
	/**
	 * The name shown in job related UIs.
	 */
	private String displayName;
	
	/**
	 * The fully qualified classname of the converter implementation.
	 */
	private String className;

	private String description;
	
	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();
	
	@Override
	public String getDisplayName() {
		return displayName;
	}

	@XmlAttribute(required = true)
	public void setDisplayName(String displayName) {
		this.displayName = displayName;
	}

	@Override
	public String getClassName() {
		return className;
	}

	@XmlAttribute(required = true)
	public void setClassName(String className) {
		this.className = UtilImpl.processStringValue(className);
	}
	
	@Override
	public String getDescription() {
		return description;
	}

	@XmlAttribute
	public void setDescription(String description) {
		this.description = UtilImpl.processStringValue(description);
	}

	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
	
	@Override
	public String getOwningModuleName() {
		return owningModuleName;
	}

	/**
	 * This is called in convert method.
	 */
	@XmlTransient
	public void setOwningModuleName(String owningModuleName) {
		this.owningModuleName = owningModuleName;
	}
}
