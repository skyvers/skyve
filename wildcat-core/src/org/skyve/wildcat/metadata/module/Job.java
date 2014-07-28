package org.skyve.wildcat.metadata.module;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;

import org.skyve.wildcat.metadata.repository.NamedMetaData;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.XMLUtil;

@XmlType(namespace = XMLUtil.MODULE_NAMESPACE,
			propOrder = {"displayName", "className", "shortDescription"})
public class Job extends NamedMetaData implements org.skyve.metadata.module.Job {
	/**
	 * For Serialization
	 */
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

	private String shortDescription;
	
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
	public String getShortDescription() {
		return shortDescription;
	}

	@XmlAttribute
	public void setShortDescription(String shortDescription) {
		this.shortDescription = UtilImpl.processStringValue(shortDescription);
	}

	@Override
	public String getOwningModuleName() {
		return owningModuleName;
	}

	@XmlTransient
	public void setOwningModuleName(String owningModuleName) {
		this.owningModuleName = owningModuleName;
	}
}
