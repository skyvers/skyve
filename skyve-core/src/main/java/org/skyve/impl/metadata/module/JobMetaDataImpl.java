package org.skyve.impl.metadata.module;

import java.util.LinkedHashMap;
import java.util.Map;

import org.skyve.impl.metadata.repository.NamedMetaData;
import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.module.JobMetaData;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * JAXB-annotated implementation of {@link JobMetaData}, representing a background
 * job declared in a module descriptor.
 *
 * <p>A job declaration maps a symbolic name to a fully-qualified class that the Quartz
 * scheduler can instantiate and execute.  The {@code displayName} and {@code description}
 * fields carry the human-readable label and documentation; the {@code className} field
 * identifies the concrete {@link org.skyve.job.Job} subclass to run.
 *
 * <p>Threading: not thread-safe.  Instances are populated during metadata loading
 * and are read-only once placed in the repository cache.
 *
 * @see JobMetaData
 * @see org.skyve.job.Job
 */
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
	private Map<String, String> properties = new LinkedHashMap<>();
	
	@Override
	@SuppressWarnings("null") // conversion rejects a null name
	public @Nonnull String getName() {
		return super.getName();
	}

	@Override
	public @Nonnull String getDisplayName() {
		return displayName;
	}

	@XmlAttribute(required = true)
	public void setDisplayName(@Nonnull String displayName) {
		this.displayName = UtilImpl.processStringValue(displayName);
	}

	@Override
	public @Nonnull String getClassName() {
		return className;
	}

	@XmlAttribute(required = true)
	public void setClassName(@Nonnull String className) {
		this.className = UtilImpl.processStringValue(className);
	}
	
	@Override
	public @Nullable String getDescription() {
		return description;
	}

	@XmlAttribute
	public void setDescription(@Nullable String description) {
		this.description = UtilImpl.processStringValue(description);
	}

	@Override
	public @Nonnull Map<String, String> getProperties() {
		return properties;
	}
	
	@Override
	public @Nullable String getOwningModuleName() {
		return owningModuleName;
	}

	/**
	 * This is called in convert method.
	 */
	@XmlTransient
	public void setOwningModuleName(@Nullable String owningModuleName) {
		this.owningModuleName = owningModuleName;
	}
}
