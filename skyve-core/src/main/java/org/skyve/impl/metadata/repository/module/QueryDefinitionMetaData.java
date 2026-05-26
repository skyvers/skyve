package org.skyve.impl.metadata.repository.module;

import org.skyve.impl.domain.types.jaxb.CDATAAdapter;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.util.Util;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * Abstract JAXB base for query definitions that embed a query body in a module
 * descriptor.
 *
 * <p>Adds a description, documentation, and an optional execution timeout to the
 * base {@link QueryMetaData} contract.  Concrete subclasses
 * ({@link MetaDataQueryMetaData}, {@link SQLMetaData}, {@link BizQLMetaData})
 * add the query-body property.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see MetaDataQueryMetaData
 * @see SQLMetaData
 * @see BizQLMetaData
 */
@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE, 
			propOrder = {"documentation", "description", "timeoutInSeconds"})
public abstract class QueryDefinitionMetaData extends QueryMetaData {
	private static final long serialVersionUID = 3163827058170250318L;

	private String documentation;
	protected String description;
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
	
	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE)
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
