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

	/**
	 * Returns developer-facing documentation for this query definition.
	 *
	 * @return the query documentation text, or {@code null} when unspecified
	 */
	public String getDocumentation() {
		return documentation;
	}

	/**
	 * Sets developer-facing documentation for this query definition.
	 *
	 * <p>Side effects: normalises the supplied value with
	 * {@link UtilImpl#processStringValue(String)} before storing it.
	 *
	 * @param documentation the documentation text; blank values become {@code null}
	 */
	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE)
	@XmlJavaTypeAdapter(CDATAAdapter.class)
	public void setDocumentation(String documentation) {
		this.documentation = UtilImpl.processStringValue(documentation);
	}

	/**
	 * Returns the untranslated description configured for this query definition.
	 *
	 * @return the raw description value, or {@code null} when unspecified
	 */
	public String getDescription() {
		return description;
	}

	/**
	 * Returns the description translated for the current locale.
	 *
	 * @return the localised description, or {@code null} when no description is defined
	 */
	public String getLocalisedDescription() {
		return Util.i18n(description);
	}
	
	/**
	 * Sets the untranslated description configured for this query definition.
	 *
	 * <p>Side effects: normalises the supplied value with
	 * {@link UtilImpl#processStringValue(String)} before storing it.
	 *
	 * @param description the raw description value; blank values become {@code null}
	 */
	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE)
	@XmlJavaTypeAdapter(CDATAAdapter.class)
	public void setDescription(String description) {
		this.description = UtilImpl.processStringValue(description);
	}

	/**
	 * Returns the query execution timeout in seconds.
	 *
	 * @return the timeout in seconds, or {@code null} when no timeout is configured
	 */
	public Integer getTimeoutInSeconds() {
		return timeoutInSeconds;
	}

	/**
	 * Sets the query execution timeout in seconds.
	 *
	 * @param timeoutInSeconds the timeout in seconds; {@code null} disables timeout enforcement
	 */
	@XmlAttribute
	public void setTimeoutInSeconds(Integer timeoutInSeconds) {
		this.timeoutInSeconds = timeoutInSeconds;
	}
}
