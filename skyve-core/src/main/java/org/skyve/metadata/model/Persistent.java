package org.skyve.metadata.model;

import org.skyve.impl.metadata.repository.NamedMetaData;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE, 
			propOrder = {"schema", "catalog", "strategy", "discriminator", "cacheName"}) 
public class Persistent extends NamedMetaData {
	private static final long serialVersionUID = -6359398747055206964L;

	@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public static enum ExtensionStrategy {
		single,
		joined,
		mapped
	}
	
	private String schema;
    private String catalog;
	private ExtensionStrategy strategy;
	private String discriminator;
	private String cacheName;
	
	public Persistent() {
		// nothing to see here
	}
	
    public String getSchema() {
        return schema;
    }

    @XmlAttribute(required = false)
    public void setSchema(String schema) {
        this.schema = UtilImpl.processStringValue(schema);
    }

    public String getCatalog() {
        return catalog;
    }

    @XmlAttribute(required = false)
    public void setCatalog(String catalog) {
        this.catalog = UtilImpl.processStringValue(catalog);
    }
    
	public ExtensionStrategy getStrategy() {
		return strategy;
	}
	
	@XmlAttribute
	public void setStrategy(ExtensionStrategy strategy) {
		this.strategy = strategy;
	}

    public String getDiscriminator() {
		return discriminator;
	}

    @XmlAttribute
	public void setDiscriminator(String discriminator) {
		this.discriminator = UtilImpl.processStringValue(discriminator);
	}

	public String getCacheName() {
		return cacheName;
	}
	
	@XmlElement(name = "cache", namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public void setCacheName(String cacheName) {
		this.cacheName = cacheName;
	}

	public String getPersistentIdentifier() {
		String name = getName();
		if (name == null) {
			return null;
		}

		String c = (catalog == null) ? UtilImpl.CATALOG : catalog;
		String s = (schema == null) ? UtilImpl.SCHEMA : schema;
		if ((c == null) && (s == null)) {
			return name;
		}

		StringBuilder result = new StringBuilder(64);
		if (c != null) {
			result.append(c).append('.');
		}
		if (s != null) {
			result.append(s).append('.');
		}
		result.append(name);

		return result.toString();
	}
}
