package org.skyve.metadata.model;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.metadata.repository.NamedMetaData;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.model.document.Cache;

@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE, 
			propOrder = {"schema", "catalog", "strategy", "discriminator", "cache"}) 
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
	private Cache cache;
	
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

	public Cache getCache() {
		return cache;
	}
	
	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public void setCache(Cache cache) {
		this.cache = cache;
	}

    public String getPersistentIdentifier() {
    	String name = getName();

    	if (name == null) {
    		return null;
    	}
    	if ((schema == null) && (catalog == null)) {
    		return name;
    	}
    	
    	StringBuilder result = new StringBuilder(64);
    	if (schema != null) {
    		result.append(schema).append('.');
    	}
    	if (catalog != null) {
    		result.append(catalog).append('.');
    	}
    	result.append(name);
    	
    	return result.toString();
    }
}
