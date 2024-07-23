package org.skyve.metadata.model;

import org.skyve.impl.metadata.repository.NamedMetaData;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

import jakarta.annotation.Nullable;
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

	/**
	 * The RDBMS Table name replete with catalog and schema identifiers if defined.
	 * This identifier is used for creating SQL statements for documents.
	 * @return the persistent identifier
	 */
	public @Nullable String getPersistentIdentifier() {
		return determinePersistentIdentifier(getName(),
												(catalog == null) ? UtilImpl.CATALOG : catalog,
												(schema == null) ? UtilImpl.SCHEMA : schema);
	}
	
	/**
	 * The RDBMS Table name with the catalog and schema if defined in the metadata.
	 * The JSON catalog and schema are not used to determine the agnostic identifier.
	 * This identifier is agnostic of the RDBMS and the default catalog and schema the data store belongs to.
	 * This identifier is used for database independent backups for the CSV file names.
	 * @return
	 */
	public @Nullable String getAgnosticIdentifier() {
		return determinePersistentIdentifier(getName(), catalog, schema);
	}
	
	/**
	 * Determines the persistent identifier given a table name, catalog and schema.
	 * @param name	The table name
	 * @param catalog	The catalog
	 * @param schema	The schema
	 * @return	The identifier
	 */
	public static @Nullable String determinePersistentIdentifier(@Nullable String name,
																	@Nullable String catalog,
																	@Nullable String schema) {
		if (name == null) {
			return null;
		}

		if ((catalog == null) && (schema == null)) {
			return name;
		}

		StringBuilder result = new StringBuilder(64);
		if (catalog != null) {
			result.append(catalog).append('.');
		}
		if (schema != null) {
			result.append(schema).append('.');
		}
		result.append(name);

		return result.toString();
	}
}
