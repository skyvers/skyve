package org.skyve.metadata.model;

import org.skyve.impl.metadata.repository.NamedMetaData;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Metadata describing how a document is persisted to an RDBMS table, including
 * optional catalog/schema qualification and inheritance strategy.
 */
@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE, 
			propOrder = {"schema", "catalog", "strategy", "discriminator", "cacheName"}) 
public class Persistent extends NamedMetaData {
	private static final long serialVersionUID = -6359398747055206964L;

	/**
	 * Strategy for mapping document inheritance to database structures.
	 */
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
	
	/**
	 * @return the explicitly configured schema, or {@code null} if not set
	 */
    public String getSchema() {
        return schema;
    }

	/**
	 * Set the schema name used to qualify the table identifier.
	 */
    @XmlAttribute(required = false)
    public void setSchema(String schema) {
        this.schema = UtilImpl.processStringValue(schema);
    }

	/**
	 * @return the explicitly configured catalog, or {@code null} if not set
	 */
    public String getCatalog() {
        return catalog;
    }

	/**
	 * Set the catalog name used to qualify the table identifier.
	 */
    @XmlAttribute(required = false)
    public void setCatalog(String catalog) {
        this.catalog = UtilImpl.processStringValue(catalog);
    }
    
	/**
	 * @return the inheritance mapping strategy, or {@code null} if not set
	 */
	public ExtensionStrategy getStrategy() {
		return strategy;
	}
	
	/**
	 * Set the inheritance mapping strategy.
	 */
	@XmlAttribute
	public void setStrategy(ExtensionStrategy strategy) {
		this.strategy = strategy;
	}

	/**
	 * @return the discriminator column/value used for inheritance, or {@code null} if not set
	 */
    public String getDiscriminator() {
		return discriminator;
	}

	/**
	 * Set the discriminator column/value used for inheritance.
	 */
    @XmlAttribute
	public void setDiscriminator(String discriminator) {
		this.discriminator = UtilImpl.processStringValue(discriminator);
	}

	/**
	 * @return the configured cache name, or {@code null} if not set
	 */
	public String getCacheName() {
		return cacheName;
	}
	
	/**
	 * Set the cache name for the persistence layer.
	 */
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
	
	/**
	 * Indicates if this document is polymorphically mapped, that is it is mapped without a persistent name.
	 * This means that the document is mapped to the same tables as its child documents and requires different integrity checks.
	 * @return true if this document is polymorphically mapped, otherwise false.
	 */
	public boolean isPolymorphicallyMapped() {
		return (strategy == ExtensionStrategy.mapped) && (getName() == null);
	}
}
