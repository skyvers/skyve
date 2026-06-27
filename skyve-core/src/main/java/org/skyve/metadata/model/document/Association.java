package org.skyve.metadata.model.document;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlType;

/**
 * A to-one relational attribute linking a document to another document instance.
 *
 * <p>Associations come in three flavours declared by {@link AssociationType}:
 * <ul>
 *   <li>{@link AssociationType#composition composition} — the associated bean is
 *       cascade-deleted when the owner is deleted.</li>
 *   <li>{@link AssociationType#aggregation aggregation} — the associated bean lives
 *       independently; only the foreign key is removed on owner deletion.</li>
 *   <li>{@link AssociationType#embedded embedded} — the associated document's columns are
 *       inlined into the owner's table, optionally with a column prefix.</li>
 * </ul>
 *
 * @see Collection
 * @see Inverse
 */
public interface Association extends Reference {
	/**
	 * Association cardinality and lifecycle variants.
	 *
	 * <p>The chosen type determines the ORM mapping and the cascade-delete behaviour.
	 */
	@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	@SuppressWarnings("java:S115") // Suppress "Constant names should comply with a naming convention" as these are not constants but enum values
	public enum AssociationType implements ReferenceType {
		/**
		 * The associated bean is persisted to another table but is cascade deleted when the owning bean is deleted.
		 */
		composition, 
		
		/**
		 * The associated bean is persisted to another table but is orphaned when the referencing bean is deleted.
		 */
		aggregation,
		
		/**
		 * The associated bean is persisted in the owning document's table and does not exist when the owning bean is deleted.
		 */
		embedded;
	}

	/**
	 * Returns the {@link AssociationType} that governs the ORM mapping and lifecycle of this association.
	 *
	 * @return the association type; never {@code null}
	 */
	@Override
	AssociationType getType();
	
	/**
	 * Used only for embedded associations to introduce a column prefix for all embedded document attributes
	 * that are inlined in the owning document's database table.
	 * This allows columns that would otherwise have the same name to be created with unique names.
	 * @return	The prefix (with no underscores).
	 */
	String getEmbeddedColumnsPrefix();

	/**
	 * This determines whether to create an index on the association foreign key column.
	 * Set to true if the database does not implement foreign keys with indexes - ie SQLServer.
	 */
	Boolean getDatabaseIndex();
}
