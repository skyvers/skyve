package org.skyve.metadata.model.document;

/**
 * An owning-side relational attribute that carries a reference type and an optional default query.
 *
 * <p>Both {@link Association} and {@link Collection} extend this interface. The
 * {@link ReferenceType} tagging sub-interface is implemented by the concrete type enums
 * ({@link Association.AssociationType} and {@link Collection.CollectionType}).
 *
 * @see Association
 * @see Collection
 */
public interface Reference extends Relation {
	/**
	 * Tagging interface for {@link Association.AssociationType} and
	 * {@link Collection.CollectionType} to provide a common type bound for
	 * {@link #getType()}.
	 */
	public static interface ReferenceType {
		// tagging interface
	}

	/**
	 * Returns the name of the default query to use when presenting this reference
	 * in a list/picker widget.
	 *
	 * @return the default query name, or {@code null} if the document's default query applies
	 */
	public String getQueryName();

	/**
	 * Returns the specific reference type that governs the ORM mapping and lifecycle behaviour.
	 *
	 * @return the reference type; never {@code null}
	 */
	public ReferenceType getType();
}
