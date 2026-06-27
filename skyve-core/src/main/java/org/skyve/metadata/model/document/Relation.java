package org.skyve.metadata.model.document;

import org.skyve.metadata.model.Attribute;

/**
 * Base contract for all relational attributes (associations, collections, and inverses).
 *
 * <p>All relations target another named document within the application. Non-scalar
 * ({@link #isScalar()} returns {@code false}) by definition.
 *
 * @see Association
 * @see Collection
 * @see Inverse
 */
public interface Relation extends Attribute {
	/**
	 * Returns the name of the document this relation targets.
	 *
	 * @return the target document name; never {@code null}
	 */
	public String getDocumentName();
	
	@Override
	default boolean isScalar() {
		return false;
	}
}
