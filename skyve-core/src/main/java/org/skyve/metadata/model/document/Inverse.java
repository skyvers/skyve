package org.skyve.metadata.model.document;

/**
 * Defines the inverse side of a document relationship.
 *
 * <p>An inverse links the current relation to a named relation in the target
 * document ({@link #getReferenceName()}) and declares inverse cardinality
 * ({@link #getCardinality()}).
 *
 * <p>{@link #getCascade()} controls whether mutations on this side should be
 * propagated to the inverse side by framework persistence logic.
 */
public interface Inverse extends Relation {
	/**
	 * Defines the InverseCardinality enumeration.
	 */
	public static enum InverseCardinality {
		one, many;
	}

	/**
	 * The reference name in the target document that this inverse relates to.
	 *
	 * @return the reference name.
	 */
	public String getReferenceName();

	/**
	 * The cardinality of this (inverse) side of the relationship.
	 * 
	 * @return	one or many.
	 */
	public InverseCardinality getCardinality();

	/**
	 * If true, changes are propagated when the inverse is mutated.
	 * 
	 * @return Boolean.TRUE, or Boolean.FALSE/null.
	 */
	public Boolean getCascade();
}
