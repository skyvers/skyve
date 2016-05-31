package org.skyve.metadata.model.document;

/**
 * Represents the inverse relationship for an association or collection.
 * An inverse can have a cardinality or one or many (on the inverse side).
 * Inverse can be three types
 * 1) oneToOne - InverseOne -> association
 * 2) oneToMany - InverseOne -> collection
 * 3) manyToMany - InverseMany -> collection
 */
public interface Inverse extends Relation {
	public static enum InverseCardinality {
		one, many;
	}

	/**
	 * The reference name in the target document that this inverse relates to.
	 * 
	 * @return	The reference name for this inverse relationship.
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
