package org.skyve.impl.snapshot;

/**
 * Defines logical operators used to compose {@link SnapshotFilter} groups.
 */
@SuppressWarnings("java:S115") // Suppress "Constant names should comply with a naming convention" as these are not constants but enum values
public enum CompoundFilterOperator {
	/**
	 * Requires all child filters to evaluate to true.
	 */
	and,
	/**
	 * Requires at least one child filter to evaluate to true.
	 */
	or,
	/**
	 * Negates the result of the child filter group.
	 */
	not
}
