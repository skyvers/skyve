package org.skyve.persistence;

import org.locationtech.jts.geom.Geometry;
import org.skyve.domain.Bean;

import jakarta.annotation.Nonnull;

/**
 * Fluent predicate builder for {@link DocumentQuery}.
 *
 * <p>Each {@code add*} method appends a condition that is ANDed into the query's WHERE
 * clause by default. Sub-filters obtained via {@link DocumentQuery#newDocumentFilter()}
 * can be composed using {@link #addAnd(DocumentFilter)} and {@link #addOr(DocumentFilter)}
 * to express more complex boolean logic.
 *
 * <h2>Method naming conventions</h2>
 * <ul>
 *   <li>Methods accepting a {@code binding} parameter operate against the root entity alias
 *       ({@link DocumentQuery#THIS_ALIAS}). The binding is a dot-separated Skyve attribute
 *       path (e.g. {@code "address.suburb"}).
 *   <li>Methods accepting an additional {@code entityAlias} parameter operate against a
 *       specific entity alias introduced by a join on the driving query
 *       (e.g. from {@link DocumentQuery#addInnerJoin(String, String)}).
 *   <li>{@code NullOr} variants evaluate as <em>true</em> when the field value is SQL NULL
 *       as well as when it satisfies the stated condition. Use these when {@code null} means
 *       "not excluded" rather than "unknown".
 * </ul>
 *
 * <h2>Spatial predicates</h2>
 * <p>The geometry overloads ({@link #addDisjoint}, {@link #addIntersects},
 * {@link #addTouches}, {@link #addCrosses}, {@link #addWithin}, {@link #addContains},
 * {@link #addOverlaps}) implement the OGC Simple Features spatial relations and are
 * translated to the appropriate database spatial functions at runtime. The target binding
 * must refer to a geometry-typed attribute.
 *
 * <h2>Collection-size predicates</h2>
 * <p>{@code addCollectionSize*} methods filter on the number of elements in a collection
 * attribute. Useful for finding beans with empty or non-empty child collections without
 * performing a subquery manually.
 *
 * <h2>Fluent chaining</h2>
 * <p>All methods return {@code this} for fluent chaining:
 * <pre>{@code
 * query.getFilter()
 *      .addEquals("status", Status.ACTIVE)
 *      .addGreaterThanOrEqualTo("startDate", today)
 *      .addNotNull("assignedTo");
 * }</pre>
 *
 * @see DocumentQuery#getFilter()
 * @see DocumentQuery#newDocumentFilter()
 */
public interface DocumentFilter {
	/**
	 * Adds an equality filter: the value at {@code binding} must equal {@code operand}.
	 *
	 * @param binding the attribute binding path on the root entity; must not be {@code null}
	 * @param operand the value to compare against; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addEquals(@Nonnull String binding, @Nonnull Object operand);

	/**
	 * Adds an equality filter from the specified entity alias.
	 *
	 * @param entityAlias the JPQL alias of the joined entity; must not be {@code null}
	 * @param binding     the attribute binding path; must not be {@code null}
	 * @param operand     the value to compare against; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addAliasedEquals(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Object operand);

	/**
	 * Adds an IN filter: the value at {@code binding} must be one of the given operands.
	 *
	 * @param binding  the attribute binding path on the root entity; must not be {@code null}
	 * @param operands the candidate values; an empty array matches nothing
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addIn(@Nonnull String binding, Object...operands);
	
	/**
	 * Adds an IN filter from the specified entity alias.
	 *
	 * @param entityAlias the JPQL alias of the joined entity; must not be {@code null}
	 * @param binding     the attribute binding path; must not be {@code null}
	 * @param operands    the candidate values; an empty array matches nothing
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addAliasedIn (@Nonnull String entityAlias, @Nonnull String binding, Object...operands);

	/**
	 * Adds a NOT IN filter: the value at {@code binding} must not be any of the given operands.
	 *
	 * @param binding  the attribute binding path on the root entity; must not be {@code null}
	 * @param operands the excluded values; an empty array matches everything
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addNotIn(@Nonnull String binding, Object... operands);

	/**
	 * Adds a NOT IN filter from the specified entity alias.
	 *
	 * @param entityAlias the JPQL alias of the joined entity; must not be {@code null}
	 * @param binding     the attribute binding path; must not be {@code null}
	 * @param operands    the excluded values; an empty array matches everything
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addAliasedNotIn(@Nonnull String entityAlias, @Nonnull String binding, Object... operands);

	/**
	 * Adds an inequality filter: the value at {@code binding} must not equal {@code operand}.
	 *
	 * @param binding the attribute binding path on the root entity; must not be {@code null}
	 * @param operand the value to exclude; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addNotEquals(@Nonnull String binding, @Nonnull Object operand);
	
	/**
	 * Adds an inequality filter from the specified entity alias.
	 *
	 * @param entityAlias the JPQL alias of the joined entity; must not be {@code null}
	 * @param binding     the attribute binding path; must not be {@code null}
	 * @param operand     the value to exclude; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addAliasedNotEquals(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Object operand);

	/**
	 * Adds a greater-than filter: the value at {@code binding} must be strictly greater than {@code operand}.
	 *
	 * @param binding the attribute binding path on the root entity; must not be {@code null}
	 * @param operand the lower-exclusive bound; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addGreaterThan(@Nonnull String binding, @Nonnull Object operand);
	
	/**
	 * Adds a greater-than filter from the specified entity alias.
	 *
	 * @param entityAlias the JPQL alias of the joined entity; must not be {@code null}
	 * @param binding     the attribute binding path; must not be {@code null}
	 * @param operand     the lower-exclusive bound; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addAliasedGreaterThan(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Object operand);
	
	/**
	 * Adds a greater-than-or-equal-to filter: the value at {@code binding} must be &ge; {@code operand}.
	 *
	 * @param binding the attribute binding path on the root entity; must not be {@code null}
	 * @param operand the lower-inclusive bound; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addGreaterThanOrEqualTo(@Nonnull String binding, @Nonnull Object operand);
	
	/**
	 * Adds a greater-than-or-equal-to filter from the specified entity alias.
	 *
	 * @param entityAlias the JPQL alias of the joined entity; must not be {@code null}
	 * @param binding     the attribute binding path; must not be {@code null}
	 * @param operand     the lower-inclusive bound; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addAliasedGreaterThanOrEqualTo(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Object operand);

	/**
	 * Adds a less-than filter: the value at {@code binding} must be strictly less than {@code operand}.
	 *
	 * @param binding the attribute binding path on the root entity; must not be {@code null}
	 * @param operand the upper-exclusive bound; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addLessThan(@Nonnull String binding, @Nonnull Object operand);
	
	/**
	 * Adds a less-than filter from the specified entity alias.
	 *
	 * @param entityAlias the JPQL alias of the joined entity; must not be {@code null}
	 * @param binding     the attribute binding path; must not be {@code null}
	 * @param operand     the upper-exclusive bound; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addAliasedLessThan(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Object operand);

	/**
	 * Adds a less-than-or-equal-to filter: the value at {@code binding} must be &le; {@code operand}.
	 *
	 * @param binding the attribute binding path on the root entity; must not be {@code null}
	 * @param operand the upper-inclusive bound; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addLessThanOrEqualTo(@Nonnull String binding, @Nonnull Object operand);
	
	/**
	 * Adds a less-than-or-equal-to filter from the specified entity alias.
	 *
	 * @param entityAlias the JPQL alias of the joined entity; must not be {@code null}
	 * @param binding     the attribute binding path; must not be {@code null}
	 * @param operand     the upper-inclusive bound; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addAliasedLessThanOrEqualTo(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Object operand);

	/**
	 * Adds a LIKE filter using SQL LIKE syntax ({@code %} and {@code _} wildcards).
	 *
	 * @param binding the attribute binding path on the root entity; must not be {@code null}
	 * @param operand the LIKE pattern (may include {@code %} and {@code _} wildcards); must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addLike(@Nonnull String binding, @Nonnull String operand);
	
	/**
	 * Adds a LIKE filter from the specified entity alias.
	 *
	 * @param entityAlias the JPQL alias of the joined entity; must not be {@code null}
	 * @param binding     the attribute binding path; must not be {@code null}
	 * @param operand     the LIKE pattern; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addAliasedLike(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull String operand);

	/**
	 * Adds a NOT LIKE filter: the value at {@code binding} must not match the LIKE pattern.
	 *
	 * @param binding the attribute binding path on the root entity; must not be {@code null}
	 * @param operand the LIKE pattern to exclude (may include {@code %} and {@code _}); must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addNotLike(@Nonnull String binding, @Nonnull String operand);
	
	/**
	 * Adds a NOT LIKE filter from the specified entity alias.
	 *
	 * @param entityAlias the JPQL alias of the joined entity; must not be {@code null}
	 * @param binding     the attribute binding path; must not be {@code null}
	 * @param operand     the LIKE pattern to exclude; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addAliasedNotLike(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull String operand);

	/**
	 * Adds a geometric equality filter (OGC ST_Equals): the geometry at {@code binding} must
	 * be topologically equal to {@code geometry}.
	 *
	 * @param binding  the attribute binding path to a geometry attribute; must not be {@code null}
	 * @param geometry the reference geometry; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addEquals(@Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * Adds a geometric equality filter from the specified entity alias.
	 *
	 * @param entityAlias the JPQL alias of the joined entity; must not be {@code null}
	 * @param binding     the attribute binding path to a geometry attribute; must not be {@code null}
	 * @param geometry    the reference geometry; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addAliasedEquals(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * Adds an ST_Disjoint spatial filter: the geometry at {@code binding} must have no points
	 * in common with {@code geometry}.
	 *
	 * @param binding  the geometry attribute binding; must not be {@code null}
	 * @param geometry the reference geometry; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addDisjoint(@Nonnull String binding, @Nonnull Geometry geometry);
	
	/**
	 * Adds an ST_Disjoint filter from the specified entity alias.
	 *
	 * @param entityAlias the JPQL alias of the joined entity; must not be {@code null}
	 * @param binding     the geometry attribute binding; must not be {@code null}
	 * @param geometry    the reference geometry; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addAliasedDisjoint(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * Adds an ST_Intersects spatial filter: the geometry at {@code binding} must intersect
	 * (share at least one point with) {@code geometry}.
	 *
	 * @param binding  the geometry attribute binding; must not be {@code null}
	 * @param geometry the reference geometry; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addIntersects(@Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * Adds an ST_Intersects filter from the specified entity alias.
	 *
	 * @param entityAlias the JPQL alias of the joined entity; must not be {@code null}
	 * @param binding     the geometry attribute binding; must not be {@code null}
	 * @param geometry    the reference geometry; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addAliasedIntersects(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * Adds an ST_Touches spatial filter: the geometry at {@code binding} must touch
	 * (share boundary points but not interior points with) {@code geometry}.
	 *
	 * @param binding  the geometry attribute binding; must not be {@code null}
	 * @param geometry the reference geometry; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addTouches(@Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * Adds an ST_Touches filter from the specified entity alias.
	 *
	 * @param entityAlias the JPQL alias of the joined entity; must not be {@code null}
	 * @param binding     the geometry attribute binding; must not be {@code null}
	 * @param geometry    the reference geometry; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addAliasedTouches(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * Adds an ST_Crosses spatial filter: the geometry at {@code binding} must cross
	 * (interiors share points but neither contains the other) {@code geometry}.
	 *
	 * @param binding  the geometry attribute binding; must not be {@code null}
	 * @param geometry the reference geometry; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addCrosses(@Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * Adds an ST_Crosses filter from the specified entity alias.
	 *
	 * @param entityAlias the JPQL alias of the joined entity; must not be {@code null}
	 * @param binding     the geometry attribute binding; must not be {@code null}
	 * @param geometry    the reference geometry; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addAliasedCrosses(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * Adds an ST_Within spatial filter: the geometry at {@code binding} must be entirely
	 * within {@code geometry}.
	 *
	 * @param binding  the geometry attribute binding; must not be {@code null}
	 * @param geometry the reference geometry; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addWithin(@Nonnull String binding, @Nonnull Geometry geometry);
	
	/**
	 * Adds an ST_Within filter from the specified entity alias.
	 *
	 * @param entityAlias the JPQL alias of the joined entity; must not be {@code null}
	 * @param binding     the geometry attribute binding; must not be {@code null}
	 * @param geometry    the reference geometry; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addAliasedWithin(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * Adds an ST_Contains spatial filter: the geometry at {@code binding} must entirely
	 * contain {@code geometry}.
	 *
	 * @param binding  the geometry attribute binding; must not be {@code null}
	 * @param geometry the geometry that must be contained; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addContains(@Nonnull String binding, @Nonnull Geometry geometry);
	
	/**
	 * Adds an ST_Contains filter from the specified entity alias.
	 *
	 * @param entityAlias the JPQL alias of the joined entity; must not be {@code null}
	 * @param binding     the geometry attribute binding; must not be {@code null}
	 * @param geometry    the geometry that must be contained; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addAliasedContains(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * Adds an ST_Overlaps spatial filter: the geometry at {@code binding} must overlap
	 * (have the same dimension and intersect but neither contain the other) {@code geometry}.
	 *
	 * @param binding  the geometry attribute binding; must not be {@code null}
	 * @param geometry the reference geometry; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addOverlaps(@Nonnull String binding, @Nonnull Geometry geometry);
	
	/**
	 * Adds an ST_Overlaps filter from the specified entity alias.
	 *
	 * @param entityAlias the JPQL alias of the joined entity; must not be {@code null}
	 * @param binding     the geometry attribute binding; must not be {@code null}
	 * @param geometry    the reference geometry; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addAliasedOverlaps(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * Adds a NULL-tolerant equality filter: passes when the value at {@code binding} is SQL NULL
	 * or equals {@code operand}. Useful when {@code null} means "not set" rather than "unknown".
	 *
	 * @param binding the attribute binding path on the root entity; must not be {@code null}
	 * @param operand the value to compare against; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addNullOrEquals(@Nonnull String binding, @Nonnull Object operand);
	
	/**
	 * Adds a NULL-tolerant equality filter from the specified entity alias.
	 *
	 * @param entityAlias the JPQL alias of the joined entity; must not be {@code null}
	 * @param binding     the attribute binding path; must not be {@code null}
	 * @param operand     the value to compare against; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addAliasedNullOrEquals(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Object operand);

	/**
	 * Adds a NULL-tolerant inequality filter: passes when the value at {@code binding} is SQL NULL
	 * or is not equal to {@code operand}.
	 *
	 * @param binding the attribute binding path on the root entity; must not be {@code null}
	 * @param operand the value to exclude; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addNullOrNotEquals(@Nonnull String binding, @Nonnull Object operand);
	
	/**
	 * Adds a NULL-tolerant inequality filter from the specified entity alias.
	 *
	 * @param entityAlias the JPQL alias of the joined entity; must not be {@code null}
	 * @param binding     the attribute binding path; must not be {@code null}
	 * @param operand     the value to exclude; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addAliasedNullOrNotEquals(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Object operand);

	/**
	 * Adds a NULL-tolerant greater-than filter: passes when the value is NULL or &gt; {@code operand}.
	 *
	 * @param binding the attribute binding path on the root entity; must not be {@code null}
	 * @param operand the lower-exclusive bound; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addNullOrGreaterThan(@Nonnull String binding, @Nonnull Object operand);
	
	/**
	 * Adds a NULL-tolerant greater-than filter from the specified entity alias.
	 *
	 * @param entityAlias the JPQL alias of the joined entity; must not be {@code null}
	 * @param binding     the attribute binding path; must not be {@code null}
	 * @param operand     the lower-exclusive bound; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addAliasedNullOrGreaterThan(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Object operand);

	/**
	 * Adds a NULL-tolerant greater-than-or-equal-to filter: passes when the value is NULL or &ge; {@code operand}.
	 *
	 * @param binding the attribute binding path on the root entity; must not be {@code null}
	 * @param operand the lower-inclusive bound; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addNullOrGreaterThanOrEqualTo(@Nonnull String binding, @Nonnull Object operand);

	/**
	 * Adds a NULL-tolerant &ge; filter from the specified entity alias.
	 *
	 * @param entityAlias the JPQL alias of the joined entity; must not be {@code null}
	 * @param binding     the attribute binding path; must not be {@code null}
	 * @param operand     the lower-inclusive bound; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addAliasedNullOrGreaterThanOrEqualTo(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Object operand);

	/**
	 * Adds a NULL-tolerant less-than filter: passes when the value is NULL or &lt; {@code operand}.
	 *
	 * @param binding the attribute binding path on the root entity; must not be {@code null}
	 * @param operand the upper-exclusive bound; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addNullOrLessThan(@Nonnull String binding, @Nonnull Object operand);
	
	/**
	 * Adds a NULL-tolerant less-than filter from the specified entity alias.
	 *
	 * @param entityAlias the JPQL alias of the joined entity; must not be {@code null}
	 * @param binding     the attribute binding path; must not be {@code null}
	 * @param operand     the upper-exclusive bound; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addAliasedNullOrLessThan(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Object operand);

	/**
	 * Adds a NULL-tolerant less-than-or-equal-to filter: passes when the value is NULL or &le; {@code operand}.
	 *
	 * @param binding the attribute binding path on the root entity; must not be {@code null}
	 * @param operand the upper-inclusive bound; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addNullOrLessThanOrEqualTo(@Nonnull String binding, @Nonnull Object operand);
	
	/**
	 * Adds a NULL-tolerant &le; filter from the specified entity alias.
	 *
	 * @param entityAlias the JPQL alias of the joined entity; must not be {@code null}
	 * @param binding     the attribute binding path; must not be {@code null}
	 * @param operand     the upper-inclusive bound; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addAliasedNullOrLessThanOrEqualTo(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Object operand);

	/**
	 * Adds a NULL-tolerant LIKE filter: passes when the value is NULL or matches the LIKE pattern.
	 *
	 * @param binding the attribute binding path on the root entity; must not be {@code null}
	 * @param operand the LIKE pattern; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addNullOrLike(@Nonnull String binding, @Nonnull String operand);
	
	/**
	 * Adds a NULL-tolerant LIKE filter from the specified entity alias.
	 *
	 * @param entityAlias the JPQL alias of the joined entity; must not be {@code null}
	 * @param binding     the attribute binding path; must not be {@code null}
	 * @param operand     the LIKE pattern; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addAliasedNullOrLike(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull String operand);

	/**
	 * Adds a NULL-tolerant NOT LIKE filter: passes when the value is NULL or does not match the LIKE pattern.
	 *
	 * @param binding the attribute binding path on the root entity; must not be {@code null}
	 * @param operand the LIKE pattern to exclude; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addNullOrNotLike(@Nonnull String binding, @Nonnull String operand);
	
	/**
	 * Adds a NULL-tolerant NOT LIKE filter from the specified entity alias.
	 *
	 * @param entityAlias the JPQL alias of the joined entity; must not be {@code null}
	 * @param binding     the attribute binding path; must not be {@code null}
	 * @param operand     the LIKE pattern to exclude; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addAliasedNullOrNotLike(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull String operand);

	/**
	 * Adds a NULL-tolerant geometric equality filter (ST_Equals): passes when the geometry is NULL
	 * or is topologically equal to {@code geometry}.
	 *
	 * @param binding  the geometry attribute binding; must not be {@code null}
	 * @param geometry the reference geometry; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addNullOrEquals(@Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * Adds a NULL-tolerant ST_Equals filter from the specified entity alias.
	 *
	 * @param entityAlias the JPQL alias of the joined entity; must not be {@code null}
	 * @param binding     the geometry attribute binding; must not be {@code null}
	 * @param geometry    the reference geometry; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addAliasedNullOrEquals(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * Adds a NULL-tolerant ST_Disjoint filter: passes when the geometry is NULL or disjoint.
	 *
	 * @param binding  the geometry attribute binding; must not be {@code null}
	 * @param geometry the reference geometry; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addNullOrDisjoint(@Nonnull String binding, @Nonnull Geometry geometry);
	
	/**
	 * Adds a NULL-tolerant ST_Disjoint filter from the specified entity alias.
	 *
	 * @param entityAlias the JPQL alias of the joined entity; must not be {@code null}
	 * @param binding     the geometry attribute binding; must not be {@code null}
	 * @param geometry    the reference geometry; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addAliasedNullOrDisjoint(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * Adds a NULL-tolerant ST_Intersects filter: passes when the geometry is NULL or intersects.
	 *
	 * @param binding  the geometry attribute binding; must not be {@code null}
	 * @param geometry the reference geometry; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addNullOrIntersects(@Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * Adds a NULL-tolerant ST_Intersects filter from the specified entity alias.
	 *
	 * @param entityAlias the JPQL alias of the joined entity; must not be {@code null}
	 * @param binding     the geometry attribute binding; must not be {@code null}
	 * @param geometry    the reference geometry; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addAliasedNullOrIntersects(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * Adds a NULL-tolerant ST_Touches filter: passes when the geometry is NULL or touches.
	 *
	 * @param binding  the geometry attribute binding; must not be {@code null}
	 * @param geometry the reference geometry; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addNullOrTouches(@Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * Adds a NULL-tolerant ST_Touches filter from the specified entity alias.
	 *
	 * @param entityAlias the JPQL alias of the joined entity; must not be {@code null}
	 * @param binding     the geometry attribute binding; must not be {@code null}
	 * @param geometry    the reference geometry; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addAliasedNullOrTouches(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * Adds a NULL-tolerant ST_Crosses filter: passes when the geometry is NULL or crosses.
	 *
	 * @param binding  the geometry attribute binding; must not be {@code null}
	 * @param geometry the reference geometry; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addNullOrCrosses(@Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * Adds a NULL-tolerant ST_Crosses filter from the specified entity alias.
	 *
	 * @param entityAlias the JPQL alias of the joined entity; must not be {@code null}
	 * @param binding     the geometry attribute binding; must not be {@code null}
	 * @param geometry    the reference geometry; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addAliasedNullOrCrosses(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * Adds a NULL-tolerant ST_Within filter: passes when the geometry is NULL or is within
	 * {@code geometry}.
	 *
	 * @param binding  the geometry attribute binding; must not be {@code null}
	 * @param geometry the reference geometry; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addNullOrWithin(@Nonnull String binding, @Nonnull Geometry geometry);
	
	/**
	 * Adds a NULL-tolerant ST_Within filter from the specified entity alias.
	 *
	 * @param entityAlias the JPQL alias of the joined entity; must not be {@code null}
	 * @param binding     the geometry attribute binding; must not be {@code null}
	 * @param geometry    the reference geometry; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addAliasedNullOrWithin(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * Adds a NULL-tolerant ST_Contains filter: passes when the geometry is NULL or contains
	 * {@code geometry}.
	 *
	 * @param binding  the geometry attribute binding; must not be {@code null}
	 * @param geometry the geometry to contain; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addNullOrContains(@Nonnull String binding, @Nonnull Geometry geometry);
	
	/**
	 * Adds a NULL-tolerant ST_Contains filter from the specified entity alias.
	 *
	 * @param entityAlias the JPQL alias of the joined entity; must not be {@code null}
	 * @param binding     the geometry attribute binding; must not be {@code null}
	 * @param geometry    the geometry to contain; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addAliasedNullOrContains(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * Adds a NULL-tolerant ST_Overlaps filter: passes when the geometry is NULL or overlaps
	 * {@code geometry}.
	 *
	 * @param binding  the geometry attribute binding; must not be {@code null}
	 * @param geometry the reference geometry; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addNullOrOverlaps(@Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * Adds a NULL-tolerant ST_Overlaps filter from the specified entity alias.
	 *
	 * @param entityAlias the JPQL alias of the joined entity; must not be {@code null}
	 * @param binding     the geometry attribute binding; must not be {@code null}
	 * @param geometry    the reference geometry; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addAliasedNullOrOverlaps(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * Adds an IS NULL filter: the value at {@code binding} must be SQL NULL.
	 *
	 * @param binding the attribute binding path on the root entity; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addNull(@Nonnull String binding);
	
	/**
	 * Adds an IS NULL filter from the specified entity alias.
	 *
	 * @param entityAlias the JPQL alias of the joined entity; must not be {@code null}
	 * @param binding     the attribute binding path; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addAliasedNull(@Nonnull String entityAlias, @Nonnull String binding);

	/**
	 * Adds an IS NOT NULL filter: the value at {@code binding} must not be SQL NULL.
	 *
	 * @param binding the attribute binding path on the root entity; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addNotNull(@Nonnull String binding);
	
	/**
	 * Adds an IS NOT NULL filter from the specified entity alias.
	 *
	 * @param entityAlias the JPQL alias of the joined entity; must not be {@code null}
	 * @param binding     the attribute binding path; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addAliasedNotNull(@Nonnull String entityAlias, @Nonnull String binding);

	/**
	 * Adds a BETWEEN filter: the value at {@code binding} must be &ge; {@code minOperand}
	 * and &le; {@code maxOperand}.
	 *
	 * @param binding    the attribute binding path on the root entity; must not be {@code null}
	 * @param minOperand the lower inclusive bound; must not be {@code null}
	 * @param maxOperand the upper inclusive bound; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addBetween(@Nonnull String binding, @Nonnull Object minOperand, @Nonnull Object maxOperand);
	
	/**
	 * Adds a BETWEEN filter from the specified entity alias.
	 *
	 * @param entityAlias the JPQL alias of the joined entity; must not be {@code null}
	 * @param binding     the attribute binding path; must not be {@code null}
	 * @param minOperand  the lower inclusive bound; must not be {@code null}
	 * @param maxOperand  the upper inclusive bound; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addAliasedBetween(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Object minOperand, @Nonnull Object maxOperand);

	/**
	 * Adds a collection-size equality filter: the number of elements in the collection at
	 * {@code binding} must equal {@code operand}.
	 *
	 * @param binding the collection attribute binding path on the root entity; must not be {@code null}
	 * @param operand the expected element count
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addCollectionSizeEquals(@Nonnull String binding, int operand);
	
	/**
	 * Adds a collection-size equality filter from the specified entity alias.
	 *
	 * @param entityAlias the JPQL alias of the joined entity; must not be {@code null}
	 * @param binding     the collection attribute binding path; must not be {@code null}
	 * @param operand     the expected element count
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addAliasedCollectionSizeEquals(@Nonnull String entityAlias, @Nonnull String binding, int operand);

	/**
	 * Adds a collection-size inequality filter: the number of elements must not equal {@code operand}.
	 *
	 * @param binding the collection attribute binding path; must not be {@code null}
	 * @param operand the excluded element count
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addCollectionSizeNotEquals(@Nonnull String binding, int operand);
	
	/**
	 * Adds a collection-size inequality filter from the specified entity alias.
	 *
	 * @param entityAlias the JPQL alias of the joined entity; must not be {@code null}
	 * @param binding     the collection attribute binding path; must not be {@code null}
	 * @param operand     the excluded element count
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addAliasedCollectionSizeNotEquals(@Nonnull String entityAlias, @Nonnull String binding, int operand);

	/**
	 * Adds a collection-size greater-than filter: the number of elements must be &gt; {@code operand}.
	 *
	 * @param binding the collection attribute binding path; must not be {@code null}
	 * @param operand the lower-exclusive bound
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addCollectionSizeGreaterThan(@Nonnull String binding, int operand);
	
	/**
	 * Adds a collection-size &gt; filter from the specified entity alias.
	 *
	 * @param entityAlias the JPQL alias of the joined entity; must not be {@code null}
	 * @param binding     the collection attribute binding path; must not be {@code null}
	 * @param operand     the lower-exclusive bound
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addAliasedCollectionSizeGreaterThan(@Nonnull String entityAlias, @Nonnull String binding, int operand);

	/**
	 * Adds a collection-size &ge; filter: the number of elements must be &ge; {@code operand}.
	 *
	 * @param binding the collection attribute binding path; must not be {@code null}
	 * @param operand the lower-inclusive bound
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addCollectionSizeGreaterThanOrEqualTo(@Nonnull String binding, int operand);
	
	/**
	 * Adds a collection-size &ge; filter from the specified entity alias.
	 *
	 * @param entityAlias the JPQL alias of the joined entity; must not be {@code null}
	 * @param binding     the collection attribute binding path; must not be {@code null}
	 * @param operand     the lower-inclusive bound
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addAliasedCollectionSizeGreaterThanOrEqualTo(@Nonnull String entityAlias, @Nonnull String binding, int operand);

	/**
	 * Adds a collection-size &lt; filter: the number of elements must be &lt; {@code operand}.
	 *
	 * @param binding the collection attribute binding path; must not be {@code null}
	 * @param operand the upper-exclusive bound
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addCollectionSizeLessThan(@Nonnull String binding, int operand);
	
	/**
	 * Adds a collection-size &lt; filter from the specified entity alias.
	 *
	 * @param entityAlias the JPQL alias of the joined entity; must not be {@code null}
	 * @param binding     the collection attribute binding path; must not be {@code null}
	 * @param operand     the upper-exclusive bound
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addAliasedCollectionSizeLessThan(@Nonnull String entityAlias, @Nonnull String binding, int operand);

	/**
	 * Adds a collection-size &le; filter: the number of elements must be &le; {@code operand}.
	 *
	 * @param binding the collection attribute binding path; must not be {@code null}
	 * @param operand the upper-inclusive bound
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addCollectionSizeLessThanOrEqualTo(@Nonnull String binding, int operand);

	/**
	 * Adds a collection-size &le; filter from the specified entity alias.
	 *
	 * @param entityAlias the JPQL alias of the joined entity; must not be {@code null}
	 * @param binding     the collection attribute binding path; must not be {@code null}
	 * @param operand     the upper-inclusive bound
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addAliasedCollectionSizeLessThanOrEqualTo(@Nonnull String entityAlias, @Nonnull String binding, int operand);

	/**
	 * Adds a collection-membership filter: the given {@code operand} bean must be a member
	 * of the collection at {@code binding}.
	 *
	 * @param binding the collection attribute binding path; must not be {@code null}
	 * @param operand the bean that must be present in the collection; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addMemberOfCollection(@Nonnull String binding, @Nonnull Bean operand);
	
	/**
	 * Adds a NOT MEMBER OF collection filter: the given {@code operand} bean must not be a
	 * member of the collection at {@code binding}.
	 *
	 * @param binding the collection attribute binding path; must not be {@code null}
	 * @param operand the bean that must not be present in the collection; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addNotMemberOfCollection(@Nonnull String binding, @Nonnull Bean operand);
	
	/**
	 * Adds a collection-membership filter from the specified entity alias.
	 *
	 * @param entityAlias the JPQL alias of the joined entity; must not be {@code null}
	 * @param binding     the collection attribute binding path; must not be {@code null}
	 * @param operand     the bean that must be present; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addAliasedMemberOfCollection(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Bean operand);
	
	/**
	 * Adds a NOT MEMBER OF collection filter from the specified entity alias.
	 *
	 * @param entityAlias the JPQL alias of the joined entity; must not be {@code null}
	 * @param binding     the collection attribute binding path; must not be {@code null}
	 * @param operand     the bean that must not be present; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addAliasedNotMemberOfCollection(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Bean operand);
	
	/**
	 * Appends the conditions of {@code filter} combined with AND.
	 *
	 * <p>Use this together with {@link DocumentQuery#newDocumentFilter()} to build
	 * complex boolean expressions:
	 * <pre>{@code
	 * DocumentFilter orFilter = query.newDocumentFilter();
	 * orFilter.addEquals("status", Status.PENDING);
	 * orFilter.addEquals("status", Status.ACTIVE);
	 * query.getFilter().addOr(orFilter);
	 * }</pre>
	 *
	 * @param filter the sub-filter to AND in; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addAnd(@Nonnull DocumentFilter filter);
	
	/**
	 * Appends the conditions of {@code filter} combined with OR.
	 *
	 * @param filter the sub-filter to OR in; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addOr(@Nonnull DocumentFilter filter);
	
	/**
	 * Appends a raw JPQL expression to the filter.
	 *
	 * <p>Use as an escape hatch for predicates not expressible through the typed methods,
	 * such as subqueries or vendor-specific functions.
	 *
	 * @param expression a valid JPQL predicate expression; must not be {@code null}
	 * @return this filter for fluent chaining
	 */
	@Nonnull DocumentFilter addExpression(@Nonnull String expression);
	
	/**
	 * Returns whether this filter has no conditions.
	 *
	 * @return {@code true} if no conditions have been added; {@code false} if at least one exists
	 */
	boolean isEmpty();
}

