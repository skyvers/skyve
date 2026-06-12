package org.skyve.impl.persistence;

import org.locationtech.jts.geom.Geometry;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.persistence.hibernate.dialect.SkyveDialect.RDBMS;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.DocumentFilter;
import org.skyve.persistence.DocumentQuery;
import org.skyve.util.Binder.TargetMetaData;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Mutable filter expression builder for {@link AbstractDocumentQuery}.
 *
 * <p>Appends predicate clauses (equality, range, like, null checks, etc.) to a
 * growing SQL/HQL {@code WHERE} expression. The resulting clause is embedded into
 * the owning query before execution. The {@code RDBMS} hint is used for
 * platform-specific like-escaping.
 *
 * @see org.skyve.persistence.DocumentFilter
 */
public class DocumentFilterImpl implements DocumentFilter {
	private static final String AND_OPERATOR = " AND ";
	private static final String GEOMETRY_TRUE_SUFFIX = ") = true";
	private static final String LIKE_OPERATOR = " like ";
	private static final String LOWER_FUNCTION = "lower(";
	private static final String NOT_LIKE_OPERATOR = " not like ";
	private static final String PARAMETER_PREFIX = "param";

	private AbstractDocumentQuery owningQuery;
	private StringBuilder filterClause = new StringBuilder(128); // resulting filter expression
	private RDBMS rdbms;
	
	/**
	 * Creates an empty mutable filter bound to the supplied owning query.
	 *
	 * @param owningQuery query that receives generated parameter bindings
	 * @param rdbms optional dialect hint for case-insensitive handling
	 */
	DocumentFilterImpl(@Nonnull AbstractDocumentQuery owningQuery, @Nullable RDBMS rdbms) {
		this(owningQuery, rdbms, null);
	}

	/**
	 * Creates a mutable filter with an optional initial clause.
	 *
	 * <p>When an initial clause is supplied, it is wrapped in parentheses so later
	 * predicates preserve operator precedence.
	 *
	 * @param owningQuery query that receives generated parameter bindings
	 * @param rdbms optional dialect hint for case-insensitive handling
	 * @param filterClause optional initial filter clause
	 */
	DocumentFilterImpl(@Nonnull AbstractDocumentQuery owningQuery, @Nullable RDBMS rdbms, @Nullable String filterClause) {
		setQuery(owningQuery);
		this.rdbms = rdbms;
		if (filterClause != null) {
			// NB we include brackets here in case there are lower precedence operators in the clause
			// like ORs etc - the brackets ensure that we respect the intention of the query
			// without worrying about operator precedence.
			// Extra brackets are removed by Hibernate during query parse
			this.filterClause.append('(').append(new AbstractBizQL(filterClause).toQueryString(false)).append(')');
		}
	}

	/**
	 * Rebinds this filter to a query that receives generated parameter bindings.
	 *
	 * @param owningQuery new owning query
	 */
	void setQuery(@Nonnull AbstractDocumentQuery owningQuery) {
		this.owningQuery = owningQuery;
	}

	/**
	 * Applies the addEquals() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addEquals(String binding, Object operand) {
		return addAliasedEquals(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	/**
	 * Applies the addAliasedEquals() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addAliasedEquals(String entityAlias, String binding, Object operand) {
		return appendRestriction(entityAlias, binding, " = ", operand, false, false, null, null, false);
	}

	/**
	 * Applies the addIn() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addIn(String binding, Object...operands) {
		return addIn(DocumentQuery.THIS_ALIAS, binding, false, operands);
	}

	/**
	 * Applies the addAliasedIn() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addAliasedIn(String entityAlias, String binding, Object...operands) {
		return addIn(entityAlias, binding, false, operands);
	}

	/**
	 * Applies the addNotIn() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addNotIn(String binding, Object... operands) {
		return addIn(DocumentQuery.THIS_ALIAS, binding, true, operands);
	}

	/**
	 * Applies the addAliasedNotIn() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addAliasedNotIn(String entityAlias, String binding, Object... operands) {
		return addIn(entityAlias, binding, true, operands);
	}
	
	/**
	 * Appends an {@code in}/{@code not in} predicate and binds all operands.
	 *
	 * <p>For PostgreSQL string operands (excluding document id bindings), both the
	 * binding and parameters are wrapped in {@code lower(...)} to preserve
	 * case-insensitive matching semantics.
	 */
	@SuppressWarnings("java:S3776") // Complexity OK
	private DocumentFilter addIn(@Nonnull String entityAlias, @Nonnull String binding, boolean not, Object... operands) {
		if (! filterClause.isEmpty()) {
			filterClause.append(AND_OPERATOR);
		}

		// lower function is required for postgresql database with 
		// String operands (where the binding isn't "bizid" or ends with ".bizId")
		boolean lower = (RDBMS.postgresql == rdbms);
		if (lower) {
			if (Bean.DOCUMENT_ID.equals(binding) || binding.endsWith(Bean.DOCUMENT_ID_SUFFIX)) {
				lower = false;
			}
			else {
				for (Object operand : operands) {
					if (! (operand instanceof String)) {
						lower = false;
						break;
					}
				}
			}
		}

		if (lower) {
			filterClause.append(LOWER_FUNCTION);
		}
		filterClause.append(entityAlias).append('.').append(binding);
		if (lower) {
			filterClause.append(')');
		}
		if (not) {
			filterClause.append(" not");
		}
		filterClause.append(" in (");
		
		for (Object operand : operands) {
			String parameterName = PARAMETER_PREFIX + owningQuery.parameterNumber++;
			// Its probably wisest (although untested as yet) to use the DB lower function 
			// on parameters to use its char set and collation.
			owningQuery.putParameter(parameterName, operand);
			if (lower) {
				filterClause.append("lower(:").append(parameterName).append("),");
			}
			else {
				filterClause.append(':').append(parameterName).append(',');
			}
		}
		filterClause.setLength(filterClause.length() - 1); // remove last comma
		filterClause.append(')');
		return this;
	}
	
	/**
	 * Applies the addNotEquals() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addNotEquals(String binding, Object operand) {
		return addAliasedNotEquals(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	/**
	 * Applies the addAliasedNotEquals() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addAliasedNotEquals(String entityAlias, String binding, Object operand) {
		return appendRestriction(entityAlias, binding, " != ", operand, false, false, null, null, false);
	}

	/**
	 * Applies the addGreaterThan() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addGreaterThan(String binding, Object operand) {
		return addAliasedGreaterThan(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	/**
	 * Applies the addAliasedGreaterThan() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addAliasedGreaterThan(String entityAlias, String binding, Object operand) {
		return appendRestriction(entityAlias, binding, " > ", operand, false, false, null, null, false);
	}

	/**
	 * Applies the addGreaterThanOrEqualTo() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addGreaterThanOrEqualTo(String binding, Object operand) {
		return addAliasedGreaterThanOrEqualTo(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	/**
	 * Applies the addAliasedGreaterThanOrEqualTo() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addAliasedGreaterThanOrEqualTo(String entityAlias, String binding, Object operand) {
		return appendRestriction(entityAlias, binding, " >= ", operand, false, false, null, null, false);
	}

	/**
	 * Applies the addLessThan() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addLessThan(String binding, Object operand) {
		return addAliasedLessThan(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	/**
	 * Applies the addAliasedLessThan() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addAliasedLessThan(String entityAlias, String binding, Object operand) {
		return appendRestriction(entityAlias, binding, " < ", operand, false, false, null, null, false);
	}

	/**
	 * Applies the addLessThanOrEqualTo() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addLessThanOrEqualTo(String binding, Object operand) {
		return addAliasedLessThanOrEqualTo(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	/**
	 * Applies the addAliasedLessThanOrEqualTo() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addAliasedLessThanOrEqualTo(String entityAlias, String binding, Object operand) {
		return appendRestriction(entityAlias, binding, " <= ", operand, false, false, null, null, false);
	}

	/**
	 * Applies the addLike() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addLike(String binding, String operand) {
		return addAliasedLike(DocumentQuery.THIS_ALIAS, binding, operand);
	}

	/**
	 * Applies the addAliasedLike() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addAliasedLike(String entityAlias, String binding, String operand) {
		return appendRestriction(entityAlias, binding, LIKE_OPERATOR, operand, false, useStr(binding), null, null, false);
	}

	/**
	 * Applies the addNotLike() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addNotLike(String binding, String operand) {
		return addAliasedNotLike(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	/**
	 * Applies the addAliasedNotLike() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addAliasedNotLike(String entityAlias, String binding, String operand) {
		return appendRestriction(entityAlias, binding, NOT_LIKE_OPERATOR, operand, false, useStr(binding), null, null, false);
	}

	/**
	 * Applies the addEquals() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addEquals(String binding, Geometry geometry) {
		return addAliasedEquals(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	/**
	 * Applies the addAliasedEquals() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addAliasedEquals(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, false, false, "equals(", GEOMETRY_TRUE_SUFFIX, false);
	}

	/**
	 * Applies the addDisjoint() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addDisjoint(String binding, Geometry geometry) {
		return addAliasedDisjoint(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	/**
	 * Applies the addAliasedDisjoint() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addAliasedDisjoint(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, false, false, "disjoint(", GEOMETRY_TRUE_SUFFIX, false);
	}
	
	/**
	 * Applies the addIntersects() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addIntersects(String binding, Geometry geometry) {
		return addAliasedIntersects(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	/**
	 * Applies the addAliasedIntersects() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addAliasedIntersects(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, false, false, "intersects(", GEOMETRY_TRUE_SUFFIX, false);
	}

	/**
	 * Applies the addTouches() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addTouches(String binding, Geometry geometry) {
		return addAliasedTouches(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	/**
	 * Applies the addAliasedTouches() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addAliasedTouches(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, false, false, "touches(", GEOMETRY_TRUE_SUFFIX, false);
	}

	/**
	 * Applies the addCrosses() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addCrosses(String binding, Geometry geometry) {
		return addAliasedCrosses(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	/**
	 * Applies the addAliasedCrosses() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addAliasedCrosses(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, false, false, "crosses(", GEOMETRY_TRUE_SUFFIX, false);
	}

	/**
	 * Applies the addWithin() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addWithin(String binding, Geometry geometry) {
		return addAliasedWithin(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	/**
	 * Applies the addAliasedWithin() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addAliasedWithin(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, false, false, "within(", GEOMETRY_TRUE_SUFFIX, false);
	}
	
	/**
	 * Applies the addContains() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addContains(String binding, Geometry geometry) {
		return addAliasedContains(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	/**
	 * Applies the addAliasedContains() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addAliasedContains(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, false, false, "contains(", GEOMETRY_TRUE_SUFFIX, false);
	}
	
	/**
	 * Applies the addOverlaps() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addOverlaps(String binding, Geometry geometry) {
		return addAliasedOverlaps(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	/**
	 * Applies the addAliasedOverlaps() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addAliasedOverlaps(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, false, false, "overlaps(", GEOMETRY_TRUE_SUFFIX, false);
	}
	
	/**
	 * Applies the addNullOrEquals() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addNullOrEquals(String binding, Object operand) {
		return addAliasedNullOrEquals(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	/**
	 * Applies the addAliasedNullOrEquals() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addAliasedNullOrEquals(String entityAlias, String binding, Object operand) {
		return appendRestriction(entityAlias, binding, " = ", operand, true, false, null, null, false);
	}

	/**
	 * Applies the addNullOrNotEquals() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addNullOrNotEquals(String binding, Object operand) {
		return addAliasedNullOrNotEquals(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	/**
	 * Applies the addAliasedNullOrNotEquals() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addAliasedNullOrNotEquals(String entityAlias, String binding, Object operand) {
		return appendRestriction(entityAlias, binding, " != ", operand, true, false, null, null, false);
	}

	/**
	 * Applies the addNullOrGreaterThan() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addNullOrGreaterThan(String binding, Object operand) {
		return addAliasedNullOrGreaterThan(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	/**
	 * Applies the addAliasedNullOrGreaterThan() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addAliasedNullOrGreaterThan(String entityAlias, String binding, Object operand) {
		return appendRestriction(entityAlias, binding, " > ", operand, true, false, null, null, false);
	}

	/**
	 * Applies the addNullOrGreaterThanOrEqualTo() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addNullOrGreaterThanOrEqualTo(String binding, Object operand) {
		return addAliasedNullOrGreaterThanOrEqualTo(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	/**
	 * Applies the addAliasedNullOrGreaterThanOrEqualTo() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addAliasedNullOrGreaterThanOrEqualTo(String entityAlias, String binding, Object operand) {
		return appendRestriction(entityAlias, binding, " >= ", operand, true, false, null, null, false);
	}

	/**
	 * Applies the addNullOrLessThan() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addNullOrLessThan(String binding, Object operand) {
		return addAliasedNullOrLessThan(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	/**
	 * Applies the addAliasedNullOrLessThan() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addAliasedNullOrLessThan(String entityAlias, String binding, Object operand) {
		return appendRestriction(entityAlias, binding, " < ", operand, true, false, null, null, false);
	}

	/**
	 * Applies the addNullOrLessThanOrEqualTo() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addNullOrLessThanOrEqualTo(String binding, Object operand) {
		return addAliasedNullOrLessThanOrEqualTo(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	/**
	 * Applies the addAliasedNullOrLessThanOrEqualTo() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addAliasedNullOrLessThanOrEqualTo(String entityAlias, String binding, Object operand) {
		return appendRestriction(entityAlias, binding, " <= ", operand, true, false, null, null, false);
	}

	/**
	 * Applies the addNullOrLike() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addNullOrLike(String binding, String operand) {
		return addAliasedNullOrLike(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	/**
	 * Applies the addAliasedNullOrLike() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addAliasedNullOrLike(String entityAlias, String binding, String operand) {
		return appendRestriction(entityAlias, binding, LIKE_OPERATOR, operand, true, useStr(binding), null, null, false);
	}

	/**
	 * Applies the addNullOrNotLike() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addNullOrNotLike(String binding, String operand) {
		return addAliasedNullOrNotLike(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	/**
	 * Applies the addAliasedNullOrNotLike() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addAliasedNullOrNotLike(String entityAlias, String binding, String operand) {
		return appendRestriction(entityAlias, binding, NOT_LIKE_OPERATOR, operand, true, useStr(binding), null, null, false);
	}

	/**
	 * Applies the addNullOrEquals() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addNullOrEquals(String binding, Geometry geometry) {
		return addAliasedNullOrEquals(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	/**
	 * Applies the addAliasedNullOrEquals() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addAliasedNullOrEquals(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, true, false, "equals(", GEOMETRY_TRUE_SUFFIX, false);
	}

	/**
	 * Applies the addNullOrDisjoint() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addNullOrDisjoint(String binding, Geometry geometry) {
		return addAliasedNullOrDisjoint(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	/**
	 * Applies the addAliasedNullOrDisjoint() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addAliasedNullOrDisjoint(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, true, false, "disjoint(", GEOMETRY_TRUE_SUFFIX, false);
	}
	
	/**
	 * Applies the addNullOrIntersects() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addNullOrIntersects(String binding, Geometry geometry) {
		return addAliasedNullOrIntersects(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	/**
	 * Applies the addAliasedNullOrIntersects() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addAliasedNullOrIntersects(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, true, false, "intersects(", GEOMETRY_TRUE_SUFFIX, false);
	}

	/**
	 * Applies the addNullOrTouches() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addNullOrTouches(String binding, Geometry geometry) {
		return addAliasedNullOrTouches(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	/**
	 * Applies the addAliasedNullOrTouches() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addAliasedNullOrTouches(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, true, false, "touches(", GEOMETRY_TRUE_SUFFIX, false);
	}

	/**
	 * Applies the addNullOrCrosses() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addNullOrCrosses(String binding, Geometry geometry) {
		return addAliasedNullOrCrosses(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	/**
	 * Applies the addAliasedNullOrCrosses() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addAliasedNullOrCrosses(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, true, false, "crosses(", GEOMETRY_TRUE_SUFFIX, false);
	}

	/**
	 * Applies the addNullOrWithin() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addNullOrWithin(String binding, Geometry geometry) {
		return addAliasedNullOrWithin(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	/**
	 * Applies the addAliasedNullOrWithin() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addAliasedNullOrWithin(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, true, false, "within(", GEOMETRY_TRUE_SUFFIX, false);
	}
	
	/**
	 * Applies the addNullOrContains() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addNullOrContains(String binding, Geometry geometry) {
		return addAliasedNullOrContains(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	/**
	 * Applies the addAliasedNullOrContains() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addAliasedNullOrContains(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, true, false, "contains(", GEOMETRY_TRUE_SUFFIX, false);
	}
	
	/**
	 * Applies the addNullOrOverlaps() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addNullOrOverlaps(String binding, Geometry geometry) {
		return addAliasedNullOrOverlaps(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	/**
	 * Applies the addAliasedNullOrOverlaps() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addAliasedNullOrOverlaps(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, true, false, "overlaps(", GEOMETRY_TRUE_SUFFIX, false);
	}
	
	/**
	 * Applies the addNull() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addNull(String binding) {
		return addAliasedNull(DocumentQuery.THIS_ALIAS, binding);
	}
	
	/**
	 * Applies the addAliasedNull() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addAliasedNull(String entityAlias, String binding) {
		if (! filterClause.isEmpty()) {
			filterClause.append(AND_OPERATOR);
		}
		filterClause.append(entityAlias).append('.').append(binding).append(" IS NULL");
		return this;
	}

	/**
	 * Applies the addNotNull() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addNotNull(String binding) {
		return addAliasedNotNull(DocumentQuery.THIS_ALIAS, binding);
	}
	
	/**
	 * Applies the addAliasedNotNull() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addAliasedNotNull(String entityAlias, String binding) {
		if (! filterClause.isEmpty()) {
			filterClause.append(AND_OPERATOR);
		}
		filterClause.append(entityAlias).append('.').append(binding).append(" IS NOT NULL");
		return this;
	}

	/**
	 * Applies the addBetween() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addBetween(String binding, Object minOperand, Object maxOperand) {
		return addAliasedBetween(DocumentQuery.THIS_ALIAS, binding, minOperand, maxOperand);
	}
	
	/**
	 * Appends a {@code between} predicate and binds both range operands.
	 *
	 * <p>For PostgreSQL string ranges (excluding document id bindings), both sides
	 * are wrapped in {@code lower(...)} for case-insensitive comparison.
	 */
	@Override
	public DocumentFilter addAliasedBetween(String entityAlias, String binding, Object minOperand, Object maxOperand) {
		// Its probably wisest (although untested as yet) to use the DB lower function 
		// on parameters to use its char set and collation.
		String minParameterName = PARAMETER_PREFIX + owningQuery.parameterNumber++;
		String maxParameterName = PARAMETER_PREFIX + owningQuery.parameterNumber++;
		owningQuery.putParameter(minParameterName, minOperand);
		owningQuery.putParameter(maxParameterName, maxOperand);

		if (! filterClause.isEmpty()) {
			filterClause.append(AND_OPERATOR);
		}

		// lower function is required for postgresql database with 
		// String operands (where the binding isn't "bizid" or ends with ".bizId")
		boolean lower = ((RDBMS.postgresql == rdbms) && (minOperand instanceof String) && (maxOperand instanceof String));
		if (lower) {
			if (Bean.DOCUMENT_ID.equals(binding) || binding.endsWith(Bean.DOCUMENT_ID_SUFFIX)) {
				lower = false;
			}
		}
		
		if (lower) {
			filterClause.append(LOWER_FUNCTION).append(entityAlias).append('.').append(binding).append(") BETWEEN ");
			filterClause.append("lower(:").append(minParameterName).append(") and lower(:").append(maxParameterName).append(')');
		}
		else {
			filterClause.append(entityAlias).append('.').append(binding).append(" BETWEEN ");
			filterClause.append(':').append(minParameterName).append(" and :").append(maxParameterName);
		}
		return this;
	}

	/**
	 * Applies the addCollectionSizeEquals() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addCollectionSizeEquals(String binding, int operand) {
		return addAliasedCollectionSizeEquals(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	/**
	 * Applies the addAliasedCollectionSizeEquals() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addAliasedCollectionSizeEquals(String entityAlias, String binding, int operand) {
		return appendRestriction(entityAlias, binding, ".size = ", Integer.valueOf(operand), false, false, null, null, false);
	}

	/**
	 * Applies the addCollectionSizeNotEquals() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addCollectionSizeNotEquals(String binding, int operand) {
		return addAliasedCollectionSizeNotEquals(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	/**
	 * Applies the addAliasedCollectionSizeNotEquals() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addAliasedCollectionSizeNotEquals(String entityAlias, String binding, int operand) {
		return appendRestriction(entityAlias, binding, ".size != ", Integer.valueOf(operand), false, false, null, null, false);
	}

	/**
	 * Applies the addCollectionSizeGreaterThan() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addCollectionSizeGreaterThan(String binding, int operand) {
		return addAliasedCollectionSizeGreaterThan(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	/**
	 * Applies the addAliasedCollectionSizeGreaterThan() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addAliasedCollectionSizeGreaterThan(String entityAlias, String binding, int operand) {
		return appendRestriction(entityAlias, binding, ".size > ", Integer.valueOf(operand), false, false, null, null, false);
	}

	/**
	 * Applies the addCollectionSizeGreaterThanOrEqualTo() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addCollectionSizeGreaterThanOrEqualTo(String binding, int operand) {
		return addAliasedCollectionSizeGreaterThanOrEqualTo(DocumentQuery.THIS_ALIAS, binding, operand);
	}

	/**
	 * Applies the addAliasedCollectionSizeGreaterThanOrEqualTo() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addAliasedCollectionSizeGreaterThanOrEqualTo(String entityAlias, String binding, int operand) {
		return appendRestriction(entityAlias, binding, ".size >= ", Integer.valueOf(operand), false, false, null, null, false);
	}

	/**
	 * Applies the addCollectionSizeLessThan() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addCollectionSizeLessThan(String binding, int operand) {
		return addAliasedCollectionSizeLessThan(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	/**
	 * Applies the addAliasedCollectionSizeLessThan() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addAliasedCollectionSizeLessThan(String entityAlias, String binding, int operand) {
		return appendRestriction(entityAlias, binding, ".size < ", Integer.valueOf(operand), false, false, null, null, false);
	}

	/**
	 * Applies the addCollectionSizeLessThanOrEqualTo() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addCollectionSizeLessThanOrEqualTo(String binding, int operand) {
		return addAliasedCollectionSizeLessThanOrEqualTo(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	/**
	 * Applies the addAliasedCollectionSizeLessThanOrEqualTo() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addAliasedCollectionSizeLessThanOrEqualTo(String entityAlias, String binding, int operand) {
		return appendRestriction(entityAlias, binding, ".size <= ", Integer.valueOf(operand), false, false, null, null, false);
	}
	
	/**
	 * Applies the addMemberOfCollection() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addMemberOfCollection(String binding, Bean operand) {
		return addAliasedMemberOfCollection(DocumentQuery.THIS_ALIAS, binding, operand);
	}

	/**
	 * Applies the addAliasedMemberOfCollection() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addAliasedMemberOfCollection(String entityAlias, String binding, Bean operand) {
		return appendRestriction(entityAlias, binding, " member of ", operand, false, false, null, null, true);
	}

	/**
	 * Applies the addNotMemberOfCollection() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addNotMemberOfCollection(String binding, Bean operand) {
		return addAliasedNotMemberOfCollection(DocumentQuery.THIS_ALIAS, binding, operand);
	}

	/**
	 * Applies the addAliasedNotMemberOfCollection() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addAliasedNotMemberOfCollection(String entityAlias, String binding, Bean operand) {
		return appendRestriction(entityAlias, binding, " not member of ", operand, false, false, null, null, true);
	}

	/**
	 * Appends a single predicate restriction and binds its parameter.
	 *
	 * <p>Supports plain operator predicates, function-style spatial predicates,
	 * optional {@code null}-inclusive grouping, optional string coercion via
	 * {@code str(...)}, and reversed operand ordering.
	 *
	 * @param entityAlias query alias containing the target binding
	 * @param binding bound attribute expression
	 * @param operator infix operator, or {@code null} when using function-style form
	 * @param operand parameter operand value
	 * @param addNullTest whether to wrap with {@code (binding is null or ...)}
	 * @param useStr whether to wrap binding with {@code str(...)}
	 * @param functionPrefix function prefix for function-style predicates
	 * @param functionSuffix function suffix for function-style predicates
	 * @param reversed whether parameter appears on the left-hand side
	 * @return this filter for fluent chaining
	 */
	@SuppressWarnings({"java:S107", "java:S3776"}) // Long parameter list preserves the existing framework/API contract; complexity OK.
	private DocumentFilter appendRestriction(@Nonnull String entityAlias,
												@Nonnull String binding,
												@Nullable String operator,
												@Nonnull Object operand,
												boolean addNullTest,
												boolean useStr,
												@Nullable String functionPrefix,
												@Nullable String functionSuffix,
												boolean reversed) {
		String parameterName = PARAMETER_PREFIX + owningQuery.parameterNumber++;
		owningQuery.putParameter(parameterName, operand);

		if (! filterClause.isEmpty()) {
			filterClause.append(AND_OPERATOR);
		}

		if (addNullTest) {
			filterClause.append('(').append(entityAlias).append('.').append(binding);
			filterClause.append(" IS NULL OR ");
		}

		if (operator == null) {
			filterClause.append(functionPrefix);
			filterClause.append(entityAlias).append('.').append(binding);
			filterClause.append(", :").append(parameterName).append(functionSuffix);
		}
		else {
			// lower function is required for postgresql database with 
			// a String operand (where the binding isn't "bizid" or ends with ".bizId")
			boolean lower = ((RDBMS.postgresql == rdbms) && (operand instanceof String));
			if (lower) {
				if (Bean.DOCUMENT_ID.equals(binding) || binding.endsWith(Bean.DOCUMENT_ID_SUFFIX)) {
					lower = false;
				}
			}

			if (reversed) {
				appendParameter(parameterName, lower);
			}
			else {
				appendBinding(entityAlias, binding, useStr, lower);
			}
			filterClause.append(operator);
			if (reversed) {
				appendBinding(entityAlias, binding, useStr, lower);
			}
			else {
				appendParameter(parameterName, lower);
			}
		}

		if (addNullTest) {
			filterClause.append(')');
		}

		return this;
	}
	
	/**
	 * Appends a binding expression, optionally wrapped for string comparison and
	 * lowercase normalization.
	 *
	 * @param entityAlias query alias containing the binding
	 * @param binding bound attribute expression
	 * @param useStr whether to wrap with {@code str(...)}
	 * @param lower whether to wrap with {@code lower(...)}
	 */
	private void appendBinding(@Nonnull String entityAlias, @Nonnull String binding, boolean useStr, boolean lower) {
		if (lower) {
			filterClause.append(LOWER_FUNCTION);
		}
		if (useStr) {
			filterClause.append("str(").append(entityAlias).append('.').append(binding).append(')');
		}
		else {
			filterClause.append(entityAlias).append('.').append(binding);
		}
		if (lower) {
			filterClause.append(')');
		}
	}

			/**
			 * Appends a named parameter reference, optionally wrapped with
			 * {@code lower(...)} for case-insensitive comparison.
			 *
			 * @param parameterName named parameter identifier without leading colon
			 * @param lower whether to wrap with {@code lower(...)}
			 */
	private void appendParameter(@Nonnull String parameterName, boolean lower) {
		// Its probably wisest (although untested as yet) to use the DB lower function 
		// on parameters to use its char set and collation.
		if (lower) {
			filterClause.append(LOWER_FUNCTION);
		}
		filterClause.append(':').append(parameterName);
		if (lower) {
			filterClause.append(')');
		}
	}
	
	/**
	 * Applies the addAnd() predicate to the current filter clause.
	 *
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addAnd(DocumentFilter filter) {
		if (! filterClause.isEmpty()) {
			filterClause.append(AND_OPERATOR);
		}
		filterClause.append('(').append(filter).append(')');
		return this;
	}

	/**
	 * Appends another filter using {@code OR} while preserving existing precedence.
	 *
	 * @param filter filter to combine with this one
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addOr(DocumentFilter filter) {
		if (! filterClause.isEmpty()) {
			filterClause.insert(0, '(');
			filterClause.append(") OR ");
		}
		filterClause.append('(').append(filter).append(')');
		return this;
	}

	/**
	 * Appends a raw expression directly into the filter clause.
	 *
	 * <p>Callers are responsible for expression correctness and parameter binding.
	 *
	 * @param expression expression to append
	 * @return this filter for fluent chaining
	 */
	@Override
	public DocumentFilter addExpression(String expression) {
		if (! filterClause.isEmpty()) {
			filterClause.append(AND_OPERATOR);
		}
		filterClause.append(expression);
		return this;
	}
	
	/**
	 * Indicates whether this filter currently contains no predicates.
	 */
	@Override
	public boolean isEmpty() {
		return filterClause.isEmpty();
	}

	/**
	 * Returns the SQL/HQL filter clause built so far.
	 */
	@Override
	public String toString() {
		return filterClause.toString();
	}

	/**
	 * Determines whether a binding should be coerced to string for LIKE-style
	 * predicates on PostgreSQL.
	 *
	 * @param binding the binding expression to inspect
	 * @return {@code true} when the binding resolves to a scalar type requiring
	 *         string coercion for consistent comparison
	 */
	private boolean useStr(@Nonnull String binding) {
		try {
			String lastBinding = binding;
			int lastDotIndex = binding.lastIndexOf('.');
			if (lastDotIndex > 0) {
				lastBinding = binding.substring(lastDotIndex + 1);
			}
			if (BindUtil.isImplicit(lastBinding)) {
				if (Bean.ORDINAL_NAME.equals(lastBinding) ||
						PersistentBean.VERSION_NAME.equals(lastBinding)) {
					return true;
				}
			}
			else {
				Customer customer = CORE.getUser().getCustomer();
				Document document = owningQuery.drivingDocument;
				Module module = customer.getModule(document.getOwningModuleName());
				TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, document, binding);
				Attribute attribute = target.getAttribute();
				if (attribute != null) {
					AttributeType type = attribute.getAttributeType();
					return AttributeType.bool.equals(type) ||
							AttributeType.date.equals(type) ||
							AttributeType.dateTime.equals(type) ||
							AttributeType.decimal10.equals(type) ||
							AttributeType.decimal2.equals(type) ||
							AttributeType.decimal5.equals(type) ||
							AttributeType.integer.equals(type) ||
							AttributeType.longInteger.equals(type) ||
							AttributeType.time.equals(type) ||
							AttributeType.timestamp.equals(type);
				}
			}
		}
		catch (@SuppressWarnings("unused") Exception e) {
			// do nothing - it'll return false below
		}
		
		return false;
	}
}
