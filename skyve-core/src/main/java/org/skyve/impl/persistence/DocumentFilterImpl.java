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

public class DocumentFilterImpl implements DocumentFilter {
	private static final String LIKE_OPERATOR = " like ";
	private static final String NOT_LIKE_OPERATOR = " not like ";

	private AbstractDocumentQuery owningQuery;
	private StringBuilder filterClause = new StringBuilder(128); // resulting filter expression
	private RDBMS rdbms;
	
	DocumentFilterImpl(AbstractDocumentQuery owningQuery, RDBMS rdbms) {
		this(owningQuery, rdbms, null);
	}

	DocumentFilterImpl(AbstractDocumentQuery owningQuery, RDBMS rdbms, String filterClause) {
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

	void setQuery(AbstractDocumentQuery owningQuery) {
		this.owningQuery = owningQuery;
	}

	@Override
	public DocumentFilter addEquals(String binding, Object operand) {
		return addAliasedEquals(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	@Override
	public DocumentFilter addAliasedEquals(String entityAlias, String binding, Object operand) {
		return appendRestriction(entityAlias, binding, " = ", operand, false, false, null, null);
	}

	@Override
	public DocumentFilter addIn(String binding, Object...operands) {
		return addIn(DocumentQuery.THIS_ALIAS, binding, false, operands);
	}

	@Override
	public DocumentFilter addAliasedIn(String entityAlias, String binding, Object...operands) {
		return addIn(entityAlias, binding, false, operands);
	}

	@Override
	public DocumentFilter addNotIn(String binding, Object... operands) {
		return addIn(DocumentQuery.THIS_ALIAS, binding, true, operands);
	}

	@Override
	public DocumentFilter addAliasedNotIn(String entityAlias, String binding, Object... operands) {
		return addIn(entityAlias, binding, true, operands);
	}
	
	private DocumentFilter addIn(String entityAlias, String binding, boolean not, Object... operands) {
		if (filterClause.length() > 0) {
			filterClause.append(" AND ");
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
			filterClause.append("lower(");
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
			String parameterName = "param" + owningQuery.parameterNumber++;
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
	
	@Override
	public DocumentFilter addNotEquals(String binding, Object operand) {
		return addAliasedNotEquals(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	@Override
	public DocumentFilter addAliasedNotEquals(String entityAlias, String binding, Object operand) {
		return appendRestriction(entityAlias, binding, " != ", operand, false, false, null, null);
	}

	@Override
	public DocumentFilter addGreaterThan(String binding, Object operand) {
		return addAliasedGreaterThan(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	@Override
	public DocumentFilter addAliasedGreaterThan(String entityAlias, String binding, Object operand) {
		return appendRestriction(entityAlias, binding, " > ", operand, false, false, null, null);
	}

	@Override
	public DocumentFilter addGreaterThanOrEqualTo(String binding, Object operand) {
		return addAliasedGreaterThanOrEqualTo(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	@Override
	public DocumentFilter addAliasedGreaterThanOrEqualTo(String entityAlias, String binding, Object operand) {
		return appendRestriction(entityAlias, binding, " >= ", operand, false, false, null, null);
	}

	@Override
	public DocumentFilter addLessThan(String binding, Object operand) {
		return addAliasedLessThan(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	@Override
	public DocumentFilter addAliasedLessThan(String entityAlias, String binding, Object operand) {
		return appendRestriction(entityAlias, binding, " < ", operand, false, false, null, null);
	}

	@Override
	public DocumentFilter addLessThanOrEqualTo(String binding, Object operand) {
		return addAliasedLessThanOrEqualTo(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	@Override
	public DocumentFilter addAliasedLessThanOrEqualTo(String entityAlias, String binding, Object operand) {
		return appendRestriction(entityAlias, binding, " <= ", operand, false, false, null, null);
	}

	@Override
	public DocumentFilter addLike(String binding, String operand) {
		return addAliasedLike(DocumentQuery.THIS_ALIAS, binding, operand);
	}

	@Override
	public DocumentFilter addAliasedLike(String entityAlias, String binding, String operand) {
		return appendRestriction(entityAlias, binding, LIKE_OPERATOR, operand, false, useStr(binding), null, null);
	}

	@Override
	public DocumentFilter addNotLike(String binding, String operand) {
		return addAliasedNotLike(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	@Override
	public DocumentFilter addAliasedNotLike(String entityAlias, String binding, String operand) {
		return appendRestriction(entityAlias, binding, NOT_LIKE_OPERATOR, operand, false, useStr(binding), null, null);
	}

	@Override
	public DocumentFilter addEquals(String binding, Geometry geometry) {
		return addAliasedEquals(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	@Override
	public DocumentFilter addAliasedEquals(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, false, false, "equals(", ") = true");
	}

	@Override
	public DocumentFilter addDisjoint(String binding, Geometry geometry) {
		return addAliasedDisjoint(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	@Override
	public DocumentFilter addAliasedDisjoint(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, false, false, "disjoint(", ") = true");
	}
	
	@Override
	public DocumentFilter addIntersects(String binding, Geometry geometry) {
		return addAliasedIntersects(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	@Override
	public DocumentFilter addAliasedIntersects(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, false, false, "intersects(", ") = true");
	}

	@Override
	public DocumentFilter addTouches(String binding, Geometry geometry) {
		return addAliasedTouches(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	@Override
	public DocumentFilter addAliasedTouches(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, false, false, "touches(", ") = true");
	}

	@Override
	public DocumentFilter addCrosses(String binding, Geometry geometry) {
		return addAliasedCrosses(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	@Override
	public DocumentFilter addAliasedCrosses(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, false, false, "crosses(", ") = true");
	}

	@Override
	public DocumentFilter addWithin(String binding, Geometry geometry) {
		return addAliasedWithin(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	@Override
	public DocumentFilter addAliasedWithin(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, false, false, "within(", ") = true");
	}
	
	@Override
	public DocumentFilter addContains(String binding, Geometry geometry) {
		return addAliasedContains(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	@Override
	public DocumentFilter addAliasedContains(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, false, false, "contains(", ") = true");
	}
	
	@Override
	public DocumentFilter addOverlaps(String binding, Geometry geometry) {
		return addAliasedOverlaps(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	@Override
	public DocumentFilter addAliasedOverlaps(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, false, false, "overlaps(", ") = true");
	}
	
	@Override
	public DocumentFilter addNullOrEquals(String binding, Object operand) {
		return addAliasedNullOrEquals(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	@Override
	public DocumentFilter addAliasedNullOrEquals(String entityAlias, String binding, Object operand) {
		return appendRestriction(entityAlias, binding, " = ", operand, true, false, null, null);
	}

	@Override
	public DocumentFilter addNullOrNotEquals(String binding, Object operand) {
		return addAliasedNullOrNotEquals(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	@Override
	public DocumentFilter addAliasedNullOrNotEquals(String entityAlias, String binding, Object operand) {
		return appendRestriction(entityAlias, binding, " != ", operand, true, false, null, null);
	}

	@Override
	public DocumentFilter addNullOrGreaterThan(String binding, Object operand) {
		return addAliasedNullOrGreaterThan(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	@Override
	public DocumentFilter addAliasedNullOrGreaterThan(String entityAlias, String binding, Object operand) {
		return appendRestriction(entityAlias, binding, " > ", operand, true, false, null, null);
	}

	@Override
	public DocumentFilter addNullOrGreaterThanOrEqualTo(String binding, Object operand) {
		return addAliasedNullOrGreaterThanOrEqualTo(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	@Override
	public DocumentFilter addAliasedNullOrGreaterThanOrEqualTo(String entityAlias, String binding, Object operand) {
		return appendRestriction(entityAlias, binding, " >= ", operand, true, false, null, null);
	}

	@Override
	public DocumentFilter addNullOrLessThan(String binding, Object operand) {
		return addAliasedNullOrLessThan(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	@Override
	public DocumentFilter addAliasedNullOrLessThan(String entityAlias, String binding, Object operand) {
		return appendRestriction(entityAlias, binding, " < ", operand, true, false, null, null);
	}

	@Override
	public DocumentFilter addNullOrLessThanOrEqualTo(String binding, Object operand) {
		return addAliasedNullOrLessThanOrEqualTo(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	@Override
	public DocumentFilter addAliasedNullOrLessThanOrEqualTo(String entityAlias, String binding, Object operand) {
		return appendRestriction(entityAlias, binding, " <= ", operand, true, false, null, null);
	}

	@Override
	public DocumentFilter addNullOrLike(String binding, String operand) {
		return addAliasedNullOrLike(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	@Override
	public DocumentFilter addAliasedNullOrLike(String entityAlias, String binding, String operand) {
		return appendRestriction(entityAlias, binding, LIKE_OPERATOR, operand, true, useStr(binding), null, null);
	}

	@Override
	public DocumentFilter addNullOrNotLike(String binding, String operand) {
		return addAliasedNullOrNotLike(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	@Override
	public DocumentFilter addAliasedNullOrNotLike(String entityAlias, String binding, String operand) {
		return appendRestriction(entityAlias, binding, NOT_LIKE_OPERATOR, operand, true, useStr(binding), null, null);
	}

	@Override
	public DocumentFilter addNullOrEquals(String binding, Geometry geometry) {
		return addAliasedNullOrEquals(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	@Override
	public DocumentFilter addAliasedNullOrEquals(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, true, false, "equals(", ") = true");
	}

	@Override
	public DocumentFilter addNullOrDisjoint(String binding, Geometry geometry) {
		return addAliasedNullOrDisjoint(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	@Override
	public DocumentFilter addAliasedNullOrDisjoint(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, true, false, "disjoint(", ") = true");
	}
	
	@Override
	public DocumentFilter addNullOrIntersects(String binding, Geometry geometry) {
		return addAliasedNullOrIntersects(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	@Override
	public DocumentFilter addAliasedNullOrIntersects(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, true, false, "intersects(", ") = true");
	}

	@Override
	public DocumentFilter addNullOrTouches(String binding, Geometry geometry) {
		return addAliasedNullOrTouches(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	@Override
	public DocumentFilter addAliasedNullOrTouches(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, true, false, "touches(", ") = true");
	}

	@Override
	public DocumentFilter addNullOrCrosses(String binding, Geometry geometry) {
		return addAliasedNullOrCrosses(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	@Override
	public DocumentFilter addAliasedNullOrCrosses(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, true, false, "crosses(", ") = true");
	}

	@Override
	public DocumentFilter addNullOrWithin(String binding, Geometry geometry) {
		return addAliasedNullOrWithin(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	@Override
	public DocumentFilter addAliasedNullOrWithin(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, true, false, "within(", ") = true");
	}
	
	@Override
	public DocumentFilter addNullOrContains(String binding, Geometry geometry) {
		return addAliasedNullOrContains(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	@Override
	public DocumentFilter addAliasedNullOrContains(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, true, false, "contains(", ") = true");
	}
	
	@Override
	public DocumentFilter addNullOrOverlaps(String binding, Geometry geometry) {
		return addAliasedNullOrOverlaps(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	@Override
	public DocumentFilter addAliasedNullOrOverlaps(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, true, false, "overlaps(", ") = true");
	}
	
	@Override
	public DocumentFilter addNull(String binding) {
		return addAliasedNull(DocumentQuery.THIS_ALIAS, binding);
	}
	
	@Override
	public DocumentFilter addAliasedNull(String entityAlias, String binding) {
		if (filterClause.length() > 0) {
			filterClause.append(" AND ");
		}
		filterClause.append(entityAlias).append('.').append(binding).append(" IS NULL");
		return this;
	}

	@Override
	public DocumentFilter addNotNull(String binding) {
		return addAliasedNotNull(DocumentQuery.THIS_ALIAS, binding);
	}
	
	@Override
	public DocumentFilter addAliasedNotNull(String entityAlias, String binding) {
		if (filterClause.length() > 0) {
			filterClause.append(" AND ");
		}
		filterClause.append(entityAlias).append('.').append(binding).append(" IS NOT NULL");
		return this;
	}

	@Override
	public DocumentFilter addBetween(String binding, Object minOperand, Object maxOperand) {
		return addAliasedBetween(DocumentQuery.THIS_ALIAS, binding, minOperand, maxOperand);
	}
	
	@Override
	public DocumentFilter addAliasedBetween(String entityAlias, String binding, Object minOperand, Object maxOperand) {
		// Its probably wisest (although untested as yet) to use the DB lower function 
		// on parameters to use its char set and collation.
		String minParameterName = "param" + owningQuery.parameterNumber++;
		String maxParameterName = "param" + owningQuery.parameterNumber++;
		owningQuery.putParameter(minParameterName, minOperand);
		owningQuery.putParameter(maxParameterName, maxOperand);

		if (filterClause.length() > 0) {
			filterClause.append(" AND ");
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
			filterClause.append("lower(").append(entityAlias).append('.').append(binding).append(") BETWEEN ");
			filterClause.append("lower(:").append(minParameterName).append(") and lower(:").append(maxParameterName).append(')');
		}
		else {
			filterClause.append(entityAlias).append('.').append(binding).append(" BETWEEN ");
			filterClause.append(':').append(minParameterName).append(" and :").append(maxParameterName);
		}
		return this;
	}

	@Override
	public DocumentFilter addCollectionSizeEquals(String binding, int operand) {
		return addAliasedCollectionSizeEquals(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	@Override
	public DocumentFilter addAliasedCollectionSizeEquals(String entityAlias, String binding, int operand) {
		return appendRestriction(entityAlias, binding, ".size = ", Integer.valueOf(operand), false, false, null, null);
	}

	@Override
	public DocumentFilter addCollectionSizeNotEquals(String binding, int operand) {
		return addAliasedCollectionSizeNotEquals(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	@Override
	public DocumentFilter addAliasedCollectionSizeNotEquals(String entityAlias, String binding, int operand) {
		return appendRestriction(entityAlias, binding, ".size != ", Integer.valueOf(operand), false, false, null, null);
	}

	@Override
	public DocumentFilter addCollectionSizeGreaterThan(String binding, int operand) {
		return addAliasedCollectionSizeGreaterThan(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	@Override
	public DocumentFilter addAliasedCollectionSizeGreaterThan(String entityAlias, String binding, int operand) {
		return appendRestriction(entityAlias, binding, ".size > ", Integer.valueOf(operand), false, false, null, null);
	}

	@Override
	public DocumentFilter addCollectionSizeGreaterThanOrEqualTo(String binding, int operand) {
		return addAliasedCollectionSizeGreaterThanOrEqualTo(DocumentQuery.THIS_ALIAS, binding, operand);
	}

	@Override
	public DocumentFilter addAliasedCollectionSizeGreaterThanOrEqualTo(String entityAlias, String binding, int operand) {
		return appendRestriction(entityAlias, binding, ".size >= ", Integer.valueOf(operand), false, false, null, null);
	}

	@Override
	public DocumentFilter addCollectionSizeLessThan(String binding, int operand) {
		return addAliasedCollectionSizeLessThan(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	@Override
	public DocumentFilter addAliasedCollectionSizeLessThan(String entityAlias, String binding, int operand) {
		return appendRestriction(entityAlias, binding, ".size < ", Integer.valueOf(operand), false, false, null, null);
	}

	@Override
	public DocumentFilter addCollectionSizeLessThanOrEqualTo(String binding, int operand) {
		return addAliasedCollectionSizeLessThanOrEqualTo(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	@Override
	public DocumentFilter addAliasedCollectionSizeLessThanOrEqualTo(String entityAlias, String binding, int operand) {
		return appendRestriction(entityAlias, binding, ".size <= ", Integer.valueOf(operand), false, false, null, null);
	}
	
	private DocumentFilter appendRestriction(String entityAlias,
												String binding,
												String operator,
												Object operand,
												boolean addNullTest,
												boolean useStr,
												String functionPrefix,
												String functionSuffix) {
		String parameterName = "param" + owningQuery.parameterNumber++;
		owningQuery.putParameter(parameterName, operand);

		if (filterClause.length() > 0) {
			filterClause.append(" AND ");
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

			if (lower) {
				filterClause.append("lower(");
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
			filterClause.append(operator);
			// Its probably wisest (although untested as yet) to use the DB lower function 
			// on parameters to use its char set and collation.
			if (lower) {
				filterClause.append("lower(");
			}
			filterClause.append(':').append(parameterName);
			if (lower) {
				filterClause.append(')');
			}
		}

		if (addNullTest) {
			filterClause.append(')');
		}
/*
		boolean isALikeOperator = (LIKE_OPERATOR.equals(operator) || NOT_LIKE_OPERATOR.equals(operator));

		String parameterName = "param" + owningQuery.parameterNumber++;
		if (! isALikeOperator) {
			owningQuery.putParameter(parameterName, operand);
		}
		if (filterClause.length() > 0) {
			filterClause.append(" AND ");
		}
		
		if (addNullTest) {
			filterClause.append('(').append(entityAlias).append('.').append(binding);
			filterClause.append(" IS NULL OR ");
		}
		if (isALikeOperator) {
			filterClause.append(entityAlias).append('.').append(binding);
			filterClause.append(operator).append('\'').append(operand).append('\'');
		}
		else {
			if (operator == null) {
				filterClause.append(functionPrefix);
				filterClause.append(entityAlias).append('.').append(binding);
				filterClause.append(", :").append(parameterName).append(functionSuffix);
			}
			else {
				filterClause.append(entityAlias).append('.').append(binding);
				filterClause.append(operator).append(':').append(parameterName);
			}
		}
		if (addNullTest) {
			filterClause.append(')');
		}
*/
		return this;
	}

	@Override
	public DocumentFilter addAnd(DocumentFilter filter) {
		if (filterClause.length() > 0) {
			filterClause.append(" AND ");
		}
		filterClause.append('(').append(filter).append(')');
		return this;
	}

	@Override
	public DocumentFilter addOr(DocumentFilter filter) {
		if (filterClause.length() > 0) {
			filterClause.insert(0, '(');
			filterClause.append(") OR ");
		}
		filterClause.append('(').append(filter).append(')');
		return this;
	}

	@Override
	public DocumentFilter addExpression(String expression) {
		if (filterClause.length() > 0) {
			filterClause.append(" AND ");
		}
		filterClause.append(expression);
		return this;
	}
	
	@Override
	public boolean isEmpty() {
		return filterClause.isEmpty();
	}

	@Override
	public String toString() {
		return filterClause.toString();
	}

	private boolean useStr(String binding) {
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
				if (target != null) {
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
		}
		catch (@SuppressWarnings("unused") Exception e) {
			// do nothing - it'll return false below
		}
		
		return false;
	}
}
