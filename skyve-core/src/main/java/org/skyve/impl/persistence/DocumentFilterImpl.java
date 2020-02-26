package org.skyve.impl.persistence;

import org.locationtech.jts.geom.Geometry;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.bind.BindUtil;
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

	public DocumentFilterImpl(AbstractDocumentQuery owningQuery) {
		this(owningQuery, null);
	}

	DocumentFilterImpl(AbstractDocumentQuery owningQuery, String filterClause) {
		setQuery(owningQuery);
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
		return addEquals(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	@Override
	public DocumentFilter addEquals(String entityAlias, String binding, Object operand) {
		return appendRestriction(entityAlias, binding, " = ", operand, false, false, null, null);
	}

	@Override
	public DocumentFilter addIn(String binding, Object...operands) {
		return addIn(DocumentQuery.THIS_ALIAS, binding, false, operands);
	}

	@Override
	public DocumentFilter addIn(String entityAlias, String binding, Object...operands) {
		return addIn(entityAlias, binding, false, operands);
	}

	@Override
	public DocumentFilter addNotIn(String binding, Object... operands) {
		return addIn(DocumentQuery.THIS_ALIAS, binding, true, operands);
	}

	@Override
	public DocumentFilter addNotIn(String entityAlias, String binding, Object... operands) {
		return addIn(entityAlias, binding, true, operands);
	}
	
	private DocumentFilter addIn(String entityAlias, String binding, boolean not, Object... operands) {
		if (filterClause.length() > 0) {
			filterClause.append(" AND ");
		}
		filterClause.append(entityAlias).append('.').append(binding);
		if (not) {
			filterClause.append(" not");
		}
		filterClause.append(" in (");
		
		for (Object operand : operands) {
			String parameterName = "param" + owningQuery.parameterNumber++;
			owningQuery.putParameter(parameterName, operand);
			filterClause.append(':').append(parameterName).append(',');
		}
		filterClause.setLength(filterClause.length() - 1); // remove last comma
		filterClause.append(')');
		return this;
	}
	
	@Override
	public DocumentFilter addNotEquals(String binding, Object operand) {
		return addNotEquals(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	@Override
	public DocumentFilter addNotEquals(String entityAlias, String binding, Object operand) {
		return appendRestriction(entityAlias, binding, " != ", operand, false, false, null, null);
	}

	@Override
	public DocumentFilter addGreaterThan(String binding, Object operand) {
		return addGreaterThan(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	@Override
	public DocumentFilter addGreaterThan(String entityAlias, String binding, Object operand) {
		return appendRestriction(entityAlias, binding, " > ", operand, false, false, null, null);
	}

	@Override
	public DocumentFilter addGreaterThanOrEqualTo(String binding, Object operand) {
		return addGreaterThanOrEqualTo(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	@Override
	public DocumentFilter addGreaterThanOrEqualTo(String entityAlias, String binding, Object operand) {
		return appendRestriction(entityAlias, binding, " >= ", operand, false, false, null, null);
	}

	@Override
	public DocumentFilter addLessThan(String binding, Object operand) {
		return addLessThan(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	@Override
	public DocumentFilter addLessThan(String entityAlias, String binding, Object operand) {
		return appendRestriction(entityAlias, binding, " < ", operand, false, false, null, null);
	}

	@Override
	public DocumentFilter addLessThanOrEqualTo(String binding, Object operand) {
		return addLessThanOrEqualTo(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	@Override
	public DocumentFilter addLessThanOrEqualTo(String entityAlias, String binding, Object operand) {
		return appendRestriction(entityAlias, binding, " <= ", operand, false, false, null, null);
	}

	@Override
	public DocumentFilter addLike(String binding, String operand) {
		return addLike(DocumentQuery.THIS_ALIAS, binding, operand);
	}

	@Override
	public DocumentFilter addLike(String entityAlias, String binding, String operand) {
		return appendRestriction(entityAlias, binding, LIKE_OPERATOR, operand, false, useStr(binding), null, null);
	}

	@Override
	public DocumentFilter addNotLike(String binding, String operand) {
		return addNotLike(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	@Override
	public DocumentFilter addNotLike(String entityAlias, String binding, String operand) {
		return appendRestriction(entityAlias, binding, NOT_LIKE_OPERATOR, operand, false, useStr(binding), null, null);
	}

	@Override
	public DocumentFilter addEquals(String binding, Geometry geometry) {
		return addEquals(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	@Override
	public DocumentFilter addEquals(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, false, false, "equals(", ") = true");
	}

	@Override
	public DocumentFilter addDisjoint(String binding, Geometry geometry) {
		return addDisjoint(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	@Override
	public DocumentFilter addDisjoint(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, false, false, "disjoint(", ") = true");
	}
	
	@Override
	public DocumentFilter addIntersects(String binding, Geometry geometry) {
		return addIntersects(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	@Override
	public DocumentFilter addIntersects(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, false, false, "intersects(", ") = true");
	}

	@Override
	public DocumentFilter addTouches(String binding, Geometry geometry) {
		return addTouches(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	@Override
	public DocumentFilter addTouches(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, false, false, "touches(", ") = true");
	}

	@Override
	public DocumentFilter addCrosses(String binding, Geometry geometry) {
		return addCrosses(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	@Override
	public DocumentFilter addCrosses(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, false, false, "crosses(", ") = true");
	}

	@Override
	public DocumentFilter addWithin(String binding, Geometry geometry) {
		return addWithin(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	@Override
	public DocumentFilter addWithin(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, false, false, "within(", ") = true");
	}
	
	@Override
	public DocumentFilter addContains(String binding, Geometry geometry) {
		return addContains(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	@Override
	public DocumentFilter addContains(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, false, false, "contains(", ") = true");
	}
	
	@Override
	public DocumentFilter addOverlaps(String binding, Geometry geometry) {
		return addOverlaps(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	@Override
	public DocumentFilter addOverlaps(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, false, false, "overlaps(", ") = true");
	}
	
	@Override
	public DocumentFilter addNullOrEquals(String binding, Object operand) {
		return addNullOrEquals(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	@Override
	public DocumentFilter addNullOrEquals(String entityAlias, String binding, Object operand) {
		return appendRestriction(entityAlias, binding, " = ", operand, true, false, null, null);
	}

	@Override
	public DocumentFilter addNullOrNotEquals(String binding, Object operand) {
		return addNullOrNotEquals(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	@Override
	public DocumentFilter addNullOrNotEquals(String entityAlias, String binding, Object operand) {
		return appendRestriction(entityAlias, binding, " != ", operand, true, false, null, null);
	}

	@Override
	public DocumentFilter addNullOrGreaterThan(String binding, Object operand) {
		return addNullOrGreaterThan(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	@Override
	public DocumentFilter addNullOrGreaterThan(String entityAlias, String binding, Object operand) {
		return appendRestriction(entityAlias, binding, " > ", operand, true, false, null, null);
	}

	@Override
	public DocumentFilter addNullOrGreaterThanOrEqualTo(String binding, Object operand) {
		return addNullOrGreaterThanOrEqualTo(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	@Override
	public DocumentFilter addNullOrGreaterThanOrEqualTo(String entityAlias, String binding, Object operand) {
		return appendRestriction(entityAlias, binding, " >= ", operand, true, false, null, null);
	}

	@Override
	public DocumentFilter addNullOrLessThan(String binding, Object operand) {
		return addNullOrLessThan(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	@Override
	public DocumentFilter addNullOrLessThan(String entityAlias, String binding, Object operand) {
		return appendRestriction(entityAlias, binding, " < ", operand, true, false, null, null);
	}

	@Override
	public DocumentFilter addNullOrLessThanOrEqualTo(String binding, Object operand) {
		return addNullOrLessThanOrEqualTo(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	@Override
	public DocumentFilter addNullOrLessThanOrEqualTo(String entityAlias, String binding, Object operand) {
		return appendRestriction(entityAlias, binding, " <= ", operand, true, false, null, null);
	}

	@Override
	public DocumentFilter addNullOrLike(String binding, String operand) {
		return addNullOrLike(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	@Override
	public DocumentFilter addNullOrLike(String entityAlias, String binding, String operand) {
		return appendRestriction(entityAlias, binding, LIKE_OPERATOR, operand, true, useStr(binding), null, null);
	}

	@Override
	public DocumentFilter addNullOrNotLike(String binding, String operand) {
		return addNullOrNotLike(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	@Override
	public DocumentFilter addNullOrNotLike(String entityAlias, String binding, String operand) {
		return appendRestriction(entityAlias, binding, NOT_LIKE_OPERATOR, operand, true, useStr(binding), null, null);
	}

	@Override
	public DocumentFilter addNullOrEquals(String binding, Geometry geometry) {
		return addNullOrEquals(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	@Override
	public DocumentFilter addNullOrEquals(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, true, false, "equals(", ") = true");
	}

	@Override
	public DocumentFilter addNullOrDisjoint(String binding, Geometry geometry) {
		return addNullOrDisjoint(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	@Override
	public DocumentFilter addNullOrDisjoint(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, true, false, "disjoint(", ") = true");
	}
	
	@Override
	public DocumentFilter addNullOrIntersects(String binding, Geometry geometry) {
		return addNullOrIntersects(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	@Override
	public DocumentFilter addNullOrIntersects(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, true, false, "intersects(", ") = true");
	}

	@Override
	public DocumentFilter addNullOrTouches(String binding, Geometry geometry) {
		return addNullOrTouches(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	@Override
	public DocumentFilter addNullOrTouches(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, true, false, "touches(", ") = true");
	}

	@Override
	public DocumentFilter addNullOrCrosses(String binding, Geometry geometry) {
		return addNullOrCrosses(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	@Override
	public DocumentFilter addNullOrCrosses(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, true, false, "crosses(", ") = true");
	}

	@Override
	public DocumentFilter addNullOrWithin(String binding, Geometry geometry) {
		return addNullOrWithin(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	@Override
	public DocumentFilter addNullOrWithin(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, true, false, "within(", ") = true");
	}
	
	@Override
	public DocumentFilter addNullOrContains(String binding, Geometry geometry) {
		return addNullOrContains(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	@Override
	public DocumentFilter addNullOrContains(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, true, false, "contains(", ") = true");
	}
	
	@Override
	public DocumentFilter addNullOrOverlaps(String binding, Geometry geometry) {
		return addNullOrOverlaps(DocumentQuery.THIS_ALIAS, binding, geometry);
	}
	
	@Override
	public DocumentFilter addNullOrOverlaps(String entityAlias, String binding, Geometry geometry) {
		return appendRestriction(entityAlias, binding, null, geometry, true, false, "overlaps(", ") = true");
	}
	
	@Override
	public DocumentFilter addNull(String binding) {
		return addNull(DocumentQuery.THIS_ALIAS, binding);
	}
	
	@Override
	public DocumentFilter addNull(String entityAlias, String binding) {
		if (filterClause.length() > 0) {
			filterClause.append(" AND ");
		}
		filterClause.append(entityAlias).append('.').append(binding).append(" IS NULL");
		return this;
	}

	@Override
	public DocumentFilter addNotNull(String binding) {
		return addNotNull(DocumentQuery.THIS_ALIAS, binding);
	}
	
	@Override
	public DocumentFilter addNotNull(String entityAlias, String binding) {
		if (filterClause.length() > 0) {
			filterClause.append(" AND ");
		}
		filterClause.append(entityAlias).append('.').append(binding).append(" IS NOT NULL");
		return this;
	}

	@Override
	public DocumentFilter addBetween(String binding, Object minOperand, Object maxOperand) {
		return addBetween(DocumentQuery.THIS_ALIAS, binding, minOperand, maxOperand);
	}
	
	@Override
	public DocumentFilter addBetween(String entityAlias, String binding, Object minOperand, Object maxOperand) {
		String minParameterName = "param" + owningQuery.parameterNumber++;
		String maxParameterName = "param" + owningQuery.parameterNumber++;
		owningQuery.putParameter(minParameterName, minOperand);
		owningQuery.putParameter(maxParameterName, maxOperand);

		if (filterClause.length() > 0) {
			filterClause.append(" AND ");
		}
		filterClause.append(entityAlias).append('.').append(binding).append(" BETWEEN ");
		filterClause.append(':').append(minParameterName).append(" and :").append(maxParameterName);
		return this;
	}

	@Override
	public DocumentFilter addCollectionSizeEquals(String binding, int operand) {
		return addCollectionSizeEquals(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	@Override
	public DocumentFilter addCollectionSizeEquals(String entityAlias, String binding, int operand) {
		return appendRestriction(entityAlias, binding, ".size = ", new Integer(operand), false, false, null, null);
	}

	@Override
	public DocumentFilter addCollectionSizeNotEquals(String binding, int operand) {
		return addCollectionSizeNotEquals(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	@Override
	public DocumentFilter addCollectionSizeNotEquals(String entityAlias, String binding, int operand) {
		return appendRestriction(entityAlias, binding, ".size != ", new Integer(operand), false, false, null, null);
	}

	@Override
	public DocumentFilter addCollectionSizeGreaterThan(String binding, int operand) {
		return addCollectionSizeGreaterThan(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	@Override
	public DocumentFilter addCollectionSizeGreaterThan(String entityAlias, String binding, int operand) {
		return appendRestriction(entityAlias, binding, ".size > ", new Integer(operand), false, false, null, null);
	}

	@Override
	public DocumentFilter addCollectionSizeGreaterThanOrEqualTo(String binding, int operand) {
		return addCollectionSizeGreaterThanOrEqualTo(DocumentQuery.THIS_ALIAS, binding, operand);
	}

	@Override
	public DocumentFilter addCollectionSizeGreaterThanOrEqualTo(String entityAlias, String binding, int operand) {
		return appendRestriction(entityAlias, binding, ".size >= ", new Integer(operand), false, false, null, null);
	}

	@Override
	public DocumentFilter addCollectionSizeLessThan(String binding, int operand) {
		return addCollectionSizeLessThan(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	@Override
	public DocumentFilter addCollectionSizeLessThan(String entityAlias, String binding, int operand) {
		return appendRestriction(entityAlias, binding, ".size < ", new Integer(operand), false, false, null, null);
	}

	@Override
	public DocumentFilter addCollectionSizeLessThanOrEqualTo(String binding, int operand) {
		return addCollectionSizeLessThanOrEqualTo(DocumentQuery.THIS_ALIAS, binding, operand);
	}
	
	@Override
	public DocumentFilter addCollectionSizeLessThanOrEqualTo(String entityAlias, String binding, int operand) {
		return appendRestriction(entityAlias, binding, ".size <= ", new Integer(operand), false, false, null, null);
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
			if (useStr) {
				filterClause.append("str(").append(entityAlias).append('.').append(binding).append(')');
			}
			else {
				filterClause.append(entityAlias).append('.').append(binding);
			}
			filterClause.append(operator).append(':').append(parameterName);
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
		return (filterClause.length() == 0);
	}

	@Override
	public String toString() {
		return (filterClause.length() > 0) ? filterClause.toString() : null;
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
