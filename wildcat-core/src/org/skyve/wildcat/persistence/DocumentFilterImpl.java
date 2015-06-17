package org.skyve.wildcat.persistence;

import com.vividsolutions.jts.geom.Geometry;

public class DocumentFilterImpl implements Cloneable, org.skyve.persistence.DocumentFilter {
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
			this.filterClause.append(new AbstractBizQL(filterClause).toQueryString(false));
		}
	}

	void setQuery(AbstractDocumentQuery owningQuery) {
		this.owningQuery = owningQuery;
	}

	@Override
	public void addEquals(String binding, Object operand) {
		appendRestriction(binding, " = ", operand, false, null, null);
	}

	@Override
	public void addIn(String binding, Object...operands) {
		addIn(binding, false, operands);
	}
	
	@Override
	public void addNotIn(String binding, Object... operands) {
		addIn(binding, true, operands);
	}
	
	private void addIn(String binding, boolean not, Object... operands) {
		if (filterClause.length() > 0) {
			filterClause.append(" AND ");
		}
		filterClause.append(org.skyve.persistence.DocumentQuery.THIS_ALIAS).append('.').append(binding);
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
	}
	
	@Override
	public void addNotEquals(String binding, Object operand) {
		appendRestriction(binding, " != ", operand, false, null, null);
	}

	@Override
	public void addGreaterThan(String binding, Object operand) {
		appendRestriction(binding, " > ", operand, false, null, null);
	}

	@Override
	public void addGreaterThanOrEqualTo(String binding, Object operand) {
		appendRestriction(binding, " >= ", operand, false, null, null);
	}

	@Override
	public void addLessThan(String binding, Object operand) {
		appendRestriction(binding, " < ", operand, false, null, null);
	}

	@Override
	public void addLessThanOrEqualTo(String binding, Object operand) {
		appendRestriction(binding, " <= ", operand, false, null, null);
	}

	@Override
	public void addLike(String binding, String operand) {
		appendRestriction(binding, LIKE_OPERATOR, operand, false, null, null);
	}

	@Override
	public void addNotLike(String binding, String operand) {
		appendRestriction(binding, NOT_LIKE_OPERATOR, operand, false, null, null);
	}

	@Override
	public void addEquals(String binding, Geometry geometry) {
		appendRestriction(binding, null, geometry, false, "equals(", ") = true");
	}

	@Override
	public void addDisjoint(String binding, Geometry geometry) {
		appendRestriction(binding, null, geometry, false, "disjoint(", ") = true");
	}
	
	@Override
	public void addIntersects(String binding, Geometry geometry) {
		appendRestriction(binding, null, geometry, false, "intersects(", ") = true");
	}

	@Override
	public void addTouches(String binding, Geometry geometry) {
		appendRestriction(binding, null, geometry, false, "touches(", ") = true");
	}

	@Override
	public void addCrosses(String binding, Geometry geometry) {
		appendRestriction(binding, null, geometry, false, "crosses(", ") = true");
	}

	@Override
	public void addWithin(String binding, Geometry geometry) {
		appendRestriction(binding, null, geometry, false, "within(", ") = true");
	}
	
	@Override
	public void addContains(String binding, Geometry geometry) {
		appendRestriction(binding, null, geometry, false, "contains(", ") = true");
	}
	
	@Override
	public void addOverlaps(String binding, Geometry geometry) {
		appendRestriction(binding, null, geometry, false, "overlaps(", ") = true");
	}
	
	@Override
	public void addNullOrEquals(String binding, Object operand) {
		appendRestriction(binding, " = ", operand, true, null, null);
	}

	@Override
	public void addNullOrNotEquals(String binding, Object operand) {
		appendRestriction(binding, " != ", operand, true, null, null);
	}

	@Override
	public void addNullOrGreaterThan(String binding, Object operand) {
		appendRestriction(binding, " > ", operand, true, null, null);
	}

	@Override
	public void addNullOrGreaterThanOrEqualTo(String binding, Object operand) {
		appendRestriction(binding, " >= ", operand, true, null, null);
	}

	@Override
	public void addNullOrLessThan(String binding, Object operand) {
		appendRestriction(binding, " < ", operand, true, null, null);
	}

	@Override
	public void addNullOrLessThanOrEqualTo(String binding, Object operand) {
		appendRestriction(binding, " <= ", operand, true, null, null);
	}

	@Override
	public void addNullOrLike(String binding, String operand) {
		appendRestriction(binding, LIKE_OPERATOR, operand, true, null, null);
	}

	@Override
	public void addNullOrNotLike(String binding, String operand) {
		appendRestriction(binding, NOT_LIKE_OPERATOR, operand, true, null, null);
	}

	@Override
	public void addNullOrEquals(String binding, Geometry geometry) {
		appendRestriction(binding, null, geometry, true, "equals(", ") = true");
	}

	@Override
	public void addNullOrDisjoint(String binding, Geometry geometry) {
		appendRestriction(binding, null, geometry, true, "disjoint(", ") = true");
	}
	
	@Override
	public void addNullOrIntersects(String binding, Geometry geometry) {
		appendRestriction(binding, null, geometry, true, "intersects(", ") = true");
	}

	@Override
	public void addNullOrTouches(String binding, Geometry geometry) {
		appendRestriction(binding, null, geometry, true, "touches(", ") = true");
	}

	@Override
	public void addNullOrCrosses(String binding, Geometry geometry) {
		appendRestriction(binding, null, geometry, true, "crosses(", ") = true");
	}

	@Override
	public void addNullOrWithin(String binding, Geometry geometry) {
		appendRestriction(binding, null, geometry, true, "within(", ") = true");
	}
	
	@Override
	public void addNullOrContains(String binding, Geometry geometry) {
		appendRestriction(binding, null, geometry, true, "contains(", ") = true");
	}
	
	@Override
	public void addNullOrOverlaps(String binding, Geometry geometry) {
		appendRestriction(binding, null, geometry, true, "overlaps(", ") = true");
	}
	
	@Override
	public void addNull(String binding) {
		if (filterClause.length() > 0) {
			filterClause.append(" AND ");
		}
		filterClause.append(org.skyve.persistence.DocumentQuery.THIS_ALIAS).append('.').append(binding).append(" IS NULL");
	}

	@Override
	public void addNotNull(String binding) {
		if (filterClause.length() > 0) {
			filterClause.append(" AND ");
		}
		filterClause.append(org.skyve.persistence.DocumentQuery.THIS_ALIAS).append('.').append(binding).append(" IS NOT NULL");
	}

	@Override
	public void addBetween(String binding, Object minOperand, Object maxOperand) {
		String minParameterName = "param" + owningQuery.parameterNumber++;
		String maxParameterName = "param" + owningQuery.parameterNumber++;
		owningQuery.putParameter(minParameterName, minOperand);
		owningQuery.putParameter(maxParameterName, maxOperand);

		if (filterClause.length() > 0) {
			filterClause.append(" AND ");
		}
		filterClause.append(org.skyve.persistence.DocumentQuery.THIS_ALIAS).append('.').append(binding).append(" BETWEEN ");
		filterClause.append(':').append(minParameterName).append(" and :").append(maxParameterName);
	}

	@Override
	public void addCollectionSizeEquals(String binding, int operand) {
		appendRestriction(binding, ".size = ", new Integer(operand), false, null, null);
	}

	@Override
	public void addCollectionSizeNotEquals(String binding, int operand) {
		appendRestriction(binding, ".size != ", new Integer(operand), false, null, null);
	}

	@Override
	public void addCollectionSizeGreaterThan(String binding, int operand) {
		appendRestriction(binding, ".size > ", new Integer(operand), false, null, null);
	}

	@Override
	public void addCollectionSizeGreaterThanOrEqualTo(String binding, int operand) {
		appendRestriction(binding, ".size >= ", new Integer(operand), false, null, null);
	}

	@Override
	public void addCollectionSizeLessThan(String binding, int operand) {
		appendRestriction(binding, ".size < ", new Integer(operand), false, null, null);
	}

	@Override
	public void addCollectionSizeLessThanOrEqualTo(String binding, int operand) {
		appendRestriction(binding, ".size <= ", new Integer(operand), false, null, null);
	}
	
	private void appendRestriction(String binding,
									String operator,
									Object operand,
									boolean addNullTest,
									String functionPrefix,
									String functionSuffix) {
		boolean isALikeOperator = (LIKE_OPERATOR.equals(operator) || NOT_LIKE_OPERATOR.equals(operator));

		String parameterName = "param" + owningQuery.parameterNumber++;
		if (! isALikeOperator) {
			owningQuery.putParameter(parameterName, operand);
		}
		if (filterClause.length() > 0) {
			filterClause.append(" AND ");
		}
		
		if (addNullTest) {
			filterClause.append('(').append(org.skyve.persistence.DocumentQuery.THIS_ALIAS).append('.').append(binding);
			filterClause.append(" IS NULL OR ");
		}
		if (isALikeOperator) {
			filterClause.append(org.skyve.persistence.DocumentQuery.THIS_ALIAS).append('.').append(binding);
			filterClause.append(operator).append('\'').append(operand).append('\'');
		}
		else {
			if (operator == null) {
				filterClause.append(functionPrefix);
				filterClause.append(org.skyve.persistence.DocumentQuery.THIS_ALIAS).append('.').append(binding);
				filterClause.append(", :").append(parameterName).append(functionSuffix);
			}
			else {
				filterClause.append(org.skyve.persistence.DocumentQuery.THIS_ALIAS).append('.').append(binding);
				filterClause.append(operator).append(':').append(parameterName);
			}
		}
		if (addNullTest) {
			filterClause.append(')');
		}
	}

	@Override
	public void addConjunction(org.skyve.persistence.DocumentFilter filter) {
		if (filterClause.length() > 0) {
			filterClause.append(" AND ");
		}
		filterClause.append('(').append(filter).append(')');
	}

	@Override
	public void addDisjunction(org.skyve.persistence.DocumentFilter filter) {
		if (filterClause.length() > 0) {
			filterClause.insert(0, '(');
			filterClause.append(") OR ");
		}
		filterClause.append('(').append(filter).append(')');
	}

	@Override
	public void addExpression(String expression) {
		if (filterClause.length() > 0) {
			filterClause.append(" AND ");
		}
		filterClause.append(expression);
	}
	
	@Override
	public boolean isEmpty() {
		return (filterClause.length() == 0);
	}

	@Override
	public String toString() {
		return (filterClause.length() > 0) ? filterClause.toString() : null;
	}

	@Override
	protected DocumentFilterImpl clone() throws CloneNotSupportedException {
		DocumentFilterImpl result = (DocumentFilterImpl) super.clone();

		result.filterClause = new StringBuilder(filterClause);

		return result;
	}
}
