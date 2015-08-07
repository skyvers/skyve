package org.skyve.wildcat.persistence;

import org.skyve.CORE;
import org.skyve.domain.ChildBean;
import org.skyve.domain.PersistentBean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.DocumentFilter;
import org.skyve.persistence.DocumentQuery;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.wildcat.bind.BindUtil;

import com.vividsolutions.jts.geom.Geometry;

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
			this.filterClause.append(new AbstractBizQL(filterClause).toQueryString(false));
		}
	}

	void setQuery(AbstractDocumentQuery owningQuery) {
		this.owningQuery = owningQuery;
	}

	@Override
	public void addEquals(String binding, Object operand) {
		appendRestriction(binding, " = ", operand, false, false, null, null);
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
		filterClause.append(DocumentQuery.THIS_ALIAS).append('.').append(binding);
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
		appendRestriction(binding, " != ", operand, false, false, null, null);
	}

	@Override
	public void addGreaterThan(String binding, Object operand) {
		appendRestriction(binding, " > ", operand, false, false, null, null);
	}

	@Override
	public void addGreaterThanOrEqualTo(String binding, Object operand) {
		appendRestriction(binding, " >= ", operand, false, false, null, null);
	}

	@Override
	public void addLessThan(String binding, Object operand) {
		appendRestriction(binding, " < ", operand, false, false, null, null);
	}

	@Override
	public void addLessThanOrEqualTo(String binding, Object operand) {
		appendRestriction(binding, " <= ", operand, false, false, null, null);
	}

	@Override
	public void addLike(String binding, String operand) {
		appendRestriction(binding, LIKE_OPERATOR, operand, false, useStr(binding), null, null);
	}

	@Override
	public void addNotLike(String binding, String operand) {
		appendRestriction(binding, NOT_LIKE_OPERATOR, operand, false, useStr(binding), null, null);
	}

	@Override
	public void addEquals(String binding, Geometry geometry) {
		appendRestriction(binding, null, geometry, false, false, "equals(", ") = true");
	}

	@Override
	public void addDisjoint(String binding, Geometry geometry) {
		appendRestriction(binding, null, geometry, false, false, "disjoint(", ") = true");
	}
	
	@Override
	public void addIntersects(String binding, Geometry geometry) {
		appendRestriction(binding, null, geometry, false, false, "intersects(", ") = true");
	}

	@Override
	public void addTouches(String binding, Geometry geometry) {
		appendRestriction(binding, null, geometry, false, false, "touches(", ") = true");
	}

	@Override
	public void addCrosses(String binding, Geometry geometry) {
		appendRestriction(binding, null, geometry, false, false, "crosses(", ") = true");
	}

	@Override
	public void addWithin(String binding, Geometry geometry) {
		appendRestriction(binding, null, geometry, false, false, "within(", ") = true");
	}
	
	@Override
	public void addContains(String binding, Geometry geometry) {
		appendRestriction(binding, null, geometry, false, false, "contains(", ") = true");
	}
	
	@Override
	public void addOverlaps(String binding, Geometry geometry) {
		appendRestriction(binding, null, geometry, false, false, "overlaps(", ") = true");
	}
	
	@Override
	public void addNullOrEquals(String binding, Object operand) {
		appendRestriction(binding, " = ", operand, true, false, null, null);
	}

	@Override
	public void addNullOrNotEquals(String binding, Object operand) {
		appendRestriction(binding, " != ", operand, true, false, null, null);
	}

	@Override
	public void addNullOrGreaterThan(String binding, Object operand) {
		appendRestriction(binding, " > ", operand, true, false, null, null);
	}

	@Override
	public void addNullOrGreaterThanOrEqualTo(String binding, Object operand) {
		appendRestriction(binding, " >= ", operand, true, false, null, null);
	}

	@Override
	public void addNullOrLessThan(String binding, Object operand) {
		appendRestriction(binding, " < ", operand, true, false, null, null);
	}

	@Override
	public void addNullOrLessThanOrEqualTo(String binding, Object operand) {
		appendRestriction(binding, " <= ", operand, true, false, null, null);
	}

	@Override
	public void addNullOrLike(String binding, String operand) {
		appendRestriction(binding, LIKE_OPERATOR, operand, true, useStr(binding), null, null);
	}

	@Override
	public void addNullOrNotLike(String binding, String operand) {
		appendRestriction(binding, NOT_LIKE_OPERATOR, operand, true, useStr(binding), null, null);
	}

	@Override
	public void addNullOrEquals(String binding, Geometry geometry) {
		appendRestriction(binding, null, geometry, true, false, "equals(", ") = true");
	}

	@Override
	public void addNullOrDisjoint(String binding, Geometry geometry) {
		appendRestriction(binding, null, geometry, true, false, "disjoint(", ") = true");
	}
	
	@Override
	public void addNullOrIntersects(String binding, Geometry geometry) {
		appendRestriction(binding, null, geometry, true, false, "intersects(", ") = true");
	}

	@Override
	public void addNullOrTouches(String binding, Geometry geometry) {
		appendRestriction(binding, null, geometry, true, false, "touches(", ") = true");
	}

	@Override
	public void addNullOrCrosses(String binding, Geometry geometry) {
		appendRestriction(binding, null, geometry, true, false, "crosses(", ") = true");
	}

	@Override
	public void addNullOrWithin(String binding, Geometry geometry) {
		appendRestriction(binding, null, geometry, true, false, "within(", ") = true");
	}
	
	@Override
	public void addNullOrContains(String binding, Geometry geometry) {
		appendRestriction(binding, null, geometry, true, false, "contains(", ") = true");
	}
	
	@Override
	public void addNullOrOverlaps(String binding, Geometry geometry) {
		appendRestriction(binding, null, geometry, true, false, "overlaps(", ") = true");
	}
	
	@Override
	public void addNull(String binding) {
		if (filterClause.length() > 0) {
			filterClause.append(" AND ");
		}
		filterClause.append(DocumentQuery.THIS_ALIAS).append('.').append(binding).append(" IS NULL");
	}

	@Override
	public void addNotNull(String binding) {
		if (filterClause.length() > 0) {
			filterClause.append(" AND ");
		}
		filterClause.append(DocumentQuery.THIS_ALIAS).append('.').append(binding).append(" IS NOT NULL");
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
		filterClause.append(DocumentQuery.THIS_ALIAS).append('.').append(binding).append(" BETWEEN ");
		filterClause.append(':').append(minParameterName).append(" and :").append(maxParameterName);
	}

	@Override
	public void addCollectionSizeEquals(String binding, int operand) {
		appendRestriction(binding, ".size = ", new Integer(operand), false, false, null, null);
	}

	@Override
	public void addCollectionSizeNotEquals(String binding, int operand) {
		appendRestriction(binding, ".size != ", new Integer(operand), false, false, null, null);
	}

	@Override
	public void addCollectionSizeGreaterThan(String binding, int operand) {
		appendRestriction(binding, ".size > ", new Integer(operand), false, false, null, null);
	}

	@Override
	public void addCollectionSizeGreaterThanOrEqualTo(String binding, int operand) {
		appendRestriction(binding, ".size >= ", new Integer(operand), false, false, null, null);
	}

	@Override
	public void addCollectionSizeLessThan(String binding, int operand) {
		appendRestriction(binding, ".size < ", new Integer(operand), false, false, null, null);
	}

	@Override
	public void addCollectionSizeLessThanOrEqualTo(String binding, int operand) {
		appendRestriction(binding, ".size <= ", new Integer(operand), false, false, null, null);
	}
	
	private void appendRestriction(String binding,
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
			filterClause.append('(').append(DocumentQuery.THIS_ALIAS).append('.').append(binding);
			filterClause.append(" IS NULL OR ");
		}

		if (operator == null) {
			filterClause.append(functionPrefix);
			filterClause.append(DocumentQuery.THIS_ALIAS).append('.').append(binding);
			filterClause.append(", :").append(parameterName).append(functionSuffix);
		}
		else {
			if (useStr) {
				filterClause.append("str(").append(DocumentQuery.THIS_ALIAS).append('.').append(binding).append(')');
			}
			else {
				filterClause.append(DocumentQuery.THIS_ALIAS).append('.').append(binding);
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
*/
	}

	@Override
	public void addAnd(DocumentFilter filter) {
		if (filterClause.length() > 0) {
			filterClause.append(" AND ");
		}
		filterClause.append('(').append(filter).append(')');
	}

	@Override
	public void addOr(DocumentFilter filter) {
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

	private boolean useStr(String binding) {
		try {
			String lastBinding = binding;
			int lastDotIndex = binding.lastIndexOf('.');
			if (lastDotIndex > 0) {
				lastBinding = binding.substring(lastDotIndex + 1);
			}
			if (BindUtil.isImplicit(lastBinding)) {
				if (ChildBean.ORDINAL_KEY.equals(lastBinding) ||
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
		catch (Exception e) {
			// do nothing - it'll return false below
		}
		
		return false;
	}
}
