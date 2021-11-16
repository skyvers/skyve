package org.skyve.metadata.view.model.list;

import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.SortedMap;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.locationtech.jts.geom.Geometry;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.types.Decimal;
import org.skyve.domain.types.converters.Converter;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.model.document.field.ConvertableField;
import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.impl.metadata.model.document.field.Field;
import org.skyve.metadata.MetaData;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.view.widget.FilterParameter;
import org.skyve.metadata.view.widget.bound.Parameter;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.util.Util;
import org.skyve.web.SortParameter;

public abstract class ListModel<T extends Bean> implements MetaData {
	private static final long serialVersionUID = -5786617076399299709L;

	public static final String ADMIN_MODULE_NAME = "admin";
	public static final String GENERIC_DOCUMENT_NAME = "Generic";
	public static final String MEMO_1_PROPERTY_NAME = "memo1";

	private T bean;
	public T getBean() {
		return bean;
	}
	public void setBean(T bean) {
		this.bean = bean;
	}

	private int startRow = Integer.MIN_VALUE;
	private int endRow = Integer.MIN_VALUE;
	private SortParameter[] sorts;
	private AggregateFunction summary;
	private String selectedTagId;
	
	protected final int getStartRow() {
		return startRow;
	}
	
	public final void setStartRow(int startRow) {
		this.startRow = startRow;
	}

	protected final int getEndRow() {
		return endRow;
	}

	public void setEndRow(int endRow) {
		this.endRow = endRow;
	}

	protected final SortParameter[] getSortParameters() {
		return sorts;
	}

	public void setSortParameters(SortParameter[] sorts) {
		this.sorts = sorts;
	}

	protected final AggregateFunction getSummary() {
		return summary;
	}

	public void setSummary(AggregateFunction summary) {
		this.summary = summary;
	}

	protected final String getSelectedTagId() {
		return selectedTagId;
	}

	public void setSelectedTagId(String selectedTagId) {
		this.selectedTagId = selectedTagId;
	}

	public final void addFilterParameters(Document drivingDocument,
											List<FilterParameter> filterParameters,
											List<Parameter> parameters)
	throws Exception {
		Filter filter = getFilter();
		Customer customer = CORE.getUser().getCustomer();
		Module drivingModule = customer.getModule(drivingDocument.getOwningModuleName());
		
		if (filterParameters != null) {
			for (FilterParameter param : filterParameters) {
				String parameterName = param.getFilterBinding();
	
				Pair<String, Object> parameter = processParameter(customer,
																	drivingModule,
																	drivingDocument,
																	parameterName,
																	param.getValueBinding(),
																	param.getValue());
				parameterName = parameter.getLeft();
				Object value = parameter.getRight();
				
				// Add the filter to the model
				switch (param.getOperator()) {
					case equal:
						if (value != null) {
							addEquals(filter, parameterName, value);
						}
						break;
					case greater:
						if (value != null) {
							addGreaterThan(filter, parameterName, value);
						}
						break;
					case greaterEqual:
						if (value != null) {
							addGreaterThanOrEqualTo(filter, parameterName, value);
						}
						break;
					case isNull:
						filter.addNull(parameterName);
						break;
					case less:
						if (value != null) {
							addLessThan(filter, parameterName, value);
						}
						break;
					case lessEqual:
						if (value != null) {
							addLessThanOrEqualTo(filter, parameterName, value);
						}
						break;
					case like:
						if (value != null) {
							filter.addContains(parameterName, (String) value);
						}
						break;
					case notEqual:
						if (value != null) {
							addNotEquals(filter, parameterName, value);
						}
						break;
					case notLike:
						if (value != null) {
							filter.addNotContains(parameterName, (String) value);
						}
						break;
					case notNull:
						filter.addNotNull(parameterName);
						break;
					case nullOrEqual:
						if (value != null) {
							Filter equalFilter = newFilter();
							addEquals(equalFilter, parameterName, value);
							addNullOrSomething(filter, equalFilter, parameterName);
						}
						break;
					case nullOrGreater:
						if (value != null) {
							Filter greaterFilter = newFilter();
							addGreaterThan(greaterFilter, parameterName, value);
							addNullOrSomething(filter, greaterFilter, parameterName);
						}
						break;
					case nullOrGreaterEqual:
						if (value != null) {
							Filter greaterEqualFilter = newFilter();
							addGreaterThan(greaterEqualFilter, parameterName, value);
							addNullOrSomething(filter, greaterEqualFilter, parameterName);
						}
						break;
					case nullOrLess:
						if (value != null) {
							Filter lessFilter = newFilter();
							addGreaterThan(lessFilter, parameterName, value);
							addNullOrSomething(filter, lessFilter, parameterName);
						}
						break;
					case nullOrLessEqual:
						if (value != null) {
							Filter lessEqualFilter = newFilter();
							addGreaterThan(lessEqualFilter, parameterName, value);
							addNullOrSomething(filter, lessEqualFilter, parameterName);
						}
						break;
					case nullOrLike:
						if (value != null) {
							Filter likeFilter = newFilter();
							likeFilter.addContains(parameterName, (String) value);
							addNullOrSomething(filter, likeFilter, parameterName);
						}
						break;
					case nullOrNotEqual:
						if (value != null) {
							Filter notEqualFilter = newFilter();
							addNotEquals(notEqualFilter, parameterName, value);
							addNullOrSomething(filter, notEqualFilter, parameterName);
						}
						break;
					case nullOrNotLike:
						if (value != null) {
							Filter notLikeFilter = newFilter();
							notLikeFilter.addNotContains(parameterName, (String) value);
							addNullOrSomething(filter, notLikeFilter, parameterName);
						}
						break;
					default:
						throw new IllegalStateException(param.getOperator() + " is not supported");
				}
			}
		}
		
		if (parameters != null) {
			for (Parameter param : parameters) {
				Pair<String, Object> parameter = processParameter(customer,
																	drivingModule,
																	drivingDocument,
																	param.getName(),
																	param.getValueBinding(),
																	param.getValue());
				putParameter(parameter.getLeft(), parameter.getRight());
			}
		}
	}
	
	private Pair<String, Object> processParameter(Customer c,
													Module m,
													Document d,
													String name,
													String binding,
													String value) {
		String newBinding = binding;

		// The resulting parameter contents
		String newName = name;
		Object result = null;

		// Determine the parameter value to use
		if (newBinding == null) {
			if (value != null) {
				// check for a surrogate binding
				if (value.startsWith("{") && value.endsWith("}")) {
					newBinding = value.substring(1, value.length() - 1);
				}
				else {
					result = value;
				}
			}
		}
		if (newBinding != null) {
			result = BindUtil.get(bean, newBinding);
		}
		if (result instanceof Bean) {
			result = ((Bean) result).getBizId();
		}

		if (result != null) {
			// Determine the type and converter of the filtered attribute
			Converter<?> converter = null;
    		Class<?> type = null;

    		// Determine the parameter name to use
			TargetMetaData target = BindUtil.getMetaDataForBinding(c, m, d, name);
			if (target != null) {
				Attribute attribute = target.getAttribute();
				if (attribute != null) {
					if (attribute instanceof Enumeration) {
						type = ((Enumeration) attribute).getEnum();
					}
					else if (attribute instanceof Field) {
						type = attribute.getAttributeType().getImplementingType();
					}
					else if (attribute instanceof Association) {
						newName = String.format("%s.%s", name, Bean.DOCUMENT_ID);
					}
					
					if (attribute instanceof ConvertableField) {
						ConvertableField field = (ConvertableField) attribute;
						converter = field.getConverterForCustomer(c);
					}
				}
    			else if (ChildBean.PARENT_NAME.equals(name) || name.endsWith(ChildBean.CHILD_PARENT_NAME_SUFFIX)) {
					newName = String.format("%s.%s", name, Bean.DOCUMENT_ID);
    			}
			}

			if (type != null) {
				result = BindUtil.fromString(c, converter, type, result.toString(), false);
			}
		}
		
		return new ImmutablePair<>(newName, result);
	}
	
	public abstract String getDescription();
	
	public String getLocalisedDescription() {
		return Util.i18n(getDescription());
	}
	
	public abstract Document getDrivingDocument();
	
	public abstract List<MetaDataQueryColumn> getColumns();
	
	public abstract Set<String> getProjections();
	
	public abstract Filter getFilter() throws Exception;
	public abstract Filter newFilter() throws Exception;
	public abstract void putParameter(String name, Object value);
	public abstract Page fetch() throws Exception;
	public abstract AutoClosingIterable<Bean> iterate() throws Exception;
	
	public abstract Bean update(String bizId, SortedMap<String, Object> properties)
	throws Exception;
	public abstract void remove(String bizId) throws Exception;
	
	public static void addEquals(Filter filter, String binding, Object value) {
		if (value instanceof String) {
			filter.addEquals(binding, (String) value);
		}
    	else if (value instanceof Date) {
    		filter.addEquals(binding, (Date) value);
    	}
    	else if (value instanceof Integer) {
    		filter.addEquals(binding, (Integer) value);
    	}
    	else if (value instanceof Long) {
    		filter.addEquals(binding, (Long) value);
    	}
    	else if (value instanceof Decimal) {
    		filter.addEquals(binding, (Decimal) value);
    	}
		else if (value instanceof Boolean) {
			filter.addEquals(binding, (Boolean) value);
		}
    	else if (value instanceof Enum) {
    		filter.addEquals(binding, (Enum<?>) value);
    	}
    	else if (value instanceof Geometry) {
    		filter.addEquals(binding, (Geometry) value);
    	}
    	else {
    		throw new IllegalArgumentException(value + " is not catered for in filtering");
    	}
    }
    
    public static void addNotEquals(Filter filter, String binding, Object value) {
    	if (value instanceof String) {
			filter.addNotEquals(binding, (String) value);
		}
    	else if (value instanceof Date) {
    		filter.addNotEquals(binding, (Date) value);
    	}
    	else if (value instanceof Integer) {
    		filter.addNotEquals(binding, (Integer) value);
    	}
    	else if (value instanceof Long) {
    		filter.addNotEquals(binding, (Long) value);
    	}
    	else if (value instanceof Decimal) {
    		filter.addNotEquals(binding, (Decimal) value);
    	}
    	else if (value instanceof Boolean) {
    		filter.addNotEquals(binding, (Boolean) value);
    	}
    	else if (value instanceof Enum) {
    		filter.addNotEquals(binding, (Enum<?>) value);
    	}
    	else if (value instanceof Geometry) {
    		filter.addNotEquals(binding, (Geometry) value);
    	}
    	else {
    		throw new IllegalArgumentException(value + " is not catered for in filtering");
    	}
    }
    
    public static void addGreaterThan(Filter filter, String binding, Object value) {
    	if (value instanceof String) {
    		filter.addGreaterThan(binding, (String) value);
    	}
    	else if (value instanceof Date) {
    		filter.addGreaterThan(binding, (Date) value);
    	}
    	else if (value instanceof Integer) {
    		filter.addGreaterThan(binding, (Integer) value);
    	}
    	else if (value instanceof Long) {
    		filter.addGreaterThan(binding, (Long) value);
    	}
    	else if (value instanceof Decimal) {
    		filter.addGreaterThan(binding, (Decimal) value);
    	}
    	else {
    		throw new IllegalArgumentException(value + " is not catered for in filtering");
    	}
    }
    
    public static void addGreaterThanOrEqualTo(Filter filter, String binding, Object value) {
    	if (value instanceof String) {
    		filter.addGreaterThanOrEqualTo(binding, (String) value);
    	}
    	else if (value instanceof Date) {
    		filter.addGreaterThanOrEqualTo(binding, (Date) value);
    	}
    	else if (value instanceof Integer) {
    		filter.addGreaterThanOrEqualTo(binding, (Integer) value);
    	}
    	else if (value instanceof Long) {
    		filter.addGreaterThanOrEqualTo(binding, (Long) value);
    	}
    	else if (value instanceof Decimal) {
    		filter.addGreaterThanOrEqualTo(binding, (Decimal) value);
    	}
    	else {
    		throw new IllegalArgumentException(value + " is not catered for in filtering");
    	}
    }
    
    public static void addLessThan(Filter filter, String binding, Object value) {
    	if (value instanceof String) {
			filter.addLessThan(binding, (String) value);
    	}
    	else if (value instanceof Date) {
    		filter.addLessThan(binding, (Date) value);
    	}
    	else if (value instanceof Integer) {
    		filter.addLessThan(binding, (Integer) value);
    	}
    	else if (value instanceof Long) {
    		filter.addLessThan(binding, (Long) value);
    	}
    	else if (value instanceof Decimal) {
    		filter.addLessThan(binding, (Decimal) value);
    	}
    	else {
    		throw new IllegalArgumentException(value + " is not catered for in filtering");
    	}
    }
    
    public static void addLessThanOrEqualTo(Filter filter, String binding, Object value) {
    	if (value instanceof String) {
			filter.addLessThanOrEqualTo(binding, (String) value);
    	}
    	else if (value instanceof Date) {
    		filter.addLessThanOrEqualTo(binding, (Date) value);
    	}
    	else if (value instanceof Integer) {
    		filter.addLessThanOrEqualTo(binding, (Integer) value);
    	}
    	else if (value instanceof Long) {
    		filter.addLessThanOrEqualTo(binding, (Long) value);
    	}
    	else if (value instanceof Decimal) {
    		filter.addLessThanOrEqualTo(binding, (Decimal) value);
    	}
    	else {
    		throw new IllegalArgumentException(value + " is not catered for in filtering");
    	}
    }
    
    public static void addBetween(Filter filter, String binding, Object start, Object end) {
    	if (start instanceof String) {
			filter.addBetween(binding, (String) start, (String) end);
    	}
    	else if (start instanceof Date) {
    		filter.addBetween(binding, (Date) start, (Date) end);
    	}
    	else if (start instanceof Integer) {
    		filter.addBetween(binding, (Integer) start, (Integer) end);
    	}
    	else if (start instanceof Long) {
    		filter.addBetween(binding, (Long) start, (Long) end);
    	}
    	else if (start instanceof Decimal) {
    		filter.addBetween(binding, (Decimal) start, (Decimal) end);
    	}
    	else {
    		throw new IllegalArgumentException(start + " or " + end + " is not catered for in filtering");
    	}
    }
    
	private void addNullOrSomething(Filter filterToAddTo, Filter somethingFilter, String binding) 
	throws Exception {
		Filter orFilter = newFilter();
		orFilter.addNull(binding);
		orFilter.addOr(somethingFilter);
		filterToAddTo.addAnd(orFilter);
	}
}
