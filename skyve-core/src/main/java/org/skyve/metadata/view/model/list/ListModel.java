package org.skyve.metadata.view.model.list;

import java.util.ArrayList;
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
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.Decimal;
import org.skyve.domain.types.converters.Converter;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.field.ConvertibleField;
import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.impl.metadata.model.document.field.Field;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.view.model.ViewModel;
import org.skyve.metadata.view.widget.FilterParameter;
import org.skyve.metadata.view.widget.bound.Parameter;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.util.Util;
import org.skyve.web.SortParameter;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

public abstract class ListModel<T extends Bean> implements ViewModel {
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

	public final void addParameters(Document drivingDocument,
										List<FilterParameter> filterParameters,
										List<Parameter> parameters)
	throws Exception {
		Filter filter = getFilter();
		Customer customer = CORE.getCustomer();
		Module drivingModule = customer.getModule(drivingDocument.getOwningModuleName());
		
		if (filterParameters != null) {
			for (FilterParameter param : filterParameters) {
				String parameterName = param.getFilterBinding();
	
				Pair<String, Object> parameter = processParameter(customer,
																	drivingModule,
																	drivingDocument,
																	parameterName,
																	param.getValueBinding(),
																	param.getValue(),
																	true);
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
																	param.getValue(),
																	false);
				putParameter(parameter.getLeft(), parameter.getRight());
			}
		}
	}
	
	private Pair<String, Object> processParameter(@Nonnull Customer customer,
													@Nonnull Module module,
													@Nonnull Document document,
													@Nonnull String name,
													@Nullable String binding,
													@Nullable String value,
													boolean filter) {
		String newBinding = binding;

		// The resulting parameter contents
		String newName = name;
		Object newValue = null;

		// Determine the parameter value to use
		if (newBinding == null) {
			if (value != null) {
				// check for a surrogate binding
				if (value.startsWith("{") && value.endsWith("}")) {
					newBinding = value.substring(1, value.length() - 1);
				}
				else {
					newValue = value;
				}
			}
		}
		if (newBinding != null) {
			newValue = BindUtil.get(bean, newBinding);
		}
		if (filter && (newValue instanceof Bean newBean)) {
			newValue = newBean.getBizId();
		}

		if (newValue != null) {
			// Determine the type and converter of the filtered attribute
			Converter<?> converter = null;
    		Class<?> type = null;

    		// Name must be a valid binding if we are adding a filter criteria
    		// Not necessarily a valid binding if processing a query parameter
    		TargetMetaData target = null;
			try {
				target = BindUtil.getMetaDataForBinding(customer, module, document, name);
    		}
    		catch (MetaDataException e ) {
    			if (filter) {
    				throw e;
    			}
    		}
			if (target != null) {
				Attribute attribute = target.getAttribute();
				if (attribute != null) {
					if (attribute instanceof Field) {
						type = attribute.getImplementingType();
	
						if (attribute instanceof Enumeration enumeration) {
							converter = enumeration.getConverter();
						}
						else if (attribute instanceof ConvertibleField field) {
							converter = field.getConverterForCustomer(customer);
						}
					}
					else if (attribute instanceof Association association) {
						if (filter) {
							newName = name + '.' + Bean.DOCUMENT_ID;
						}
						else {
							if (newValue instanceof String newString) {
								Document targetDocument = target.getDocument();
								Module m = customer.getModule(targetDocument.getOwningModuleName());
								Document d = m.getDocument(customer, association.getDocumentName());
								newValue = CORE.getPersistence().retrieve(d, newString);
							}
							else if (! (newValue instanceof Bean)) {
								throw new DomainException(newValue + " is not supported as an association parameter");
							}
						}
					}
				}
				else if (ChildBean.PARENT_NAME.equals(name) || name.endsWith(ChildBean.CHILD_PARENT_NAME_SUFFIX)) {
					newName = name + '.' + Bean.DOCUMENT_ID;
				}
			}
			
			if ((type != null) && (newValue != null)) {
				// Check if newValue is already the required type, if not...
				Class<?> valueType = newValue.getClass();
				if (! type.isAssignableFrom(valueType)) {
					// try converting it
					Object convertedNewValue = BindUtil.nullSafeConvert(type, newValue);
					// if the conversion did not produce a new value - try a String conversion
					if (convertedNewValue == newValue) {
						newValue = BindUtil.fromString(customer, converter, type, newValue.toString());
					}
					else {
						newValue = convertedNewValue;
					}
				}
			}
		}
		
		return new ImmutablePair<>(newName, newValue);
	}
	
	public abstract String getDescription();
	
	public String getLocalisedDescription() {
		return Util.i18n(getDescription());
	}
	
	/**
	 * Use this to get the localised column title to use as set in the displayName in the column definition 
	 * or if no displayName, and their is a column binding, use the displayName from the document meta data.
	 * 
	 * @param column	The column to get the title for.
	 * @return	The title.
	 */
	public String determineColumnTitle(MetaDataQueryColumn column) {
		String result = column.getLocalisedDisplayName();
		if (result == null) {
			String binding = column.getBinding();
			if (binding != null) {
				Document d = getDrivingDocument();
				Customer c = CORE.getCustomer();
				Module m = c.getModule(d.getOwningModuleName());
				TargetMetaData target = BindUtil.getMetaDataForBinding(c, m, d, binding);
				Document targetDocument = target.getDocument();
				Attribute targetAttribute = target.getAttribute();
				if (binding.endsWith(Bean.BIZ_KEY)) {
					result = targetDocument.getLocalisedSingularAlias();
				}
				else if (binding.endsWith(Bean.ORDINAL_NAME)) {
					result = DocumentImpl.getBizOrdinalAttribute().getLocalisedDisplayName();
				}
				else if (targetAttribute != null) {
					result = targetAttribute.getLocalisedDisplayName();
				}
			}
			if (result == null) {
				result = column.getName();
			}
		}
		return result;
	}
	
	public abstract Document getDrivingDocument();
	
	public abstract List<MetaDataQueryColumn> getColumns();
	
	public abstract Set<String> getProjections();
	
	public abstract Filter getFilter();
	public abstract Filter newFilter();
	public abstract void putParameter(String name, Object value);
	public abstract Page fetch() throws Exception;
	public abstract AutoClosingIterable<Bean> iterate() throws Exception;
	
	public abstract Bean update(String bizId, SortedMap<String, Object> properties)
	throws Exception;
	public abstract void remove(String bizId) throws Exception;
	
	public static void addEquals(Filter filter, String binding, Object value) {
		if (value instanceof String string) {
			filter.addEquals(binding, string);
		}
    	else if (value instanceof Date date) {
    		filter.addEquals(binding, date);
    	}
    	else if (value instanceof Integer integer) {
    		filter.addEquals(binding, integer);
    	}
    	else if (value instanceof Long longValue) {
    		filter.addEquals(binding, longValue);
    	}
    	else if (value instanceof Decimal decimal) {
    		filter.addEquals(binding, decimal);
    	}
		else if (value instanceof Boolean booleanValue) {
			filter.addEquals(binding, booleanValue);
		}
    	else if (value instanceof Enum<?> enumValue) {
    		filter.addEquals(binding, enumValue);
    	}
    	else if (value instanceof Geometry geometry) {
    		filter.addEquals(binding, geometry);
    	}
    	else {
    		throw new IllegalArgumentException(value + " is not catered for in filtering");
    	}
    }
    
    public static void addNotEquals(Filter filter, String binding, Object value) {
    	if (value instanceof String string) {
			filter.addNotEquals(binding, string);
		}
    	else if (value instanceof Date date) {
    		filter.addNotEquals(binding, date);
    	}
    	else if (value instanceof Integer integer) {
    		filter.addNotEquals(binding, integer);
    	}
    	else if (value instanceof Long longValue) {
    		filter.addNotEquals(binding, longValue);
    	}
    	else if (value instanceof Decimal decimal) {
    		filter.addNotEquals(binding, decimal);
    	}
    	else if (value instanceof Boolean booleanValue) {
    		filter.addNotEquals(binding, booleanValue);
    	}
    	else if (value instanceof Enum<?> enumValue) {
    		filter.addNotEquals(binding, enumValue);
    	}
    	else if (value instanceof Geometry geometry) {
    		filter.addNotEquals(binding, geometry);
    	}
    	else {
    		throw new IllegalArgumentException(value + " is not catered for in filtering");
    	}
    }
    
    public static void addGreaterThan(Filter filter, String binding, Object value) {
    	if (value instanceof String string) {
    		filter.addGreaterThan(binding, string);
    	}
    	else if (value instanceof Date date) {
    		filter.addGreaterThan(binding, date);
    	}
    	else if (value instanceof Integer integer) {
    		filter.addGreaterThan(binding, integer);
    	}
    	else if (value instanceof Long longValue) {
    		filter.addGreaterThan(binding, longValue);
    	}
    	else if (value instanceof Decimal decimal) {
    		filter.addGreaterThan(binding, decimal);
    	}
    	else {
    		throw new IllegalArgumentException(value + " is not catered for in filtering");
    	}
    }
    
    public static void addGreaterThanOrEqualTo(Filter filter, String binding, Object value) {
    	if (value instanceof String string) {
    		filter.addGreaterThanOrEqualTo(binding, string);
    	}
    	else if (value instanceof Date date) {
    		filter.addGreaterThanOrEqualTo(binding, date);
    	}
    	else if (value instanceof Integer integer) {
    		filter.addGreaterThanOrEqualTo(binding, integer);
    	}
    	else if (value instanceof Long longValue) {
    		filter.addGreaterThanOrEqualTo(binding, longValue);
    	}
    	else if (value instanceof Decimal decimal) {
    		filter.addGreaterThanOrEqualTo(binding, decimal);
    	}
    	else {
    		throw new IllegalArgumentException(value + " is not catered for in filtering");
    	}
    }
    
    public static void addLessThan(Filter filter, String binding, Object value) {
    	if (value instanceof String string) {
			filter.addLessThan(binding, string);
    	}
    	else if (value instanceof Date date) {
    		filter.addLessThan(binding, date);
    	}
    	else if (value instanceof Integer integer) {
    		filter.addLessThan(binding, integer);
    	}
    	else if (value instanceof Long longValue) {
    		filter.addLessThan(binding, longValue);
    	}
    	else if (value instanceof Decimal decimal) {
    		filter.addLessThan(binding, decimal);
    	}
    	else {
    		throw new IllegalArgumentException(value + " is not catered for in filtering");
    	}
    }
    
    public static void addLessThanOrEqualTo(Filter filter, String binding, Object value) {
    	if (value instanceof String string) {
			filter.addLessThanOrEqualTo(binding, string);
    	}
    	else if (value instanceof Date date) {
    		filter.addLessThanOrEqualTo(binding, date);
    	}
    	else if (value instanceof Integer integer) {
    		filter.addLessThanOrEqualTo(binding, integer);
    	}
    	else if (value instanceof Long longValue) {
    		filter.addLessThanOrEqualTo(binding, longValue);
    	}
    	else if (value instanceof Decimal decimal) {
    		filter.addLessThanOrEqualTo(binding, decimal);
    	}
    	else {
    		throw new IllegalArgumentException(value + " is not catered for in filtering");
    	}
    }
    
    public static void addBetween(Filter filter, String binding, Object start, Object end) {
    	if (start instanceof String stringStart) {
			filter.addBetween(binding, stringStart, (String) end);
    	}
    	else if (start instanceof Date dateStart) {
    		filter.addBetween(binding, dateStart, (Date) end);
    	}
    	else if (start instanceof Integer integerStart) {
    		filter.addBetween(binding, integerStart, (Integer) end);
    	}
    	else if (start instanceof Long longStart) {
    		filter.addBetween(binding, longStart, (Long) end);
    	}
    	else if (start instanceof Decimal decimalStart) {
    		filter.addBetween(binding, decimalStart, (Decimal) end);
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
	
	public static Object[] getTop100VariantDomainValueCodesFromDescriptionFilter(Document document, Attribute attribute, String like) {
		List<DomainValue> values = ((DocumentImpl) document).getDomainValues((CustomerImpl) CORE.getCustomer(),
																				DomainType.variant,
																				attribute, 
																				null,
																				true);
		
		if ((values == null) || values.isEmpty()) {
			return new Object[0];
		}
		
		List<String> codes = new ArrayList<>(values.size());
		for (DomainValue value : values) {
			String description = value.getLocalisedDescription();
			if (description.toLowerCase().contains(like.toLowerCase())) {
				codes.add(value.getCode());
			}
		}
		int length = Math.min(codes.size(), 100);
		AttributeType attributeType = attribute.getAttributeType();
		Class<?> implementingType = attribute.getImplementingType();
		Object[] result = new Object[length];
		for (int i = 0; i < length; i++) {
			// Leave strings, enumerations and bean bizIds alone
			if (String.class.equals(implementingType) || (attributeType == AttributeType.enumeration) || Bean.class.equals(implementingType)) {
				result[i] = codes.get(i);
			}
			// coerce to the correct type
			else {
				result[i] = BindUtil.fromSerialised(implementingType, codes.get(i));
			}
		}
		
		return result;
	}
}
