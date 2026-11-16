package org.skyve.metadata.view.model.chart;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.stream.Collectors;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.impl.metadata.OrderingImpl;
import org.skyve.impl.persistence.AbstractDocumentQuery;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.view.model.chart.colours.ColourSeries;
import org.skyve.metadata.view.model.chart.colours.RainbowColourSeries;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.util.Binder;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Generate the ChartData based on a simple declarative method call chain.
 * The chart is generated from a document, a document query or a metadata query.
 * The projections, orderings and groupings are cleared and re-applied
 * based on the method calls made in this builder.
 * 
 * @author mike
 */
public class ChartBuilder {
	private static final String CATEGORY_ALIAS = "category";
	private static final String VALUE_ALIAS = "value";

	private Document document;
	private DocumentQuery query;
	private String categoryBinding;
	private Bucket categoryBucket;
	private String valueBinding;
	private AggregateFunction valueFunction;
	private int top = Integer.MIN_VALUE;
	private SortDirection topSort;
	private OrderBy topOrderBy;
	private boolean topOthers = false;
	private SortDirection orderBySort;
	private OrderBy orderBy;
	private String fullyQualifiedJFreeChartPostProcessorClassName;
	private String fullyQualifiedPrimeFacesChartPostProcessorClassName;
	
	/**
	 * Document builder.
	 * @param moduleName
	 * @param documentName
	 */
	public @Nonnull ChartBuilder with(@Nonnull String moduleName, @Nonnull String documentName) {
		Customer c = CORE.getCustomer();
		Module m = c.getModule(moduleName);
		return with(m.getDocument(c, documentName));
	}
	
	/**
	 * Document Builder
	 * @param document
	 */
	public @Nonnull ChartBuilder with(@SuppressWarnings("hiding") @Nonnull Document document) {
		this.document = document;
		this.query = CORE.getPersistence().newDocumentQuery(document);
		return this;
	}
	
	/**
	 * Document Query Builder.
	 * This will clear the projections, orderings and groupings from this query.
	 * @param query
	 */
	public @Nonnull ChartBuilder with(@SuppressWarnings("hiding") @Nonnull DocumentQuery query) {
		this.query = query;
		this.document = query.getDrivingDocument();

		AbstractDocumentQuery adq = (AbstractDocumentQuery) this.query;
		adq.clearProjections();
		adq.clearOrderings();
		adq.clearGroups();
		
		return this;
	}
	
	/**
	 * Query builder.
	 * This will use the query without the projections, orderings and groupings.
	 * @param moduleName
	 * @param queryName
	 */
	public @Nonnull ChartBuilder withQuery(@Nonnull String moduleName, @Nonnull String queryName) {
		Customer c = CORE.getCustomer();
		Module m = c.getModule(moduleName);
		return with(m.getNullSafeMetaDataQuery(queryName));
	}

	/**
	 * Query builder.
	 * This will use the query without the projections, orderings and groupings.
	 * @param query
	 */
	public @Nonnull ChartBuilder with(@SuppressWarnings("hiding") @Nonnull MetaDataQueryDefinition query) {
		this.query = query.constructDocumentQuery(null, null);
		this.document = this.query.getDrivingDocument();

		AbstractDocumentQuery adq = (AbstractDocumentQuery) this.query;
		adq.clearProjections();
		adq.clearOrderings();
		adq.clearGroups();
		
		return this;
	}
	
	/**
	 * Set the category data field
	 * @param binding
	 */
	public @Nonnull ChartBuilder category(@Nonnull String binding) {
		categoryBinding = binding;
		return this;
	}
	
	/**
	 * Set the category data field with a bucketing function.
	 * @param binding
	 * @param bucket
	 */
	public @Nonnull ChartBuilder category(@Nonnull String binding, @Nullable Bucket bucket) {
		categoryBinding = binding;
		categoryBucket = bucket;
		return this;
	}

	/**
	 * Set the value data field.
	 * @param binding
	 */
	public @Nonnull ChartBuilder value(@Nonnull String binding) {
		valueBinding = binding;
		return this;
	}

	/**
	 * Set the value data field with an aggregation function
	 * @param binding
	 * @param function
	 */
	public @Nonnull ChartBuilder value(@Nonnull String binding, @Nullable AggregateFunction function) {
		valueBinding = binding;
		valueFunction = function;
		return this;
	}
	
	/**
	 * Order the results by categories or values, ascending or descending.
	 * Applied after top processing.
	 * @param orderBy
	 * @param sort
	 */
	public @Nonnull ChartBuilder orderBy(@SuppressWarnings("hiding") @Nonnull OrderBy orderBy, @Nullable SortDirection sort) {
		this.orderBy = orderBy;
		orderBySort = sort;
		return this;
	}

	/**
	 * Show the top n categories or values, sorted ascending or descending.
	 * The orderByCategory or orderByValue is applied after the top sort.
	 * @param top
	 * @param orderBy
	 * @param sort
	 * @param includeOthers
	 */
	public @Nonnull ChartBuilder top(@SuppressWarnings("hiding") int top,
										@SuppressWarnings("hiding") @Nonnull OrderBy orderBy,
										@Nonnull SortDirection sort,
										boolean includeOthers) {
		this.top = top;
		topOrderBy = orderBy;
		topSort = sort;
		topOthers = includeOthers;
		return this;
	}
	
	/**
	 * Add a JFreeChart post processor to this chart.
	 * @param fullyQualifiedJFreeChartPostProcessorClassName	The postProcessor to add.
	 */
	public @Nonnull ChartBuilder jFreeChartPostProcessorClassName(@SuppressWarnings("hiding") @Nullable String fullyQualifiedJFreeChartPostProcessorClassName) {
		this.fullyQualifiedJFreeChartPostProcessorClassName = fullyQualifiedJFreeChartPostProcessorClassName;
		return this;
	}

	/**
	 * Add a PrimeFaces Chart post processor to this chart.
	 * @param fullyQualifiedPrimeFacesChartPostProcessorClassName	The postProcessor to add.
	 */
	public @Nonnull ChartBuilder primeFacesChartPostProcessorClassName(@SuppressWarnings("hiding") @Nullable String fullyQualifiedPrimeFacesChartPostProcessorClassName) {
		this.fullyQualifiedPrimeFacesChartPostProcessorClassName = fullyQualifiedPrimeFacesChartPostProcessorClassName;
		return this;
	}

	/**
	 * Build the ChartData.
	 * @param label	The data set label.
	 * @return	The ChartData.
	 */
	public @Nonnull ChartData build(@Nullable String label) {
		return build(null, label);
	}

	/**
	 * Build the ChartData.
	 * @param title The chart title.
	 * @param label	The data set label.
	 * @return	The ChartData.
	 */
	public @Nonnull ChartData build(@Nullable String title, @Nullable String label) {
		return build(new RainbowColourSeries(), new RainbowColourSeries(), title, label);
	}
	
	/**
	 * Build the ChartData using defined ColourSeries.
	 * @param backgroundColours
	 * @param borderColours
	 * @param label
	 * @return	The ChartData.
	 */
	public @Nonnull ChartData build(@Nonnull ColourSeries backgroundColours,
									@Nonnull ColourSeries borderColours,
									@Nullable String label) {
		return build(backgroundColours, borderColours, null, label);
	}
	
	/**
	 * Build the ChartData using defined ColourSeries.
	 * @param backgroundColours
	 * @param borderColours
	 * @param title
	 * @param label
	 * @return	The ChartData.
	 */
	public @Nonnull ChartData build(@Nonnull ColourSeries backgroundColours,
									@Nonnull ColourSeries borderColours,
									@Nullable String title,
									@Nullable String label) {
		List<Bean> data = query();
		backgroundColours.setSize(data.size());
		borderColours.setSize(data.size());
		
		ChartData result = new ChartData();
		result.setTitle(title);
		result.setLabel(label);
		result.setBackground(backgroundColours.getCurrent(200));
		result.setBorder(borderColours.getCurrent());
		Customer c = CORE.getCustomer();
		result.setLabels(data.stream().map(r -> (categoryBucket == null) ?
													label(Binder.getDisplay(c, r, categoryBinding)) :
													label(categoryBucket.label(Binder.get(r, CATEGORY_ALIAS)))).collect(Collectors.toList()));
		result.setValues(data.stream().map(r -> (Number) Binder.get(r, VALUE_ALIAS)).collect(Collectors.toList()));

		result.setBackgrounds(backgroundColours.list(200));
		result.setBorders(borderColours.list());
		result.setJFreeChartPostProcessorClassName(fullyQualifiedJFreeChartPostProcessorClassName);
		result.setPrimeFacesChartPostProcessorClassName(fullyQualifiedPrimeFacesChartPostProcessorClassName);
		return result;
	}
	
	static @Nonnull String label(@Nullable String value) {
		if ((value == null) || value.isEmpty()) {
			return "Others";
		}
		return value;
	}
	
	@SuppressWarnings("java:S3776") // Complexity OK
	private @Nonnull List<Bean> query() {
		String categoryExpression = null;
		String categoryAlias = CATEGORY_ALIAS;
		String valueExpression = (valueFunction == null) ? 
									"bean." + valueBinding : 
									valueFunction + "(bean." + valueBinding + ")";
		if (categoryBucket == null) {
			categoryExpression = "bean." + categoryBinding;
			categoryAlias = categoryBinding.replace('.', '_'); // So we get display values
		}
		else {
			categoryExpression = categoryBucket.bizQLExpression(categoryBinding);
		}

		query.addExpressionProjection(categoryExpression, categoryAlias);
		query.addExpressionProjection(valueExpression, VALUE_ALIAS);
		query.addExpressionGrouping(categoryExpression);
		
		if (top > 0) {
			query.addExpressionOrdering(OrderBy.category.equals(topOrderBy) ? categoryExpression : valueExpression, topSort);
		}
		else {
			if (orderBySort == null) {
				query.addExpressionOrdering(OrderBy.category.equals(orderBy) ? categoryExpression : valueExpression);
			}
			else {
				query.addExpressionOrdering(OrderBy.category.equals(orderBy) ? categoryExpression : valueExpression, orderBySort);
			}
		}

		List<Bean> result = query.projectedResults();

		if (top > 0) {
			// cull the list if its bigger than the top requirement
			if (top < result.size()) {
				if (topOthers) {
					List<Bean> best = new ArrayList<>(result.subList(0, top));
					result = result.subList(top, result.size());
					
					Number rest = null;
					if ((valueFunction == null) || 
							AggregateFunction.Count.equals(valueFunction) ||
							AggregateFunction.Sum.equals(valueFunction)) {
						// sum the rest
						rest = sum(best, result, false);
					}
					else if (AggregateFunction.Avg.equals(valueFunction)) {
						// average the rest
						rest = sum(best, result, true);
					}
					else if (AggregateFunction.Min.equals(valueFunction)) {
						rest = minMax(result, true);
					}
					else if (AggregateFunction.Max.equals(valueFunction)) {
						rest = minMax(result, false);
					}
					
					Map<String, Object> properties = new TreeMap<>();
					properties.put((categoryBucket == null) ? categoryBinding : CATEGORY_ALIAS, null);
					properties.put(VALUE_ALIAS, rest);
					best.add(new DynamicBean(document.getOwningModuleName(), document.getName(), properties));
					result = best;
				}
				else {
					result = result.subList(0, top);
				}
			}
			// Always order here as the top sort was applied on the data store
			OrderingImpl ordering = new OrderingImpl(OrderBy.category.equals(orderBy) ? ((categoryBucket == null) ? categoryBinding : CATEGORY_ALIAS) : VALUE_ALIAS,
														SortDirection.descending.equals(orderBySort) ? SortDirection.descending : SortDirection.ascending);
			Binder.order(result, ordering);
		}
		return result;
	}
	
	private static @Nonnull Number sum(@Nonnull List<Bean> best, @Nonnull List<Bean> beans, boolean avg) {
		double result = 0.0;
		
		Class<?> numberType = null;
		for (Bean bean : best) {
			Number number = (Number) Binder.get(bean, VALUE_ALIAS);
			if (number != null) {
				numberType = number.getClass();
				break;
			}
		}
		
		for (Bean bean : beans) {
			Number number = (Number) Binder.get(bean, VALUE_ALIAS);
			if (number != null) {
				result += number.doubleValue();
			}
		}

		if (avg) {
			result = result / beans.size();
		}
		result = Math.round((result * 100000d) / 100000d);
		
		if (Integer.class.equals(numberType)) {
			return Integer.valueOf((int) result);
		}
		if (Long.class.equals(numberType)) {
			return Long.valueOf((long) result);
		}
		if (Float.class.equals(numberType)) {
			return Float.valueOf((float) result);
		}
		if (BigInteger.class.equals(numberType)) {
			return BigInteger.valueOf((long) result);
		}
		if (BigDecimal.class.equals(numberType)) {
			return BigDecimal.valueOf(result);
		}
		return Double.valueOf(result);
	}
	
	@SuppressWarnings("java:S3776") // Complexity OK
	private static @Nullable Number minMax(@Nonnull List<Bean> beans, boolean min) {
		Number result = null;
		
		for (Bean bean : beans) {
			@SuppressWarnings("unchecked")
			Comparable<Number> number = (Comparable<Number>) Binder.get(bean, VALUE_ALIAS);
			if (number != null) {
				if (result == null) {
					result = (Number) number;
				}
				else {
					if (min) {
						if (number.compareTo(result) < 0) {
							result = (Number) number;
						}
					}
					else {
						if (number.compareTo(result) > 0) {
							result = (Number) number;
						}
					}
				}
			}
		}
		
		return result;
	}
}
