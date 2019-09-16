package org.skyve.metadata.view.model.chart;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.stream.Collectors;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.MapBean;
import org.skyve.impl.metadata.model.document.CollectionImpl.OrderingImpl;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.view.model.chart.colours.ColourSeries;
import org.skyve.metadata.view.model.chart.colours.RainbowColourSeries;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.util.Binder;

/**
 * Generate the ChartData based on a simple declarative method call chain.
 * 
 * @author mike
 */
public class ChartBuilder {
	private Document document;
	private String categoryBindingOrAlias;
	private Bucket categoryBucket;
	private String valueBindingOrAlias;
	private AggregateFunction valueFunction;
	private int top = Integer.MIN_VALUE;
	private SortDirection topSort;
	private OrderBy topOrderBy;
	private boolean topOthers = false;
	private SortDirection orderBySort;
	private OrderBy orderBy;
	private Class<?> postProcessor;
	
	/**
	 * Document builder.
	 * @param moduleName
	 * @param documentName
	 */
	public ChartBuilder with(String moduleName, String documentName) {
		Customer c = CORE.getCustomer();
		Module m = c.getModule(moduleName);
		return with(m.getDocument(c, documentName));
	}
	
	/**
	 * Document Builder
	 * @param document
	 */
	public ChartBuilder with(@SuppressWarnings("hiding") Document document) {
		this.document = document;
		return this;
	}
	
	/**
	 * Set the category data field
	 * @param bindingOrAlias
	 */
	public ChartBuilder category(String bindingOrAlias) {
		categoryBindingOrAlias = bindingOrAlias;
		return this;
	}
	
	/**
	 * Set the category data field with a bucketing function.
	 * @param bindingOrAlias
	 * @param bucket
	 */
	public ChartBuilder category(String bindingOrAlias, Bucket bucket) {
		categoryBindingOrAlias = bindingOrAlias;
		categoryBucket = bucket;
		return this;
	}

	/**
	 * Set the value data field.
	 * @param bindingOrAlias
	 */
	public ChartBuilder value(String bindingOrAlias) {
		valueBindingOrAlias = bindingOrAlias;
		return this;
	}

	/**
	 * Set the value data field with an aggregation function
	 * @param bindingOrAlias
	 * @param function
	 */
	public ChartBuilder value(String bindingOrAlias, AggregateFunction function) {
		valueBindingOrAlias = bindingOrAlias;
		valueFunction = function;
		return this;
	}
	
	/**
	 * Order the results by categories or values, ascending or descending.
	 * Applied after top processing.
	 * @param orderBy
	 * @param sort
	 */
	public ChartBuilder orderBy(@SuppressWarnings("hiding") OrderBy orderBy, SortDirection sort) {
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
	public ChartBuilder top(@SuppressWarnings("hiding") int top,
								@SuppressWarnings("hiding") OrderBy orderBy,
								SortDirection sort,
								boolean includeOthers) {
		this.top = top;
		topOrderBy = orderBy;
		topSort = sort;
		topOthers = includeOthers;
		return this;
	}
	
	/**
	 * Add a post processor to this chart.
	 * @param postProcessor	The postProcessor to add.
	 */
	public ChartBuilder postProcessor(@SuppressWarnings("hiding") Class<?> postProcessor) {
		this.postProcessor = postProcessor;
		return this;
	}
	
	/**
	 * Build the ChartData.
	 * @param label	The data set label.
	 * @return	The ChartData.
	 */
	public ChartData build(String label) {
		return build(null, label);
	}

	/**
	 * Build the ChartData.
	 * @param title The chart title.
	 * @param label	The data set label.
	 * @return	The ChartData.
	 */
	public ChartData build(String title, String label) {
		return build(new RainbowColourSeries(), new RainbowColourSeries(), title, label);
	}
	
	/**
	 * Build the ChartData using defined ColourSeries.
	 * @param backgroundColours
	 * @param borderColours
	 * @param label
	 * @return	The ChartData.
	 */
	public ChartData build(ColourSeries backgroundColours,
							ColourSeries borderColours,
							String label) {
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
	public ChartData build(ColourSeries backgroundColours,
							ColourSeries borderColours,
							String title,
							String label) {
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
													label(Binder.getDisplay(c, r, categoryBindingOrAlias)) :
													label(categoryBucket.label(Binder.get(r, "category")))).collect(Collectors.toList()));
		result.setValues(data.stream().map(r -> (Number) Binder.get(r, "value")).collect(Collectors.toList()));

		result.setBackgrounds(backgroundColours.list(200));
		result.setBorders(borderColours.list());
		result.setPostProcessor(postProcessor);
		return result;
	}
	
	private static String label(String value) {
		if ((value == null) || value.isEmpty()) {
			return "Others";
		}
		return value;
	}
	
	private List<Bean> query() {
		DocumentQuery q = CORE.getPersistence().newDocumentQuery(document);
		String categoryExpression = null;
		String categoryAlias = "category";
		String valueExpression = (valueFunction == null) ? valueBindingOrAlias : valueFunction + "(" + valueBindingOrAlias + ")";
		if (categoryBucket == null) {
			categoryExpression = categoryBindingOrAlias;
			categoryAlias = categoryBindingOrAlias.replace('.', '_'); // So we get display values
		}
		else {
			categoryExpression = categoryBucket.bizQLExpression(categoryBindingOrAlias);
		}

		q.addExpressionProjection(categoryExpression, categoryAlias);
		q.addExpressionProjection(valueExpression, "value");
		q.addExpressionGrouping(categoryExpression);
		
		if (top > 0) {
			q.addExpressionOrdering(OrderBy.category.equals(topOrderBy) ? categoryExpression : valueExpression, topSort);
		}
		else {
			if (orderBySort == null) {
				q.addExpressionOrdering(OrderBy.category.equals(orderBy) ? categoryExpression : valueExpression);
			}
			else {
				q.addExpressionOrdering(OrderBy.category.equals(orderBy) ? categoryExpression : valueExpression, orderBySort);
			}
		}

		List<Bean> result = q.projectedResults();

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
						rest = Double.valueOf(sum(result));
					}
					else if (AggregateFunction.Avg.equals(valueFunction)) {
						// average the rest
						rest = Double.valueOf(sum(result) / result.size());
					}
					else if (AggregateFunction.Min.equals(valueFunction)) {
						rest = minMax(result, true);
					}
					else if (AggregateFunction.Max.equals(valueFunction)) {
						rest = minMax(result, false);
					}
					
					Map<String, Object> properties = new TreeMap<>();
					properties.put((categoryBucket == null) ? categoryBindingOrAlias : "category", null);
					properties.put("value", rest);
					best.add(new MapBean(document.getOwningModuleName(), document.getName(), properties));
					result = best;
				}
				else {
					result = result.subList(0, top);
				}
			}
			// Always order here as the top sort was applied on the data store
			OrderingImpl ordering = new OrderingImpl(OrderBy.category.equals(orderBy) ? ((categoryBucket == null) ? categoryBindingOrAlias : "category") : "value",
														SortDirection.descending.equals(orderBySort) ? SortDirection.descending : SortDirection.ascending);
			Binder.sortCollectionByOrdering(result, ordering);
		}
		return result;
	}
	
	private static double sum(List<Bean> beans) {
		double result = 0.0;
		
		for (Bean bean : beans) {
			Number number = (Number) Binder.get(bean, "value");
			if (number != null) {
				result += number.doubleValue();
			}
		}
		
		return Math.round((result * 100000d) / 100000d);
	}
	
	private static Number minMax(List<Bean> beans, boolean min) {
		Number result = null;
		
		for (Bean bean : beans) {
			@SuppressWarnings("unchecked")
			Comparable<Number> number = (Comparable<Number>) Binder.get(bean, "value");
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
