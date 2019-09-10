package org.skyve.metadata.view.model.chart;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.model.document.CollectionImpl.OrderingImpl;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
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
	private SortDirection orderBySort;
	private OrderBy orderBy;
	
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
	 */
	public ChartBuilder top(@SuppressWarnings("hiding") int top,
								@SuppressWarnings("hiding") OrderBy orderBy,
								SortDirection sort) {
		this.top = top;
		topOrderBy = orderBy;
		topSort = sort;
		return this;
	}
	
	/**
	 * Build the ChartData.
	 * @param label	The data set label.
	 * @return	The ChartData.
	 */
	public ChartData build(String label) {
		return build(new ThemedColourSeries(), new ThemedColourSeries(), label);
	}
	
	/**
	 * Build the ChartData using defined ColourSeries.
	 * @param backgroundColours
	 * @param borderColours
	 * @param label
	 * @return	The ChartData.
	 */
	public ChartData build(ColourSeries backgroundColours, ColourSeries borderColours, String label) {
		List<Bean> data = query();
		backgroundColours.setSize(data.size());
		borderColours.setSize(data.size());
		
		ChartData result = new ChartData();
		result.setLabel(label);
		result.setBackground(backgroundColours.getCurrent(200));
		result.setBorder(borderColours.getCurrent());
		Customer c = CORE.getCustomer();
		result.setLabels(data.stream().map(r -> (categoryBucket == null) ?
													Binder.getDisplay(c, r, categoryBindingOrAlias) :
													categoryBucket.label(Binder.get(r, "category"))).collect(Collectors.toList()));
		result.setValues(data.stream().map(r -> (Number) Binder.get(r, "value")).collect(Collectors.toList()));

		result.setBackgrounds(backgroundColours.list(200));
		result.setBorders(borderColours.list());
		return result;
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
				result = new ArrayList<>(result.subList(0, top));
			}
			// Always order here as the top sort was applied on the data store
			OrderingImpl ordering = new OrderingImpl(OrderBy.category.equals(orderBy) ? ((categoryBucket == null) ? categoryBindingOrAlias : "category") : "value",
														SortDirection.descending.equals(orderBySort) ? SortDirection.descending : SortDirection.ascending);
			Binder.sortCollectionByOrdering(result, ordering);
		}
		return result;
	}
}
