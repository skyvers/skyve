package org.skyve.metadata.view.model.chart;

import org.skyve.metadata.MetaData;

/**
 * Used to aggregate chart data.
 * It is a definition of how to aggregate the category values.
 * 
 * @author mike
 */
public interface Bucket extends MetaData {
	/**
	 * Create a BizQL expression representing the bucket.
	 * @param categoryBindingOrAlias	The category binding.
	 * @return	The expression.
	 */
	public String bizQLExpression(String categoryBindingOrAlias);

	/**
	 * Obtain a label representing the category returned in the chart data.
	 * @param category	The category value.
	 * @return	The label for display on the chart.
	 */
	public String label(Object category);
}
