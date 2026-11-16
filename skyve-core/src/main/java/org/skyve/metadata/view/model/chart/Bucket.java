package org.skyve.metadata.view.model.chart;

import org.skyve.metadata.SerializableMetaData;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Used to aggregate chart data.
 * It is a definition of how to aggregate the category values.
 * 
 * @author mike
 */
public interface Bucket extends SerializableMetaData {
	/**
	 * Create a BizQL expression representing the bucket.
	 * @param categoryBinding	The category binding.
	 * @return	The expression.
	 */
	public @Nonnull String bizQLExpression(@Nonnull String categoryBinding);

	/**
	 * Obtain a label representing the category returned in the chart data.
	 * @param category	The category value.
	 * @return	The label for display on the chart.
	 */
	public @Nullable String label(@Nullable Object category);
}
