/**
 * Chart builder metadata for configuring the Skyve chart view widget.
 *
 * <p>This package contains the metadata model for specifying how chart data should be
 * built and presented. {@code ChartBuilderMetaData} is the primary descriptor; it
 * references ordering ({@code ChartBuilderOrderMetaData}), top-N filtering
 * ({@code ChartBuilderTopMetaData}), and a bucketing strategy.
 *
 * <p>Bucketing strategies control how continuous data is grouped into chart categories:
 * <ul>
 *   <li>{@code NoBucketMetaData} — no bucketing; each distinct value is its own bar/slice.
 *   <li>{@code NumericMultipleBucketMetaData} — groups by multiples of a number.
 *   <li>{@code NumericRangeBucketMetaData} — groups into explicit numeric ranges.
 *   <li>{@code NumericRangeMetaData} — defines a single numeric range bound.
 *   <li>{@code TemporalBucketMetaData} — groups date/time values by period (day, month, year, etc.).
 *   <li>{@code TextLengthBucketMetaData} — groups text values by string length.
 *   <li>{@code TextStartsWithBucketMetaData} — groups text values by common prefix.
 * </ul>
 *
 * @see org.skyve.impl.metadata.view.model
 */
package org.skyve.impl.metadata.view.model.chart;
