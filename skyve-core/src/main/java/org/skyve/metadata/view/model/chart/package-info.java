/**
 * Custom chart view model SPI and chart builder API.
 *
 * <p>{@link org.skyve.metadata.view.model.chart.ChartModel} is the primary SPI: subclass it
 * and implement {@code getChartData()} to supply aggregated chart data to a Skyve
 * {@code chart} widget. For query-backed charts without custom Java, use
 * {@link org.skyve.metadata.view.model.chart.ChartBuilder} or
 * {@link org.skyve.metadata.view.model.chart.ProjectedChartBuilder}.
 *
 * <p>Bucketing classes ({@link org.skyve.metadata.view.model.chart.Bucket},
 * {@link org.skyve.metadata.view.model.chart.TemporalBucket},
 * {@link org.skyve.metadata.view.model.chart.NumericRangeBucket}, etc.) control
 * how raw values are grouped into chart categories.
 *
 * @see org.skyve.metadata.view.model.chart.ChartModel
 * @see org.skyve.metadata.view.model.chart.ChartData
 * @see org.skyve.metadata.view.model.chart.ChartBuilder
 */
package org.skyve.metadata.view.model.chart;
