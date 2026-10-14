package org.skyve.impl.generate.charts;

import org.jfree.chart.JFreeChart;

import jakarta.annotation.Nonnull;

/**
 * Extension point for post-processing a generated JFreeChart instance before
 * it is rendered.
 *
 * <p>Implementations can customise colours, fonts, legends, or any other
 * JFreeChart property after the default configuration is applied.
 */
public interface JFreeChartPostProcessor {
	public void process(@Nonnull JFreeChart chart);
}
