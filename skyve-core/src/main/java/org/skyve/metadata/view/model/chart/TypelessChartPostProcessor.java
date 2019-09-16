package org.skyve.metadata.view.model.chart;

public interface TypelessChartPostProcessor<PF extends Object, JF extends Object> {
	void processPrimeFaces(PF model);
	void processJFreeChart(JF chart);
}
