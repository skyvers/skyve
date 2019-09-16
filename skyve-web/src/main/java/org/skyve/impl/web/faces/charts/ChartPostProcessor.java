package org.skyve.impl.web.faces.charts;

import org.jfree.chart.JFreeChart;
import org.primefaces.model.charts.ChartModel;
import org.skyve.metadata.view.model.chart.TypelessChartPostProcessor;

public interface ChartPostProcessor extends TypelessChartPostProcessor<ChartModel, JFreeChart> {
	@Override
	public void processPrimeFaces(ChartModel model);
	@Override
	public void processJFreeChart(JFreeChart chart);
}
