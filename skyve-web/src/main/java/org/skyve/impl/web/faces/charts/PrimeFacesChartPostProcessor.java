package org.skyve.impl.web.faces.charts;

import org.primefaces.model.charts.ChartModel;

public interface PrimeFacesChartPostProcessor <T extends ChartModel> {
	public void process(T model);
}
