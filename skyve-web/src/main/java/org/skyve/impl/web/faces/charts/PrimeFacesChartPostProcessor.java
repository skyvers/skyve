package org.skyve.impl.web.faces.charts;

import org.primefaces.model.charts.ChartModel;

/**
 * Implements internal web-module behavior for this Skyve runtime concern.
 */
public interface PrimeFacesChartPostProcessor <T extends ChartModel> {
	public void process(T model);
}
