package org.skyve.impl.web.faces.charts;

import org.primefaces.model.charts.ChartModel;

import jakarta.annotation.Nonnull;

/**
 * Implements internal web-module behavior for this Skyve runtime concern.
 */
public interface PrimeFacesChartPostProcessor <T extends ChartModel> {
	public void process(@Nonnull T model);
}
