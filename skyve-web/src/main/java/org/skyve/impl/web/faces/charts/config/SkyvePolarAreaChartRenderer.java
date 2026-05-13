package org.skyve.impl.web.faces.charts.config;

import java.io.IOException;

import org.primefaces.component.polarareachart.PolarAreaChartRenderer;
import org.primefaces.model.charts.ChartModel;
import org.skyve.impl.sail.mock.MockFacesContext;

import jakarta.faces.context.FacesContext;

public class SkyvePolarAreaChartRenderer extends PolarAreaChartRenderer {
	public String encodeConfig(ChartModel model) throws IOException {
		FacesContext fc = new MockFacesContext();
		try (ChartResponseWriter writer = new ChartResponseWriter()) {
			fc.setResponseWriter(writer);
			encodeConfig(fc, model);
			return ChartConfigRenderer.spruce(writer);
		}
	}
}
