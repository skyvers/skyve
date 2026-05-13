package org.skyve.impl.web.faces.charts.config;

import java.io.IOException;

import org.primefaces.model.charts.ChartModel;
import org.skyve.impl.metadata.view.widget.Chart.ChartType;

public class ChartConfigRenderer {
	private ChartConfigRenderer() {
		// prevent instantiation
	}
	
	public static String config(ChartType type, ChartModel model) throws IOException {
		switch (type) {
		case bar, horizontalBar:
			return new SkyveBarChartRenderer().encodeConfig(model);
		case doughnut:
			return new SkyveDoughnutChartRenderer().encodeConfig(model);
		case line, lineArea:
			return new SkyveLineChartRenderer().encodeConfig(model);
		case pie:
			return new SkyvePieChartRenderer().encodeConfig(model);
		case polarArea:
			return new SkyvePolarAreaChartRenderer().encodeConfig(model);
		case radar:
			return new SkyveRadarChartRenderer().encodeConfig(model);
		default:
			throw new IllegalStateException(type + " is not catered for.");	
		}
	}

	static String spruce(ChartResponseWriter writer) {
		StringBuffer result = writer.getStringWriter().getBuffer();
		result.replace(0, 1, "{"); // replace the starting comma with a curly brace
		result.append('}'); // end the JSON
		return result.toString();
	}
}
