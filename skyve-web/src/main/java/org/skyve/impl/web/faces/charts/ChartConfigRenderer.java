package org.skyve.impl.web.faces.charts;

import java.io.IOException;

import org.primefaces.model.charts.ChartModel;
import org.skyve.impl.metadata.view.widget.Chart.ChartType;

public class ChartConfigRenderer {
	public static String config(ChartType type, ChartModel model) throws IOException {
		switch (type) {
		case bar:
		case horizontalBar:
			return new BarChartRenderer().encodeConfig(model);
		case doughnut:
			return new DoughnutChartRenderer().encodeConfig(model);
		case line:
		case lineArea:
			return new LineChartRenderer().encodeConfig(model);
		case pie:
			return new PieChartRenderer().encodeConfig(model);
		case polarArea:
			return new PolarAreaChartRenderer().encodeConfig(model);
		case radar:
			return new RadarChartRenderer().encodeConfig(model);
		default:
			throw new IllegalStateException(type + " is not catered for.");	
		}
	}

	static String spruce(ResponseWriter writer) {
		StringBuffer result = writer.getStringWriter().getBuffer();
		result.replace(0, 1, "{"); // replace the starting comma with a curly brace
		result.append('}'); // end the JSON
		return result.toString();
	}
}
