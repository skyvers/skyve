package org.skyve.impl.web.faces.charts.config;

import java.io.IOException;

import org.primefaces.model.charts.ChartModel;
import org.skyve.impl.metadata.view.widget.Chart.ChartType;

/**
 * Renders Skyve metadata into JSF or PrimeFaces output for this concern.
 */
public class ChartConfigRenderer {
	/**
	 * Prevents instantiation of this utility class.
	 */
	private ChartConfigRenderer() {
		// prevent instantiation
	}
	
	/**
	 * Encodes chart configuration JSON for the requested chart type and PrimeFaces model.
	 *
	 * @param type the chart type to encode
	 * @param model the PrimeFaces chart model to encode
	 * @return the encoded chart configuration JSON
	 * @throws IOException if the chart model cannot be written
	 */
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

	/**
	 * Converts the writer's comma-prefixed buffer into valid JSON.
	 *
	 * @param writer the chart response writer containing the JSON buffer
	 * @return the rendered JSON string
	 */
	static String spruce(ChartResponseWriter writer) {
		StringBuffer result = writer.getStringWriter().getBuffer();
		result.replace(0, 1, "{"); // replace the starting comma with a curly brace
		result.append('}'); // end the JSON
		return result.toString();
	}
}
