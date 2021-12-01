package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.Chart;

public class FluentChart extends FluentWidget {
	private Chart chart = null;
	
	public FluentChart() {
		chart = new Chart();
	}
	
	public FluentChart(Chart chart) {
		this.chart = chart;
	}
	
	public FluentChart from(@SuppressWarnings("hiding") Chart chart) {
		return this;
	}
	
	@Override
	public Chart get() {
		return chart;
	}
}
