package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.model.chart.ChartBuilderMetaData;
import org.skyve.impl.metadata.view.widget.Chart;
import org.skyve.impl.metadata.view.widget.Chart.ChartType;

public class FluentChart extends FluentWidget implements FluentRelativeSize<FluentChart> {
	private Chart chart = null;

	public FluentChart() {
		chart = new Chart();
	}

	public FluentChart(Chart chart) {
		this.chart = chart;
	}

	public FluentChart from(@SuppressWarnings("hiding") Chart chart) {

		type(chart.getType());
		modelName(chart.getModelName());
		model(chart.getModel());
		invisibleConditionName(chart.getInvisibleConditionName());

		relativeSize(chart, this);

		return this;
	}

	public FluentChart type(ChartType type) {
		chart.setType(type);
		return this;
	}

	public FluentChart modelName(String modelName) {
		chart.setModelName(modelName);
		return this;
	}

	public FluentChart invisibleConditionName(String invisibleConditionName) {
		chart.setInvisibleConditionName(invisibleConditionName);
		return this;
	}

	public FluentChart model(ChartBuilderMetaData model) {
		chart.setModel(model);
		return this;
	}

	@Override
	public FluentChart pixelHeight(int height) {
		chart.setPixelHeight(Integer.valueOf(height));
		return this;
	}

	@Override
	public FluentChart pixelWidth(int width) {
		chart.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	@Override
	public FluentChart minPixelWidth(int minPixelWidth) {
		chart.setMinPixelWidth(Integer.valueOf(minPixelWidth));
		return this;
	}

	@Override
	public FluentChart maxPixelWidth(int maxPixelWidth) {
		chart.setMaxPixelWidth(Integer.valueOf(maxPixelWidth));
		return this;
	}

	@Override
	public FluentChart maxPixelHeight(int maxPixelHeight) {
		chart.setMaxPixelHeight(Integer.valueOf(maxPixelHeight));
		return this;
	}

	@Override
	public FluentChart minPixelHeight(int minPixelHeight) {
		chart.setMinPixelHeight(Integer.valueOf(minPixelHeight));
		return this;
	}

	@Override
	public FluentChart percentageWidth(int percentageWidth) {
		chart.setPercentageWidth(Integer.valueOf(percentageWidth));
		return this;
	}

	@Override
	public FluentChart percentageHeight(int percentageHeight) {
		chart.setPercentageHeight(Integer.valueOf(percentageHeight));
		return this;
	}

	@Override
	public FluentChart responsiveWidth(int responsiveWidth) {
		chart.setResponsiveWidth(Integer.valueOf(responsiveWidth));
		return this;
	}

	@Override
	public FluentChart sm(int sm) {
		chart.setSm(Integer.valueOf(sm));
		return this;
	}

	@Override
	public FluentChart md(int md) {
		chart.setMd(Integer.valueOf(md));
		return this;
	}

	@Override
	public FluentChart lg(int lg) {
		chart.setLg(Integer.valueOf(lg));
		return this;
	}

	@Override
	public FluentChart xl(int xl) {
		chart.setXl(Integer.valueOf(xl));
		return this;
	}

	@Override
	public Chart get() {
		return chart;
	}

}
