package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.model.chart.ChartBuilderMetaData;
import org.skyve.impl.metadata.view.widget.Chart;
import org.skyve.impl.metadata.view.widget.Chart.ChartType;

/**
 * Builds {@link Chart} widget metadata using a fluent API.
 */
public class FluentChart extends FluentWidget implements FluentRelativeSize<FluentChart> {
	private Chart chart = null;

	/**
	 * Creates a fluent builder backed by a new {@link Chart} metadata instance.
	 */
	public FluentChart() {
		chart = new Chart();
	}

	/**
	 * Creates a fluent builder backed by the supplied chart metadata instance.
	 *
	 * @param chart the metadata instance to mutate
	 */
	public FluentChart(Chart chart) {
		this.chart = chart;
	}

	/**
	 * Copies chart metadata into this fluent builder.
	 */
	public FluentChart from(@SuppressWarnings("hiding") Chart chart) {

		type(chart.getType());
		modelName(chart.getModelName());
		model(chart.getModel());
		invisibleConditionName(chart.getInvisibleConditionName());

		relativeSize(chart, this);

		return this;
	}

	/**
	 * Sets the chart type used when rendering this widget.
	 *
	 * @param type the chart type
	 * @return this builder
	 */
	public FluentChart type(ChartType type) {
		chart.setType(type);
		return this;
	}

	/**
	 * Sets the backing model name resolved for this chart.
	 *
	 * @param modelName the chart model name
	 * @return this builder
	 */
	public FluentChart modelName(String modelName) {
		chart.setModelName(modelName);
		return this;
	}

	/**
	 * Sets the condition name used to hide this chart when it evaluates to true.
	 *
	 * @param invisibleConditionName the invisibility condition name
	 * @return this builder
	 */
	public FluentChart invisibleConditionName(String invisibleConditionName) {
		chart.setInvisibleConditionName(invisibleConditionName);
		return this;
	}

	/**
	 * Sets the chart builder metadata used to render chart values.
	 *
	 * @param model the chart builder metadata
	 * @return this builder
	 */
	public FluentChart model(ChartBuilderMetaData model) {
		chart.setModel(model);
		return this;
	}

	/**
	 * Sets the absolute pixel height for this chart widget.
	 *
	 * @param height the height in pixels
	 * @return this builder
	 */
	@Override
	public FluentChart pixelHeight(int height) {
		chart.setPixelHeight(Integer.valueOf(height));
		return this;
	}

	/**
	 * Sets the absolute pixel width for this chart widget.
	 *
	 * @param width the width in pixels
	 * @return this builder
	 */
	@Override
	public FluentChart pixelWidth(int width) {
		chart.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	/**
	 * Sets the minimum allowed pixel width for this chart widget.
	 *
	 * @param minPixelWidth the minimum width in pixels
	 * @return this builder
	 */
	@Override
	public FluentChart minPixelWidth(int minPixelWidth) {
		chart.setMinPixelWidth(Integer.valueOf(minPixelWidth));
		return this;
	}

	/**
	 * Sets the maximum allowed pixel width for this chart widget.
	 *
	 * @param maxPixelWidth the maximum width in pixels
	 * @return this builder
	 */
	@Override
	public FluentChart maxPixelWidth(int maxPixelWidth) {
		chart.setMaxPixelWidth(Integer.valueOf(maxPixelWidth));
		return this;
	}

	/**
	 * Sets the maximum allowed pixel height for this chart widget.
	 *
	 * @param maxPixelHeight the maximum height in pixels
	 * @return this builder
	 */
	@Override
	public FluentChart maxPixelHeight(int maxPixelHeight) {
		chart.setMaxPixelHeight(Integer.valueOf(maxPixelHeight));
		return this;
	}

	/**
	 * Sets the minimum allowed pixel height for this chart widget.
	 *
	 * @param minPixelHeight the minimum height in pixels
	 * @return this builder
	 */
	@Override
	public FluentChart minPixelHeight(int minPixelHeight) {
		chart.setMinPixelHeight(Integer.valueOf(minPixelHeight));
		return this;
	}

	/**
	 * Sets the percentage width for this chart widget.
	 *
	 * @param percentageWidth the width percentage
	 * @return this builder
	 */
	@Override
	public FluentChart percentageWidth(int percentageWidth) {
		chart.setPercentageWidth(Integer.valueOf(percentageWidth));
		return this;
	}

	/**
	 * Sets the percentage height for this chart widget.
	 *
	 * @param percentageHeight the height percentage
	 * @return this builder
	 */
	@Override
	public FluentChart percentageHeight(int percentageHeight) {
		chart.setPercentageHeight(Integer.valueOf(percentageHeight));
		return this;
	}

	/**
	 * Sets the responsive width value used by the renderer grid model.
	 *
	 * @param responsiveWidth the responsive width value
	 * @return this builder
	 */
	@Override
	public FluentChart responsiveWidth(int responsiveWidth) {
		chart.setResponsiveWidth(Integer.valueOf(responsiveWidth));
		return this;
	}

	/**
	 * Sets the small breakpoint width value.
	 *
	 * @param sm the small breakpoint width
	 * @return this builder
	 */
	@Override
	public FluentChart sm(int sm) {
		chart.setSm(Integer.valueOf(sm));
		return this;
	}

	/**
	 * Sets the medium breakpoint width value.
	 *
	 * @param md the medium breakpoint width
	 * @return this builder
	 */
	@Override
	public FluentChart md(int md) {
		chart.setMd(Integer.valueOf(md));
		return this;
	}

	/**
	 * Sets the large breakpoint width value.
	 *
	 * @param lg the large breakpoint width
	 * @return this builder
	 */
	@Override
	public FluentChart lg(int lg) {
		chart.setLg(Integer.valueOf(lg));
		return this;
	}

	/**
	 * Sets the extra-large breakpoint width value.
	 *
	 * @param xl the extra-large breakpoint width
	 * @return this builder
	 */
	@Override
	public FluentChart xl(int xl) {
		chart.setXl(Integer.valueOf(xl));
		return this;
	}

	/**
	 * Returns the wrapped chart metadata instance.
	 *
	 * @return the mutable chart metadata being configured
	 */
	@Override
	public Chart get() {
		return chart;
	}

}
