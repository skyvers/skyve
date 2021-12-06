package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.Comparison;

public class FluentComparison extends FluentInputWidget<FluentComparison> implements FluentRelativeSize<FluentComparison> {
	private Comparison comparison = null;

	public FluentComparison() {
		comparison = new Comparison();
	}

	public FluentComparison(Comparison comparison) {
		this.comparison = comparison;
	}

	public FluentComparison from(@SuppressWarnings("hiding") Comparison comparison) {
		relativeSize(comparison, this);
		modelName(comparison.getModelName());
		editable(comparison.getEditable());
		super.from(comparison);
		return this;
	}

	public FluentComparison modelName(String modelName) {
		comparison.setModelName(modelName);
		return this;
	}

	public FluentComparison editable(Boolean editable) {
		comparison.setEditable(editable);
		return this;
	}

	@Override
	public FluentComparison pixelHeight(int height) {
		comparison.setPixelHeight(Integer.valueOf(height));
		return this;
	}

	@Override
	public FluentComparison pixelWidth(int width) {
		comparison.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	@Override
	public FluentComparison minPixelWidth(int minPixelWidth) {
		comparison.setMinPixelWidth(Integer.valueOf(minPixelWidth));
		return this;
	}

	@Override
	public FluentComparison maxPixelWidth(int maxPixelWidth) {
		comparison.setMaxPixelWidth(Integer.valueOf(maxPixelWidth));
		return this;
	}

	@Override
	public FluentComparison maxPixelHeight(int maxPixelHeight) {
		comparison.setMaxPixelHeight(Integer.valueOf(maxPixelHeight));
		return this;
	}

	@Override
	public FluentComparison minPixelHeight(int minPixelHeight) {
		comparison.setMinPixelHeight(Integer.valueOf(minPixelHeight));
		return this;
	}

	@Override
	public FluentComparison percentageWidth(int percentageWidth) {
		comparison.setPercentageWidth(Integer.valueOf(percentageWidth));
		return this;
	}

	@Override
	public FluentComparison percentageHeight(int percentageHeight) {
		comparison.setPercentageHeight(Integer.valueOf(percentageHeight));
		return this;
	}

	@Override
	public FluentComparison responsiveWidth(int responsiveWidth) {
		comparison.setResponsiveWidth(Integer.valueOf(responsiveWidth));
		return this;
	}

	@Override
	public FluentComparison sm(int sm) {
		comparison.setSm(Integer.valueOf(sm));
		return this;
	}

	@Override
	public FluentComparison md(int md) {
		comparison.setMd(Integer.valueOf(md));
		return this;
	}

	@Override
	public FluentComparison lg(int lg) {
		comparison.setLg(Integer.valueOf(lg));
		return this;
	}

	@Override
	public FluentComparison xl(int xl) {
		comparison.setXl(Integer.valueOf(xl));
		return this;
	}

	@Override
	public Comparison get() {
		return comparison;
	}

}
