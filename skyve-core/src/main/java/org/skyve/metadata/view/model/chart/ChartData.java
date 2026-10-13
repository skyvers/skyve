package org.skyve.metadata.view.model.chart;

import java.awt.Color;
import java.util.List;

import jakarta.annotation.Nullable;

public class ChartData {
	private String title;
	private List<Number> values;
	private List<String> labels;
	private List<Color> backgrounds;
	private List<Color> borders;
	private String label;
	private Color background;
	private Color border;
	private String fullyQualifiedJFreeChartPostProcessorClassName;
	private String fullyQualifiedPrimeFacesChartPostProcessorClassName;

	public @Nullable String getTitle() {
		return title;
	}
	public void setTitle(@Nullable String title) {
		this.title = title;
	}

	public @Nullable List<Number> getValues() {
		return values;
	}
	public void setValues(@Nullable List<Number> values) {
		this.values = values;
	}
	public @Nullable List<String> getLabels() {
		return labels;
	}
	public void setLabels(@Nullable List<String> labels) {
		this.labels = labels;
	}
	public @Nullable List<Color> getBackgrounds() {
		return backgrounds;
	}
	public void setBackgrounds(@Nullable List<Color> backgrounds) {
		this.backgrounds = backgrounds;
	}
	public @Nullable List<Color> getBorders() {
		return borders;
	}
	public void setBorders(@Nullable List<Color> borders) {
		this.borders = borders;
	}
	public @Nullable String getLabel() {
		return label;
	}
	public void setLabel(@Nullable String label) {
		this.label = label;
	}
	public @Nullable Color getBackground() {
		return background;
	}
	public void setBackground(@Nullable Color background) {
		this.background = background;
	}
	public @Nullable Color getBorder() {
		return border;
	}
	public void setBorder(@Nullable Color border) {
		this.border = border;
	}
	public @Nullable String getJFreeChartPostProcessorClassName() {
		return fullyQualifiedJFreeChartPostProcessorClassName;
	}
	public void setJFreeChartPostProcessorClassName(@Nullable String fullyQualifiedJFreeChartPostProcessorClassName) {
		this.fullyQualifiedJFreeChartPostProcessorClassName = fullyQualifiedJFreeChartPostProcessorClassName;
	}
	public @Nullable String getPrimeFacesChartPostProcessorClassName() {
		return fullyQualifiedPrimeFacesChartPostProcessorClassName;
	}
	public void setPrimeFacesChartPostProcessorClassName(@Nullable String fullyQualifiedPrimeFacesChartPostProcessorClassName) {
		this.fullyQualifiedPrimeFacesChartPostProcessorClassName = fullyQualifiedPrimeFacesChartPostProcessorClassName;
	}
}
