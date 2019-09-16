package org.skyve.metadata.view.model.chart;

import java.awt.Color;
import java.util.List;

public class ChartData {
	private String title;
	private List<Number> values;
	private List<String> labels;
	private List<Color> backgrounds;
	private List<Color> borders;
	private String label;
	private Color background;
	private Color border;
	// Can't be of type ChartPostProcessor coz this may not be a class visible to this class
	private Class<?> postProcessor;

	public String getTitle() {
		return title;
	}
	public void setTitle(String title) {
		this.title = title;
	}

	public List<Number> getValues() {
		return values;
	}
	public void setValues(List<Number> values) {
		this.values = values;
	}
	public List<String> getLabels() {
		return labels;
	}
	public void setLabels(List<String> labels) {
		this.labels = labels;
	}
	public List<Color> getBackgrounds() {
		return backgrounds;
	}
	public void setBackgrounds(List<Color> backgrounds) {
		this.backgrounds = backgrounds;
	}
	public List<Color> getBorders() {
		return borders;
	}
	public void setBorders(List<Color> borders) {
		this.borders = borders;
	}
	public String getLabel() {
		return label;
	}
	public void setLabel(String label) {
		this.label = label;
	}
	public Color getBackground() {
		return background;
	}
	public void setBackground(Color background) {
		this.background = background;
	}
	public Color getBorder() {
		return border;
	}
	public void setBorder(Color border) {
		this.border = border;
	}
	public Class<?> getPostProcessor() {
		return postProcessor;
	}
	public void setPostProcessor(Class<?> postProcessor) {
		this.postProcessor = postProcessor;
	}
}
