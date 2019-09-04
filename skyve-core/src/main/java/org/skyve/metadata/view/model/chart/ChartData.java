package org.skyve.metadata.view.model.chart;

import java.awt.Color;
import java.util.List;

public class ChartData {
	private List<Number> values;
	private List<String> labels;
	private List<Color> colours;
	private List<Color> borders;
	private String label;
	private Color colour;
	private Color border;

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
	public List<Color> getColours() {
		return colours;
	}
	public void setColours(List<Color> colours) {
		this.colours = colours;
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
	public Color getColour() {
		return colour;
	}
	public void setColour(Color colour) {
		this.colour = colour;
	}
	public Color getBorder() {
		return border;
	}
	public void setBorder(Color border) {
		this.border = border;
	}
}
