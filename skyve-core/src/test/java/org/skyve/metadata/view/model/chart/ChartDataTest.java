package org.skyve.metadata.view.model.chart;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.awt.Color;
import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.Test;

class ChartDataTest {

	@Test
	@SuppressWarnings("static-method")
	void defaultConstructorHasNullFields() {
		ChartData data = new ChartData();
		assertNull(data.getTitle());
		assertNull(data.getValues());
		assertNull(data.getLabels());
		assertNull(data.getBackgrounds());
		assertNull(data.getBorders());
		assertNull(data.getLabel());
		assertNull(data.getBackground());
		assertNull(data.getBorder());
		assertNull(data.getJFreeChartPostProcessorClassName());
		assertNull(data.getPrimeFacesChartPostProcessorClassName());
	}

	@Test
	@SuppressWarnings("static-method")
	void titleRoundtrip() {
		ChartData data = new ChartData();
		data.setTitle("My Chart");
		assertThat(data.getTitle(), is("My Chart"));
	}

	@Test
	@SuppressWarnings("static-method")
	void valuesRoundtrip() {
		ChartData data = new ChartData();
		List<Number> values = Arrays.asList(Integer.valueOf(1), Integer.valueOf(2), Integer.valueOf(3));
		data.setValues(values);
		assertThat(data.getValues(), is(values));
	}

	@Test
	@SuppressWarnings("static-method")
	void labelsRoundtrip() {
		ChartData data = new ChartData();
		List<String> labels = Arrays.asList("a", "b", "c");
		data.setLabels(labels);
		assertThat(data.getLabels(), is(labels));
	}

	@Test
	@SuppressWarnings("static-method")
	void backgroundsRoundtrip() {
		ChartData data = new ChartData();
		List<Color> backgrounds = Arrays.asList(Color.RED, Color.BLUE);
		data.setBackgrounds(backgrounds);
		assertThat(data.getBackgrounds(), is(backgrounds));
	}

	@Test
	@SuppressWarnings("static-method")
	void bordersRoundtrip() {
		ChartData data = new ChartData();
		List<Color> borders = Arrays.asList(Color.GREEN, Color.BLACK);
		data.setBorders(borders);
		assertThat(data.getBorders(), is(borders));
	}

	@Test
	@SuppressWarnings("static-method")
	void labelRoundtrip() {
		ChartData data = new ChartData();
		data.setLabel("Series 1");
		assertThat(data.getLabel(), is("Series 1"));
	}

	@Test
	@SuppressWarnings("static-method")
	void backgroundColorRoundtrip() {
		ChartData data = new ChartData();
		data.setBackground(Color.CYAN);
		assertThat(data.getBackground(), is(Color.CYAN));
	}

	@Test
	@SuppressWarnings("static-method")
	void borderColorRoundtrip() {
		ChartData data = new ChartData();
		data.setBorder(Color.ORANGE);
		assertThat(data.getBorder(), is(Color.ORANGE));
	}

	@Test
	@SuppressWarnings("static-method")
	void jFreeChartPostProcessorClassNameRoundtrip() {
		ChartData data = new ChartData();
		data.setJFreeChartPostProcessorClassName("com.example.MyPostProcessor");
		assertThat(data.getJFreeChartPostProcessorClassName(), is("com.example.MyPostProcessor"));
	}

	@Test
	@SuppressWarnings("static-method")
	void primeFacesChartPostProcessorClassNameRoundtrip() {
		ChartData data = new ChartData();
		data.setPrimeFacesChartPostProcessorClassName("com.example.PrimeFacesPostProcessor");
		assertThat(data.getPrimeFacesChartPostProcessorClassName(), is("com.example.PrimeFacesPostProcessor"));
	}
}
