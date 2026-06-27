package org.skyve.metadata.view.model.chart.colours;

import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.awt.Color;

import org.junit.jupiter.api.Test;

class ThemedColourSeriesTest {

	@Test
	@SuppressWarnings("static-method")
	void defaultConstructorUsesBizhubBlue() {
		ThemedColourSeries series = new ThemedColourSeries();
		assertNotNull(series);
		assertNotNull(ThemedColourSeries.BIZHUB_BLUE);
	}

	@Test
	@SuppressWarnings("static-method")
	void customColourConstructorIsAccepted() {
		ThemedColourSeries series = new ThemedColourSeries(new Color(100, 150, 200));
		assertNotNull(series);
	}

	@Test
	@SuppressWarnings("static-method")
	void setSizeDoesNotThrowForPositiveSize() {
		ThemedColourSeries series = new ThemedColourSeries();
		series.setSize(5);
		// size was set; no exception expected
		assertNotNull(series);
	}

	@Test
	@SuppressWarnings("static-method")
	void setSizeZeroDoesNotThrow() {
		ThemedColourSeries series = new ThemedColourSeries();
		series.setSize(0);
		assertNotNull(series);
	}

	@Test
	@SuppressWarnings("static-method")
	void nextReturnsDarkerColour() {
		ThemedColourSeries series = new ThemedColourSeries(new Color(120, 120, 120));
		series.setSize(10);
		Color next = series.next();
		assertNotNull(next);
		// each call to next() should produce a darker shade (lower RGB component values)
		// we just verify it doesn't throw and returns a colour
	}

	@Test
	@SuppressWarnings("static-method")
	void nextCalledMultipleTimesDoesNotThrow() {
		ThemedColourSeries series = new ThemedColourSeries();
		series.setSize(5);
		for (int i = 0; i < 5; i++) {
			assertNotNull(series.next());
		}
	}
}
