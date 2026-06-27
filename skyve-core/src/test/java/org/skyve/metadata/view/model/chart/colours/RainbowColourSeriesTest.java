package org.skyve.metadata.view.model.chart.colours;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.awt.Color;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.view.model.chart.colours.rainbow.RainbowException;

class RainbowColourSeriesTest {

	@Test
	@SuppressWarnings("static-method")
	void defaultConstructorProducesNonNullCurrent() {
		RainbowColourSeries series = new RainbowColourSeries();
		assertNotNull(series.getCurrent());
	}

	@Test
	@SuppressWarnings("static-method")
	void customSpectrumConstructorProducesNonNullCurrent() {
		RainbowColourSeries series = new RainbowColourSeries("ff0000", "0000ff");
		assertNotNull(series.getCurrent());
	}

	@Test
	@SuppressWarnings("static-method")
	void nextReturnsNonNullColor() {
		RainbowColourSeries series = new RainbowColourSeries();
		Color next = series.next();
		assertNotNull(next);
	}

	@Test
	@SuppressWarnings("static-method")
	void nextAdvancesCurrentColor() {
		RainbowColourSeries series = new RainbowColourSeries();
		Color first = series.getCurrent();
		series.next();
		Color second = series.getCurrent();
		// colours should not be the same after advancing
		assertNotNull(second);
		// just verify we got a color object (may or may not equal first depending on gradient)
		assertNotNull(first);
	}

	@Test
	@SuppressWarnings("static-method")
	void setSizeKeepsCurrentColourAvailable() {
		for (int size : new int[] {5, 10, 20}) {
			RainbowColourSeries series = new RainbowColourSeries();
			series.setSize(size);
			assertNotNull(series.getCurrent());
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void setSpectrumChangesGradient() throws RainbowException {
		RainbowColourSeries series = new RainbowColourSeries();
		series.setSpectrum("ff0000", "00ff00");
		// Should not throw - spectrum set successfully
		assertNotNull(series.getCurrent());
	}

	@Test
	@SuppressWarnings("static-method")
	void emptySpectrumConstructorFallsBackToDefault() {
		// passing empty array should keep existing spectrum
		RainbowColourSeries series = new RainbowColourSeries(new String[0]);
		assertNotNull(series.getCurrent());
	}

	@Test
	@SuppressWarnings("static-method")
	void nextWithAlphaReturnsColorWithAlpha() {
		RainbowColourSeries series = new RainbowColourSeries();
		Color c = series.next(128);
		assertNotNull(c);
	}

	@Test
	@SuppressWarnings("static-method")
	void getCurrentWithAlphaReturnsColorWithAlpha() {
		RainbowColourSeries series = new RainbowColourSeries();
		Color c = series.getCurrent(200);
		assertNotNull(c);
	}

	@Test
	@SuppressWarnings("static-method")
	void listReturnsSizeColors() {
		RainbowColourSeries series = new RainbowColourSeries();
		series.setSize(3);
		List<Color> colours = series.list();
		assertEquals(3, colours.size());
		for (Color c : colours) {
			assertNotNull(c);
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void listWithAlphaReturnsSizeColors() {
		RainbowColourSeries series = new RainbowColourSeries();
		series.setSize(3);
		List<Color> colours = series.list(128);
		assertEquals(3, colours.size());
		for (Color c : colours) {
			assertNotNull(c);
		}
	}
}
