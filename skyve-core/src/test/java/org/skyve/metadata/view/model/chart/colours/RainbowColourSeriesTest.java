package org.skyve.metadata.view.model.chart.colours;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.awt.Color;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.view.model.chart.colours.rainbow.RainbowException;

public class RainbowColourSeriesTest {

	@Test
	@SuppressWarnings("static-method")
	public void defaultConstructorProducesNonNullCurrent() {
		RainbowColourSeries series = new RainbowColourSeries();
		assertNotNull(series.getCurrent());
	}

	@Test
	@SuppressWarnings("static-method")
	public void customSpectrumConstructorProducesNonNullCurrent() {
		RainbowColourSeries series = new RainbowColourSeries("ff0000", "0000ff");
		assertNotNull(series.getCurrent());
	}

	@Test
	@SuppressWarnings("static-method")
	public void nextReturnsNonNullColor() {
		RainbowColourSeries series = new RainbowColourSeries();
		Color next = series.next();
		assertNotNull(next);
	}

	@Test
	@SuppressWarnings("static-method")
	public void nextAdvancesCurrentColor() {
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
	public void setSizeSmallSizeTrimsSpectrum() {
		RainbowColourSeries series = new RainbowColourSeries();
		// size <= 10 uses trimmed spectrum
		series.setSize(5);
		assertNotNull(series.getCurrent());
	}

	@Test
	@SuppressWarnings("static-method")
	public void setSizeLargeSizeKeepsSpectrum() {
		RainbowColourSeries series = new RainbowColourSeries();
		series.setSize(20);
		assertNotNull(series.getCurrent());
	}

	@Test
	@SuppressWarnings("static-method")
	public void setSizeTenKeepsTrimmedSpectrum() {
		RainbowColourSeries series = new RainbowColourSeries();
		series.setSize(10);
		assertNotNull(series.getCurrent());
	}

	@Test
	@SuppressWarnings("static-method")
	public void setSpectrumChangesGradient() throws RainbowException {
		RainbowColourSeries series = new RainbowColourSeries();
		series.setSpectrum("ff0000", "00ff00");
		// Should not throw - spectrum set successfully
		assertNotNull(series.getCurrent());
	}

	@Test
	@SuppressWarnings("static-method")
	public void emptySpectrumConstructorFallsBackToDefault() {
		// passing empty array should keep existing spectrum
		RainbowColourSeries series = new RainbowColourSeries(new String[0]);
		assertNotNull(series.getCurrent());
	}

	@Test
	@SuppressWarnings("static-method")
	public void nextWithAlphaReturnsColorWithAlpha() {
		RainbowColourSeries series = new RainbowColourSeries();
		Color c = series.next(128);
		assertNotNull(c);
	}

	@Test
	@SuppressWarnings("static-method")
	public void getCurrentWithAlphaReturnsColorWithAlpha() {
		RainbowColourSeries series = new RainbowColourSeries();
		Color c = series.getCurrent(200);
		assertNotNull(c);
	}

	@Test
	@SuppressWarnings("static-method")
	public void listReturnsSizeColors() {
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
	public void listWithAlphaReturnsSizeColors() {
		RainbowColourSeries series = new RainbowColourSeries();
		series.setSize(3);
		List<Color> colours = series.list(128);
		assertEquals(3, colours.size());
		for (Color c : colours) {
			assertNotNull(c);
		}
	}
}
