package org.skyve.metadata.view.model.chart.colours.rainbow;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;

class RainbowTest {

	@Test
	@SuppressWarnings("static-method")
	void defaultConstructorCreatesInstance() {
		assertNotNull(new Rainbow());
	}

	@Test
	@SuppressWarnings("static-method")
	void colourAtZeroReturnsRed() {
		// Default spectrum: red → yellow → lime → blue, range 0–100
		// At 0: start of gradient 0 (red → yellow), returns red
		Rainbow r = new Rainbow();
		assertEquals("ff0000", r.colourAt(0));
	}

	@Test
	@SuppressWarnings("static-method")
	void colourAtOneHundredReturnsBlue() {
		// At 100: end of gradient 2 (lime → blue), returns blue
		Rainbow r = new Rainbow();
		assertEquals("0000ff", r.colourAt(100));
	}

	@Test
	@SuppressWarnings("static-method")
	void colorAtDelegatesToColourAt() {
		// colorAt (US spelling) should return same result as colourAt
		Rainbow r = new Rainbow();
		assertEquals(r.colourAt(50), r.colorAt(50));
	}

	@Test
	@SuppressWarnings("static-method")
	void colourAtReturnsSixCharHexString() {
		Rainbow r = new Rainbow();
		// Any value should return a 6-character hex string
		String colour = r.colourAt(50);
		assertNotNull(colour);
		assertEquals(6, colour.length());
		// Must be valid hex
		Integer.parseInt(colour, 16);
	}

	@Test
	@SuppressWarnings("static-method")
	void colourAtBelowMinReturnsMinColour() {
		Rainbow r = new Rainbow();
		// Below min (0) → clamped to min → same as colourAt(0)
		assertEquals(r.colourAt(0), r.colourAt(-10));
	}

	@Test
	@SuppressWarnings("static-method")
	void colourAtAboveMaxReturnsMaxColour() {
		Rainbow r = new Rainbow();
		// Above max (100) → clamped to max → same as colourAt(100)
		assertEquals(r.colourAt(100), r.colourAt(150));
	}

	@Test
	@SuppressWarnings("static-method")
	void setSpectrumTwoColoursChangesSpectrum() {
		Rainbow r = new Rainbow();
		r.setSpectrum("red", "blue");
		// With only red→blue spectrum, at 0 → red
		assertEquals("ff0000", r.colourAt(0));
		// At 100 → blue
		assertEquals("0000ff", r.colourAt(100));
	}

	@Test
	@SuppressWarnings("static-method")
	void setSpectrumWithHexColoursWorks() {
		Rainbow r = new Rainbow();
		r.setSpectrum("ff0000", "0000ff");
		assertEquals("ff0000", r.colourAt(0));
		assertEquals("0000ff", r.colourAt(100));
	}

	@Test
	@SuppressWarnings("static-method")
	void setSpectrumWithOneColourThrowsRainbowException() {
		Rainbow r = new Rainbow();
		assertThrows(RainbowException.class, () -> r.setSpectrum("red"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setSpectrumWithInvalidColourThrowsRainbowException() {
		Rainbow r = new Rainbow();
		assertThrows(RainbowException.class, () -> r.setSpectrum("notacolour", "blue"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setNumberRangeChangesRange() throws NumberRangeException {
		Rainbow r = new Rainbow();
		r.setNumberRange(0, 200);
		// At 0 with new range 0-200 → red
		assertEquals("ff0000", r.colourAt(0));
		// At 200 (max of new range) → blue
		assertEquals("0000ff", r.colourAt(200));
	}

	@Test
	@SuppressWarnings("static-method")
	void setNumberRangeWithInvalidRangeThrowsNumberRangeException() {
		Rainbow r = new Rainbow();
		assertThrows(NumberRangeException.class, () -> r.setNumberRange(50, 10));
	}

	@Test
	@SuppressWarnings("static-method")
	void setNumberRangeWithEqualBoundsThrowsNumberRangeException() {
		Rainbow r = new Rainbow();
		assertThrows(NumberRangeException.class, () -> r.setNumberRange(50, 50));
	}

	@Test
	@SuppressWarnings("static-method")
	void threeColourSpectrumMidpointTest() {
		Rainbow r = new Rainbow();
		r.setSpectrum("red", "white", "blue");
		// At 0 → red
		assertEquals("ff0000", r.colourAt(0));
		// At 50 (midpoint) → white
		assertEquals("ffffff", r.colourAt(50));
		// At 100 → blue
		assertEquals("0000ff", r.colourAt(100));
	}
}
