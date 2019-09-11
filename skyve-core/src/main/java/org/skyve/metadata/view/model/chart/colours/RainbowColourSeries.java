package org.skyve.metadata.view.model.chart.colours;

import java.awt.Color;

import org.skyve.metadata.view.model.chart.colours.rainbow.Rainbow;
import org.skyve.metadata.view.model.chart.colours.rainbow.RainbowException;

/**
 * Produce a gradient of colours between the default or a specified spectrum (min 2 colours).
 * 
 * @author ben
 */
public class RainbowColourSeries extends ColourSeries {

	private Rainbow rainbow;
	private int currentRainbowIndex = 1;

	public RainbowColourSeries() {
		this.rainbow = new Rainbow();
		rainbow.setSpectrum("ff6385", "ffa040", "ffcc56", "4bc0c0", "36a3eb");
		rainbow.setNumberRange(0, size < 5 ? 5 : size);
		current = getColor(0);
	}

	public RainbowColourSeries(String... spectrum) {
		this();
		if (spectrum.length > 0) {
			rainbow.setSpectrum(spectrum);
		}
		current = getColor(0);
	}

	@Override
	public Color next() {
		current = getColor(++currentRainbowIndex);
		return current;
	}

	@Override
	public void setSize(int size) {
		super.setSize(size);
		rainbow.setNumberRange(0, size < 5 ? 5 : size);
	}

	/**
	 * Sets the spectrum of the Rainbow. By default, the spectrum is a rainbow between the
	 * default chartjs colours. You must have a minimum of two colours, but you can specify
	 * more than two colours.
	 * 
	 * Colours can be in the form "red", "ff0000", or "#ff0000".
	 * For example, <code>rainbowColourSeries.setSpectrum("red", "yellow", "white");</code>
	 * makes the "Rainbow" a colour gradient from red to yellow to white.
	 * 
	 * @param spectrum Two or more Strings representing HTML colours,
	 *        or pass in a whole String array of length 2 or greater
	 * @throws RainbowException if there is less than two arguments or if one of the arguments
	 *         is an invalid colour
	 */
	public void setSpectrum(String... spectrum) throws RainbowException {
		this.rainbow.setSpectrum(spectrum);
	}

	@SuppressWarnings("boxing")
	private Color getColor(int index) {
		String hex = rainbow.colorAt(index);
		return new Color(
				Integer.valueOf(hex.substring(0, 2), 16),
				Integer.valueOf(hex.substring(2, 4), 16),
				Integer.valueOf(hex.substring(4, 6), 16));
	}
}
