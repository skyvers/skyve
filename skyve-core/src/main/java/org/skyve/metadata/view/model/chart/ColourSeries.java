package org.skyve.metadata.view.model.chart;

import java.awt.Color;
import java.util.ArrayList;
import java.util.List;

/**
 * Use to produce a series of colours for the size given.
 * Note that the class is depleted once <code>size</code> colours have been generated.
 * Sub-classes should set the curent colour in either a constructor or in an override of setSize().
 * 
 * @author mike
 */
public abstract class ColourSeries {
	protected int size;
	protected Color current;
	
	/**
	 * Get the current colour in the series.
	 * @return	The current colour.
	 */
	public Color getCurrent() {
		return current;
	}

	/**
	 * Get the current colour in the series with the given alpha.
	 * @param alpha
	 * @return	The current colour.
	 */
	public Color getCurrent(int alpha) {
		return new Color(current.getRed(), current.getGreen(), current.getBlue(), alpha);
	}

	/**
	 * Set the size of the colour series required.
	 * @param size
	 */
	public void setSize(int size) {
		this.size = size;
	}
	
	/**
	 * Sets the current colour to the next colour in the colour series and returns the new current colour.
	 * @return	The new current colour.
	 */
	public abstract Color next();

	/**
	 * Gets the next colour in the colour series with the given alpha.
	 * @param alpha
	 * @return	The next colour.
	 */	 
	public Color next(int alpha) {
		next();
		return getCurrent(alpha);
	}
	
	/**
	 * List all the colours in the series.
	 * @return
	 */
	public List<Color> list() {
		List<Color> result = new ArrayList<>(size);
		for (int i = 0; i < size; i++) {
			result.add(getCurrent());
			next();
		}
		return result;
	}

	/**
	 * List all the colours in the series with the given alpha.
	 * @return
	 */
	public List<Color> list(int alpha) {
		List<Color> result = new ArrayList<>(size);
		for (int i = 0; i < size; i++) {
			result.add(getCurrent(alpha));
			next();
		}
		return result;
	}
}
