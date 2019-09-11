package org.skyve.metadata.view.model.chart.colours;

import java.awt.Color;

/**
 * Produce darker shades of the base colour given.
 * 
 * @author mike
 */
public class ThemedColourSeries extends ColourSeries {
	public static final Color BIZHUB_BLUE = new Color(70, 130, 180);

	private int redDiff;
	private int greenDiff;
	private int blueDiff;

	public ThemedColourSeries() {
		this(BIZHUB_BLUE);
	}
	
	public ThemedColourSeries(Color baseColour) {
		this.current = baseColour;
	}
	
	@Override
	public void setSize(int size) {
		super.setSize(size);
		if (size > 0) {
			this.redDiff = (current.getRed() / 2) / size;
			this.greenDiff = (current.getGreen() / 2) / size;
			this.blueDiff = (current.getBlue() / 2) / size;
		}
	}
	

	@Override
	public Color next() {
		current = new Color(current.getRed() - redDiff,
								current.getGreen() - greenDiff,
								current.getBlue() - blueDiff);
		return current;
	}
}
