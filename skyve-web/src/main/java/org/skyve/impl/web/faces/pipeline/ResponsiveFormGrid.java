package org.skyve.impl.web.faces.pipeline;

import java.io.Serializable;

/**
 * Encapsulates the calculated responsive column style calcs
 * that enable the layout of skyve <form/> edit view definitions 
 * on the fly.
 */
public class ResponsiveFormGrid implements Serializable {
	private static final long serialVersionUID = 4698791205752063327L;

	/**
	 * Immutable class representing the number of responsive columns 
	 * for medium and large displays.
	 * NB Small displays always has 1 column.
	 * Allows addition of styles to cater for colspan.
	 */
	public static class ResponsiveGridStyle implements Serializable {
		private static final long serialVersionUID = -5073864962769492729L;

		private int mediumCols;
		private int largeCols;
		
		public ResponsiveGridStyle(int mediumCols, int largeCols) {
			this.mediumCols = mediumCols;
			this.largeCols = largeCols;
		}

		/**
		 * Immutable addition.
		 * @param other	Other style
		 * @return	A new ResponsiveGridStyle that adds the various columns together.
		 */
		public ResponsiveGridStyle add(ResponsiveGridStyle other) {
			int medium = mediumCols + other.mediumCols;
			int large = largeCols + other.largeCols;
			return new ResponsiveGridStyle(medium, large);
		}
		
		/**
		 * Return the style css required.
		 */
		@Override
		public String toString() {
			return String.format("ui-g-12 ui-md-%s ui-lg-%s", 
									Integer.toString(mediumCols), 
									Integer.toString(largeCols));
		}
	}

	// The current column to be filled in the next getStyle() call.
	private int currentColumn = 0;
	// The column styles derived from the edit view markup.
	private ResponsiveGridStyle[] columnStyles;
	
	public ResponsiveFormGrid(ResponsiveGridStyle[] columnStyles) {
		this.columnStyles = columnStyles;
	}
	
	/**
	 * Get the next style.
	 * @param colspan	The colspan required to produce the next style.
	 */
	public String getStyle(int colspan) {
		// reset the current column index if it will be out of bounds
		if (currentColumn > (columnStyles.length - 1)) {
			currentColumn = 0;
		}
		// Get this column style
		ResponsiveGridStyle result = columnStyles[currentColumn++];
		// Add all the next colspan column styles
		for (int i = 1, l = colspan; i < l; i++) {
			if (currentColumn > (columnStyles.length - 1)) {
				currentColumn = 0;
			}
			result = result.add(columnStyles[currentColumn++]);
		}

		return result.toString();
	}
	
	/**
	 * Reset the style.
	 */
	public void reset() {
		currentColumn = 0;
	}
}
