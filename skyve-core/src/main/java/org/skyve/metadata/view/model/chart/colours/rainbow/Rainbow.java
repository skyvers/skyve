// (MIT License)
// Copyright (c) 2012 Sophiah (Zing-Ming)
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
// The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

package org.skyve.metadata.view.model.chart.colours.rainbow;

import java.util.ArrayList;
import java.util.Hashtable;

/**
 * The Rainbow class by default maps the range 0 to 100 (inclusive) to the colours of the rainbow
 * (i.e., a gradient transitioning from red to yellow to lime to blue)
 * 
 * @author Sophiah (Zing-Ming)
 */
public class Rainbow {
	
	private double minNum;
	private double maxNum;
	private String[] colours;
	private ArrayList<ColourGradient> colourGradients;	
	
	/**
	 * Constructor. By default, the number range is from 0 to 100, and the spectrum is a rainbow.
	 */
	public Rainbow() {
		minNum = 0;
		maxNum = 100;
		colours = new String[] { "red", "yellow", "lime", "blue" };
		setSpectrum(colours);
	}
	
	/**
	 * Returns the hex colour corresponding to the number. If number is out of range, 
	 * it returns the appropriate hex colour corresponding to either the minNumber or maxNumber.
	 * @param number The number for which you want to find the corresponding colour
	 * @return The corresponding colour represented as a HTML RGB hexidecimal String
	 */
	public String colourAt(double number) {
		if (colourGradients.size() == 1) {
			return colourGradients.get(0).colourAt(number);
		}

		double segment = (maxNum - minNum) / (colourGradients.size());
		int index = (int) Math.min(Math.floor((Math.max(number, minNum) - minNum) / segment), colourGradients.size() - 1);
		return colourGradients.get(index).colourAt(number);
	}	
	
	/**
	 * Sets the spectrum of the Rainbow object. By default, the spectrum is a rainbow.
	 * You must have a minimum of two colours, but you can specify more than two colours.
	 * Colours can be in the form "red", "ff0000", or "#ff0000".
	 * For example, <code>rainbow.setSpectrum("red", "yellow", "white");</code>
	 * makes the "Rainbow" a colour gradient from red to yellow to white.
	 * 
	 * @param spectrum Two or more Strings representing HTML colours,
	 *        or pass in a whole String array of length 2 or greater
	 * @throws RainbowException if there is less than two arguments or if one of the arguments
	 *         is an invalid colour
	 */
	public void setSpectrum(String... spectrum) throws RainbowException {
		try {
			if (spectrum.length < 2) {
				throw new RainbowException("Rainbow must have two or more colours.");
			}

			double increment = (maxNum - minNum) / (spectrum.length - 1);
			ColourGradient firstGradient = new ColourGradient();
			firstGradient.setGradient(spectrum[0], spectrum[1]);
			firstGradient.setNumberRange(minNum, minNum + increment);

			colourGradients = new ArrayList<>();
			colourGradients.add(firstGradient);

			for (int i = 1; i < spectrum.length - 1; i++) {
				ColourGradient colourGradient = new ColourGradient();
				colourGradient.setGradient(spectrum[i], spectrum[i + 1]);
				colourGradient.setNumberRange(minNum + increment * i, minNum + increment * (i + 1));
				colourGradients.add(colourGradient);
			}

			colours = spectrum;
		}
		// This exception is theoretically impossible, so rethrow as unchecked exception			
		catch (NumberRangeException e) {
			throw new RuntimeException(e);
		}		
	}
	
	/**
	 * Sets the number range of the Rainbow object. By default, it is 0 to 100.
	 * @param minNumber The minimum number of the number range
	 * @param maxNumber The maximum number of the number range
	 * @throws NumberRangeException if minNumber is greater than maxNumber
	 */
	public void setNumberRange(double minNumber, double maxNumber) throws NumberRangeException {
		if (maxNumber > minNumber) {
			minNum = minNumber;
			maxNum = maxNumber;
			setSpectrum(colours);
		} else {
			throw new NumberRangeException(minNumber, maxNumber);
		}
	}	
	
	/**
	 * Same as colourAt(double number)
	 */
	public String colorAt(double number) {
		return colourAt(number);
	}
}

class ColourGradient {
	
	private int[] startColour = {0xff, 0x00, 0x00};
	private int[] endColour = {0x00, 0x00, 0xff};
	private double minNum = 0;
	private double maxNum = 100;
	
	private static Hashtable<String, int[]> htmlColors;

	static {
		htmlColors = new Hashtable<>();
			htmlColors.put("black", new int[]{0x00, 0x00, 0x00});
			htmlColors.put("navy", new int[]{0x00, 0x00, 0x80});		
			htmlColors.put("blue", new int[]{0x00, 0x00, 0xff});			
			htmlColors.put("green", new int[]{0x00, 0x80, 0x00});
			htmlColors.put("teal", new int[]{0x00, 0x80, 0x80});			
			htmlColors.put("lime", new int[]{0x00, 0xff, 0x00});			
			htmlColors.put("aqua", new int[]{0x00, 0xff, 0xff});			
			htmlColors.put("maroon", new int[]{0x80, 0x00, 0x00});			
			htmlColors.put("purple", new int[]{0x80, 0x00, 0x80});				
			htmlColors.put("olive", new int[]{0x80, 0x80, 0x00});			
			htmlColors.put("grey", new int[]{0x80, 0x80, 0x80});
			htmlColors.put("gray", new int[]{0x80, 0x80, 0x80});
			htmlColors.put("silver", new int[]{0xc0, 0xc0, 0xc0});				
			htmlColors.put("red", new int[]{0xff, 0x00, 0x00});			
			htmlColors.put("fuchsia", new int[]{0xff, 0x00, 0xff});		
			htmlColors.put("orange", new int[]{0xff, 0x80, 0x00});				
			htmlColors.put("yellow", new int[]{0xff, 0xff, 0x00});			
			htmlColors.put("white", new int[]{0xff, 0xff, 0xff});				
	}

	public String colourAt(double number) {
		return 	formatHex(calcHex(number, startColour[0], endColour[0]))
			+	formatHex(calcHex(number, startColour[1], endColour[1]))
			+	formatHex(calcHex(number, startColour[2], endColour[2]));
	}
	
	private int calcHex(double number, int channelStart, int channelEnd) {
		double num = number;
		if (num < minNum) {
			num = minNum;
		}
		if (num > maxNum) {
			num = maxNum;
		} 
		double numRange = maxNum - minNum; 
		double cPerUnit = (channelEnd - channelStart)/numRange;
		return (int) Math.round(cPerUnit * (num - minNum) + channelStart);
	}
	
	private static String formatHex(int val) {
		String hex = Integer.toHexString(val);
		if (hex.length() == 1) {
			return '0' + hex;
		}

		return hex;
	}
	
	public void setNumberRange(double minNumber, double maxNumber) throws NumberRangeException{
		if (maxNumber > minNumber) {
			minNum = minNumber;
			maxNum = maxNumber;
		} else {
			throw new NumberRangeException(minNumber, maxNumber);
		}
	}
	
	public void setGradient(String colourStart, String colourEnd) throws RainbowException {
		startColour = getHexColour(colourStart);
		endColour = getHexColour(colourEnd);		
	}
	
	private static int[] getHexColour(String s) throws RainbowException {
		if (s.matches("^#?[0-9a-fA-F]{6}$")){
			return rgbStringToArray(s.replace("#", ""));
		}
		int[] rgbArray = htmlColors.get(s.toLowerCase());
		if (rgbArray == null) {
			throw new RainbowException(String.format("%s is not a valid colour.", s));
		}
		return rgbArray;
	}
	
	private static int[] rgbStringToArray(String s) {
		int red = Integer.parseInt(s.substring(0,2), 16);
		int green = Integer.parseInt(s.substring(2,4), 16);
		int blue = Integer.parseInt(s.substring(4,6), 16);
		return new int[]{red, green, blue};		
	}
}