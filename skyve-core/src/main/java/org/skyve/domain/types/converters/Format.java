package org.skyve.domain.types.converters;

import java.text.ParseException;

import javax.swing.text.MaskFormatter;

import org.apache.commons.text.WordUtils;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlType;

public class Format<T> {
	@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public static enum TextCase {
		upper, lower, capital;
	}
	
	// A - alphanumeric
	// # - digit
	// L - letter
	private String mask;
	private TextCase textCase;
	private String maskFormatterMask;
	
	public Format(String mask, TextCase textCase) {
		this.mask = mask;
		this.textCase = textCase;
	}
	
	public final String getMask() {
		return mask;
	}
	
	public final TextCase getTextCase() {
		return textCase;
	}
	
	/**
	 * @param value
	 * @return
	 * @throws ParseException
	 */
	@SuppressWarnings("unchecked")
	public final T fromDisplayValue(String value) throws ParseException {
		MaskFormatter maskFormatter = getMaskFormatter();
		T result = (maskFormatter == null) ? (T) value : (T) maskFormatter.stringToValue(value);
		if (result instanceof String) {
			result = (T) applyCase((String) result, textCase);
		}
		return result;
	}

	/**
	 * @param value
	 * @return
	 * @throws ParseException
	 */
	public final String toDisplayValue(T value) throws ParseException {
		MaskFormatter maskFormatter = getMaskFormatter();
		String displayValue = (maskFormatter == null) ? 
								((value == null) ? "" : value.toString()) : 
								maskFormatter.valueToString(value);
		return applyCase(displayValue, textCase);
	}

	private static String applyCase(String value, TextCase textCase) {
		String result = value;
		if ((result != null) && (textCase != null)) {
			if (TextCase.lower.equals(textCase)) {
				result = result.toLowerCase();
			}
			else if (TextCase.upper.equals(textCase)) {
				result = result.toUpperCase();
			}
			else { // capital
				result = WordUtils.capitalize(result);
			}
		}
		
		return result;
	}
	
	/*
	 * My spec is
	 * A - alphanumeric
	 * # - digit
	 * L - letter
	 * 
	 * MaskFormatter spec is 
	 * # - Any valid number, uses Character.isDigit. 
	 * ' - Escape character, used to escape any of the special formatting characters. 
	 * U - Any character (Character.isLetter). All lowercase letters are mapped to upper case. 
	 * L - Any character (Character.isLetter). All upper case letters are mapped to lower case. 
	 * A - Any character or number (Character.isLetter or Character.isDigit) 
	 * ? - Any character (Character.isLetter). 
	 * * - Anything. 
	 * H - Any hex character (0-9, a-f or A-F). 
	 * 
	 */
	private MaskFormatter getMaskFormatter() throws ParseException {
		if ((mask != null) && (maskFormatterMask == null)) {
			maskFormatterMask = mask.replace("U", "'U");
			maskFormatterMask = maskFormatterMask.replace("?", "'?");
			maskFormatterMask = maskFormatterMask.replace("*", "'*");
			maskFormatterMask = maskFormatterMask.replace("H", "'H");
			
			if (textCase == null) {
				maskFormatterMask = maskFormatterMask.replace('L', '?'); // my spec has 'L', MaskFormatter is '?' for letter
			}
			else if (TextCase.upper.equals(textCase)) {
				maskFormatterMask = maskFormatterMask.replace('L', 'U'); // my spec has 'L', MaskFormatter for upper letter is 'U'
			}
			else if (TextCase.capital.equals(textCase)) {
				// MaskFormatter upper case is 'U', so replace first 'L', 'L' means lower case so leave them alone
				maskFormatterMask = maskFormatterMask.replaceFirst("L", "U");
			}
			// no need to cater for lower as 'L' means lower letter anyway
		}
		
		if (maskFormatterMask != null) {
			MaskFormatter formatter = new MaskFormatter(maskFormatterMask);
			formatter.setAllowsInvalid(false);
			formatter.setValueContainsLiteralCharacters(true);
			return formatter;
		}
		return null;
	}
}
