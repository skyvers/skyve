package org.skyve.domain.types.converters;

import java.text.ParseException;

import javax.swing.text.MaskFormatter;
import javax.xml.bind.annotation.XmlType;

import org.apache.commons.lang3.StringUtils;
import org.skyve.impl.util.XMLUtil;

public class Format<T> {
	@XmlType(namespace = XMLUtil.DOCUMENT_NAMESPACE)
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
	
	/**
	 * @param value
	 * @return
	 * @throws ParseException
	 */
	@SuppressWarnings("unchecked")
	public final T fromDisplayValue(String value) throws ParseException {
		T result = (T) getMaskFormatter().stringToValue(value);
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
		return applyCase(getMaskFormatter().valueToString(value), textCase);
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
				result = StringUtils.capitalize(result);
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
		if (maskFormatterMask == null) {
			if (mask == null) {
				maskFormatterMask = "*";
			}
			else {
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
		}
		
		MaskFormatter formatter = new MaskFormatter(maskFormatterMask);
		formatter.setAllowsInvalid(false);
		formatter.setValueContainsLiteralCharacters(true);
		return formatter;
	}
}
