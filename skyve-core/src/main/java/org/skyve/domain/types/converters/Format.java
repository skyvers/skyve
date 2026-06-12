package org.skyve.domain.types.converters;

import java.text.ParseException;

import javax.swing.text.MaskFormatter;

import org.apache.commons.text.WordUtils;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlType;

/**
 * Applies an input mask and optional text-case transformation to a display value.
 *
 * <p>A {@code Format} is the optional mask/case constraint attached to a {@link Converter}.
 * It uses a simplified mask notation that is translated internally to
 * {@link MaskFormatter} syntax:
 * <ul>
 *   <li>{@code A} — any alphanumeric character (letter or digit)
 *   <li>{@code #} — any digit
 *   <li>{@code L} — any letter (case controlled by {@link TextCase})
 *   <li>all other characters are treated as literals in the mask
 * </ul>
 *
 * <p>The {@link TextCase} enum controls how the resulting string value is cased:
 * {@code upper}, {@code lower}, or {@code capital} (title-case via
 * {@link org.apache.commons.text.WordUtils#capitalize}).
 *
 * @param <T> the value type this format produces and consumes
 */
public class Format<T> {
	/**
	 * Controls the case transformation applied to display strings.
	 */
	@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	@SuppressWarnings("java:S115") // Enum names are metadata XML values.
	public enum TextCase {
		upper, lower, capital;
	}
	
	// A - alphanumeric
	// # - digit
	// L - letter
	private String mask;
	private TextCase textCase;
	private String maskFormatterMask;
	
	/**
	 * Constructs a {@code Format} with a mask and optional text-case transformation.
	 *
	 * @param mask      the mask string using A/#/L notation, or {@code null} for no mask
	 * @param textCase  the desired text-case transformation, or {@code null} for no change
	 */
	public Format(String mask, TextCase textCase) {
		this.mask = mask;
		this.textCase = textCase;
	}
	
	/**
	 * Returns the mask string as declared in converter metadata.
	 *
	 * @return the mask, or {@code null} if no mask was specified
	 */
	public final String getMask() {
		return mask;
	}
	
	/**
	 * Returns the text-case transformation applied to string values.
	 *
	 * @return the text case, or {@code null} if none was specified
	 */
	public final TextCase getTextCase() {
		return textCase;
	}
	
	/**
	 * Parses a display string through this format's mask (if any) and applies the
	 * text-case transformation.
	 *
	 * @param value the display string to parse
	 * @return the formatted value of type {@code T}
	 * @throws ParseException if the value does not match the mask
	 */
	@SuppressWarnings("unchecked")
	public final T fromDisplayValue(String value) throws ParseException {
		MaskFormatter maskFormatter = getMaskFormatter();
		T result = (maskFormatter == null) ? (T) value : (T) maskFormatter.stringToValue(value);
		if (result instanceof String string) {
			result = (T) applyCase(string, textCase);
		}
		return result;
	}

	/**
	 * Formats a domain value through this format's mask (if any) and applies the
	 * text-case transformation.
	 *
	 * @param value the value to format; may be {@code null}, in which case an empty
	 *              string is returned
	 * @return the display string; never {@code null}
	 * @throws ParseException if the mask formatter cannot represent {@code value}
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
