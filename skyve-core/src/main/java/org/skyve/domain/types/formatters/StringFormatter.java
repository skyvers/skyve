package org.skyve.domain.types.formatters;

import org.skyve.metadata.view.TextOutput.Sanitisation;
import org.skyve.util.OWASP;

/**
 * Format a String into a temporal representation. 
 */
public class StringFormatter implements Formatter<String> {
	private boolean escapeHTML;
	private boolean escapeJSON;
	private boolean escapeJS;
	private Sanitisation sanitise;
	
	/**
	 * Creates a new StringFormatter instance.
	 * @param escapeHTML the escapeHTML
	 * @param escapeJSON the escapeJSON
	 * @param escapeJS the escapeJS
	 * @param sanitise the sanitise
	 */
	public StringFormatter(boolean escapeHTML, boolean escapeJSON, boolean escapeJS, Sanitisation sanitise) {
		super();
		this.escapeHTML = escapeHTML;
		this.escapeJSON = escapeJSON;
		this.escapeJS = escapeJS;
		this.sanitise = sanitise;
	}
	
	/**
	 * Returns the valueType.
	 * @return the result
	 */
	@Override
	public Class<String> getValueType() {
		return String.class;
	}
	
	/**
	 * Executes toDisplayValue.
	 * @param value the value
	 * @return the result
	 */
	@Override
	public String toDisplayValue(String value) {
		String result = value;

		if (escapeHTML) {
			if (sanitise != null) {
				result = OWASP.sanitiseAndEscapeHtml(sanitise, result);
			}
			else {
				result = OWASP.escapeHtml(result);
			}
		}
		else if (sanitise != null) {
			result = OWASP.sanitise(sanitise, result);
		}
		if (escapeJSON) {
			result = OWASP.escapeJsonString(result);
		}
		if (escapeJS) {
			result = OWASP.escapeJsString(result);
		}
		
		return result;
	}
}
