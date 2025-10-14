package modules.admin.Country;

import modules.admin.domain.Country;

/**
 * Extends the document to create domain values and create Countries from its code.
 */
public class CountryExtension extends Country {
	private static final long serialVersionUID = 1512671790867040617L;

	/**
	 * Return the ISO 2 letter country code for this country.
	 */
	public String getCode() {
		return getBizId();
	}
}
