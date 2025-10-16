package modules.admin.Country;

import com.google.common.base.MoreObjects;

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

	/**
	 * Returns a string representation of this Country object.
	 * <p>
	 * The returned string contains the country's name and code in a
	 * formatted "ClassName{Name=value, Code=value}" style.
	 * 
	 * @return A string representation of this Country
	 */
	@Override
	public String toString() {
		return MoreObjects.toStringHelper(this)
				.add("Name", getName())
				.add("Code", getCode())
				.toString();
	}
}
