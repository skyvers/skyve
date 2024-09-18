package modules.admin.Country;

import java.util.Comparator;
import java.util.List;
import java.util.Locale;
import java.util.stream.Collectors;

import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.util.Util;

import modules.admin.domain.Country;

/**
 * Extends the document to create domain values and create Countries from its code.
 */
public class CountryExtension extends Country {
	private static final long serialVersionUID = 1512671790867040617L;

	private static final String UNKNOWN_COUNTRY_NAME = "Unknown";
	
	/**
	 * Return the country for an ISO 2 letter country code localised to the current user's locale.
	 * @param countryCode	The ISO 2 letter country code.
	 * @return	The localised country.
	 */
	public static CountryExtension fromCode(String countryCode) {
		CountryExtension result = Country.newInstance();
		result.setBizId(countryCode);
		String name = Util.countryNameFromCode(countryCode);
		result.setName((name == null) ? UNKNOWN_COUNTRY_NAME : name);
		return result;
	}
	
	/**
	 * Get the list of countries, localised to the current user's locale as domain values.
	 * @return	The domain values
	 */
	public static List<DomainValue> getCountries() {
		return Locale.getISOCountries(Locale.IsoCountryCode.PART1_ALPHA2)
						.stream()
						.map(code -> {
							String name = Util.countryNameFromCode(code);
							return new DomainValue(code, (name == null) ? UNKNOWN_COUNTRY_NAME : name);
						})
						.sorted(Comparator.comparing(DomainValue::getLocalisedDescription))
						.collect(Collectors.toList());
	}
	
	/**
	 * Return the ISO 2 letter country code for this country.
	 */
	public String getCode() {
		return getBizId();
	}
}
