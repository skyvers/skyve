package modules.admin.Country;

import java.util.Comparator;
import java.util.List;
import java.util.Locale;
import java.util.stream.Collectors;

import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.util.Util;

import jakarta.enterprise.inject.Default;
import modules.admin.domain.Country;

/**
 * This class acts as a service layer to encapsulate domain logic.
 *
 * Add this line to classes that wish to use it: @Inject private transient CountryService countryService;
 */
@Default
public class CountryService {

	private static final String UNKNOWN_COUNTRY_NAME = "Unknown";

	/**
	 * Return the country for an ISO 2 letter country code localised to the current user's locale.
	 * 
	 * @param countryCode The ISO 2 letter country code.
	 * @return The localised country.
	 */
	@SuppressWarnings("static-method")
	public CountryExtension fromCode(String countryCode) {
		CountryExtension result = Country.newInstance();
		result.setBizId(countryCode);
		String name = Util.countryNameFromCode(countryCode);
		result.setName((name == null) ? UNKNOWN_COUNTRY_NAME : name);
		return result;
	}

	/**
	 * Get the list of countries, localised to the current user's locale as domain values.
	 * 
	 * @return The domain values
	 */
	@SuppressWarnings("static-method")
	public List<DomainValue> getCountries() {
		return Locale.getISOCountries(Locale.IsoCountryCode.PART1_ALPHA2)
				.stream()
				.map(code -> {
					String name = Util.countryNameFromCode(code);
					return new DomainValue(code, (name == null) ? UNKNOWN_COUNTRY_NAME : name);
				})
				.sorted(Comparator.comparing(DomainValue::getLocalisedDescription))
				.collect(Collectors.toList());
	}
}
