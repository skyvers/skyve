package modules.admin.Country;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.model.document.Bizlet.DomainValue;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
class CountryServiceH2Test extends AbstractH2Test {
	@Test
	void fromCodeCreatesCountryWithCodeAndLocalisedName() {
		CountryExtension country = new CountryService().fromCode("AU");

		assertThat(country.getBizId(), is("AU"));
		assertThat(country.getName(), is("Australia"));
	}

	@Test
	void getCountriesReturnsSortedDomainValuesIncludingAustralia() {
		List<DomainValue> countries = new CountryService().getCountries();

		assertThat(countries, is(notNullValue()));
		assertTrue(countries.stream().anyMatch(country -> "AU".equals(country.getCode())));
		assertTrue(countries.stream().anyMatch(country -> "Australia".equals(country.getLocalisedDescription())));
	}
}
