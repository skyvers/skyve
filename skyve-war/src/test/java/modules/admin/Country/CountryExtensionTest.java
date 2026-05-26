package modules.admin.Country;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
public class CountryExtensionTest {

	@Test
	void getCodeReturnsBizId() {
		CountryExtension country = new CountryExtension();
		country.setBizId("AU");
		country.setName("Australia");
		// getCode() returns getBizId()
		assertTrue("AU".equals(country.getCode()));
	}

	@Test
	void toStringContainsNameAndCode() {
		CountryExtension country = new CountryExtension();
		country.setBizId("AU");
		country.setName("Australia");
		String s = country.toString();
		assertNotNull(s);
		assertTrue(s.contains("Australia"), "Expected name in toString: " + s);
		assertTrue(s.contains("AU"), "Expected code in toString: " + s);
	}

	@Test
	void toStringWithNullNameDoesNotThrow() {
		CountryExtension country = new CountryExtension();
		country.setBizId("US");
		// name is null - should still return a string
		assertNotNull(country.toString());
	}
}
