package modules.admin.User;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.CoreMatchers.startsWith;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.skyve.impl.util.UtilImpl;

/**
 * Unit tests for {@link UserExtension} that do not require the H2 test database.
 */
class UserExtensionUnitTest {

	@Spy
	private UserExtension user;

	private String originalServerUrl;
	private String originalSkyveContext;
	private String originalHomeUri;

	@BeforeEach
	@SuppressWarnings("deprecation")
	void setup() {
		MockitoAnnotations.initMocks(this);
		originalServerUrl = UtilImpl.SERVER_URL;
		originalSkyveContext = UtilImpl.SKYVE_CONTEXT;
		originalHomeUri = UtilImpl.HOME_URI;
	}

	@AfterEach
	void tearDown() {
		UtilImpl.SERVER_URL = originalServerUrl;
		UtilImpl.SKYVE_CONTEXT = originalSkyveContext;
		UtilImpl.HOME_URI = originalHomeUri;
	}

	@Test
	void getActivateUrlReturnsNullWhenActivationCodeIsNull() {
		// setup: no activation code set (defaults to null)
		user.setActivationCode(null);

		// call the method under test
		String result = user.getActivateUrl();

		// verify
		assertNull(result);
	}

	@Test
	void getActivateUrlContainsActivationCodeWhenSet() {
		// setup
		UtilImpl.SERVER_URL = "http://localhost:8080";
		UtilImpl.SKYVE_CONTEXT = "/skyve";
		UtilImpl.HOME_URI = "/";
		user.setActivationCode("test-activation-code-123");

		// call the method under test
		String result = user.getActivateUrl();

		// verify: contains activation code
		assertThat(result, containsString("test-activation-code-123"));
		assertThat(result, containsString("http://localhost:8080"));
	}

	@Test
	void getActivateUrlContainsSelfRegistrationActivationDocument() {
		// setup
		UtilImpl.SERVER_URL = "http://localhost";
		UtilImpl.SKYVE_CONTEXT = "/app";
		UtilImpl.HOME_URI = "/home";
		user.setActivationCode("abc123");

		// call the method under test
		String result = user.getActivateUrl();

		// verify: contains module and document names
		assertThat(result, containsString("SelfRegistrationActivation"));
		assertThat(result, containsString("&code=abc123"));
	}

	@Test
	void getPasswordLastChangedCountryNameReturnsNullWhenCountryCodeIsNull() {
		// setup: passwordLastChangedCountryCode is null by default
		user.setPasswordLastChangedCountryCode(null);

		// call the method under test
		String result = user.getPasswordLastChangedCountryName();

		// verify
		assertThat(result, is(nullValue()));
	}

	@Test
	void bizKeyPrefixesInactiveWhenUserIsInactive() {
		user.setInactive(Boolean.TRUE);
		String result = user.bizKey();
		assertThat(result, startsWith("INACTIVE "));
	}

}
