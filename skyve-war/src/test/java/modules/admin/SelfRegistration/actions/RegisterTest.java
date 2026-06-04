package modules.admin.SelfRegistration.actions;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.concurrent.CopyOnWriteArraySet;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.WebContainer;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.util.GeoIPService;
import org.skyve.util.IPGeolocation;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;

import modules.admin.Contact.ContactExtension;
import modules.admin.SelfRegistration.SelfRegistrationExtension;
import modules.admin.User.UserExtension;
import util.AbstractH2Test;

/**
 * Tests simple self-registration action branches.
 */
@SuppressWarnings("static-method")
class RegisterTest extends AbstractH2Test {
	@AfterEach
	void clearRequestContext() {
		WebContainer.clear();
		UtilImpl.GEO_IP_COUNTRY_CODES = null;
		UtilImpl.GEO_IP_WHITELIST = false;
	}

	@Test
	void executeWithNoUserReturnsBean() throws Exception {
		SelfRegistrationExtension bean = new SelfRegistrationExtension();

		ServerSideActionResult<SelfRegistrationExtension> result = new Register().execute(bean, null);

		assertNotNull(result);
		assertSame(bean, result.getBean());
	}

	@Test
	void executeWithUserButNoContactReturnsBean() throws Exception {
		SelfRegistrationExtension bean = new SelfRegistrationExtension();
		bean.setUser(new UserExtension());

		ServerSideActionResult<SelfRegistrationExtension> result = new Register().execute(bean, null);

		assertNotNull(result);
		assertSame(bean, result.getBean());
	}

	@Test
	void executeWithMissingConfirmEmailThrowsValidationException() {
		SelfRegistrationExtension bean = registrationWithUserAndContact("person@example.com", null);
		Register register = registerWithGeoIpNotBlocking();

		assertThrows(ValidationException.class, () -> register.execute(bean, null));
	}

	@Test
	void executeWithMismatchedConfirmEmailThrowsValidationException() {
		SelfRegistrationExtension bean = registrationWithUserAndContact("person@example.com", "other@example.com");
		Register register = registerWithGeoIpNotBlocking();

		assertThrows(ValidationException.class, () -> register.execute(bean, null));
	}

	@Test
	void executeWithBlockedGeoIpPassesSilently() throws Exception {
		UtilImpl.GEO_IP_COUNTRY_CODES = new CopyOnWriteArraySet<>(java.util.List.of("AU"));
		SelfRegistrationExtension bean = registrationWithUserAndContact("person@example.com", "person@example.com");
		bean.getUser().getContact().setName("Blocked Person");
		MockHttpServletRequest request = new MockHttpServletRequest();
		request.setRemoteAddr("203.0.113.7");
		WebContainer.setHttpServletRequestResponse(request, new MockHttpServletResponse());
		GeoIPService geoIPService = mock(GeoIPService.class);
		doReturn(Boolean.TRUE).when(geoIPService).isBlocking();
		when(geoIPService.geolocate("203.0.113.7")).thenReturn(new IPGeolocation(null, null, "AU", null));
		Register register = registerWithGeoIpService(geoIPService);

		ServerSideActionResult<SelfRegistrationExtension> result = register.execute(bean, null);

		assertSame(bean, result.getBean());
		assertSame(Boolean.TRUE, bean.getPassSilently());
	}

	private static SelfRegistrationExtension registrationWithUserAndContact(String email, String confirmEmail) {
		SelfRegistrationExtension bean = new SelfRegistrationExtension();
		UserExtension user = new UserExtension();
		ContactExtension contact = new ContactExtension();
		contact.setEmail1(email);
		user.setContact(contact);
		bean.setUser(user);
		bean.setConfirmEmail(confirmEmail);
		return bean;
	}

	private static Register registerWithGeoIpNotBlocking() {
		GeoIPService geoIPService = mock(GeoIPService.class);
		doReturn(Boolean.FALSE).when(geoIPService).isBlocking();

		return registerWithGeoIpService(geoIPService);
	}

	private static Register registerWithGeoIpService(GeoIPService geoIPService) {
		Register register = new Register();
		try {
			Field field = Register.class.getDeclaredField("geoIPService");
			field.setAccessible(true);
			field.set(register, geoIPService);
		} catch (ReflectiveOperationException e) {
			throw new IllegalStateException("Failed to inject GeoIPService", e);
		}
		return register;
	}
}
