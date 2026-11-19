package org.skyve.impl.web.spring;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.nio.charset.StandardCharsets;
import java.util.Base64;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.skyve.impl.security.SkyveRememberMeTokenRepository;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.web.authentication.rememberme.PersistentTokenRepository;

import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

class SkyveRememberMeServicesTest {
	private static final String USER = "demo/inspector";

	private SkyveRememberMeTokenRepository repository;
	private SkyveRememberMeServices services;
	private HttpServletRequest request;
	private HttpServletResponse response;
	private final Authentication authentication = new UsernamePasswordAuthenticationToken(USER, "n/a");

	@BeforeEach
	void setUp() {
		repository = mock(SkyveRememberMeTokenRepository.class);
		services = new SkyveRememberMeServices("remember", mock(UserDetailsService.class), repository);
		services.setCookieName("remember");
		request = mock(HttpServletRequest.class);
		when(request.getContextPath()).thenReturn("/skyve");
		response = mock(HttpServletResponse.class);
	}

	private static String cookieValue(String series, String token) {
		return Base64.getEncoder().encodeToString((series + ":" + token).getBytes(StandardCharsets.UTF_8));
	}

	private void hasCookie(String value) {
		when(request.getCookies()).thenReturn(new Cookie[] {new Cookie("remember", value)});
	}

	@Test
	void logoutWithACookieRemovesOnlyThatSeries() {
		hasCookie(cookieValue("pcSeries", "token"));

		services.logout(request, response, authentication);

		verify(repository).removeUserToken(USER, "pcSeries");
		verify(repository, never()).removeUserTokens(anyString());
	}

	@Test
	void logoutCancelsTheCookie() {
		hasCookie(cookieValue("pcSeries", "token"));

		services.logout(request, response, authentication);

		ArgumentCaptor<Cookie> cookie = ArgumentCaptor.forClass(Cookie.class);
		verify(response).addCookie(cookie.capture());
		assertEquals("remember", cookie.getValue().getName());
		assertEquals(0, cookie.getValue().getMaxAge());
	}

	@Test
	void logoutWithoutACookieRemovesNothing() {
		when(request.getCookies()).thenReturn(null);

		services.logout(request, response, authentication);

		verifyNoInteractions(repository);
	}

	@Test
	void logoutWithOtherCookiesOnlyRemovesNothing() {
		when(request.getCookies()).thenReturn(new Cookie[] {new Cookie("JSESSIONID", "x")});

		services.logout(request, response, authentication);

		verifyNoInteractions(repository);
	}

	@Test
	void logoutWithAnInvalidCookieRemovesNothing() {
		hasCookie("not base 64 !");

		services.logout(request, response, authentication);

		verifyNoInteractions(repository);
	}

	@Test
	void logoutWithAnEmptyCookieRemovesNothing() {
		hasCookie("");

		services.logout(request, response, authentication);

		verifyNoInteractions(repository);
	}

	@Test
	void logoutWithACookieOfTheWrongShapeRemovesNothing() {
		hasCookie(Base64.getEncoder().encodeToString("one:two:three".getBytes(StandardCharsets.UTF_8)));

		services.logout(request, response, authentication);

		verifyNoInteractions(repository);
	}

	@Test
	void logoutWhenNotSignedInRemovesNothing() {
		hasCookie(cookieValue("pcSeries", "token"));

		services.logout(request, response, null);

		verifyNoInteractions(repository);
	}

	/** A repository that cannot remove one series is handled as Spring does: all of the user's tokens go. */
	@Test
	void logoutWithAnotherRepositoryRemovesAllTheUsersTokens() {
		PersistentTokenRepository other = mock(PersistentTokenRepository.class);
		SkyveRememberMeServices otherServices = new SkyveRememberMeServices("remember", mock(UserDetailsService.class), other);
		otherServices.setCookieName("remember");
		hasCookie(cookieValue("pcSeries", "token"));

		otherServices.logout(request, response, authentication);

		verify(other).removeUserTokens(USER);
	}

	@Test
	void extractSeriesReadsTheSeriesOfThisBrowsersCookie() {
		hasCookie(cookieValue("pcSeries", "token"));

		assertEquals("pcSeries", SkyveRememberMeServices.extractSeries(request, "remember"));
	}

	/** Spring URL-encodes the Base64 series ("+", "/" and "=") into the cookie, and stores the decoded series. */
	@Test
	void extractSeriesDecodesTheUrlEncodedSeriesAsTheTokenStoreHoldsIt() {
		hasCookie(cookieValue("0PjFbQ0J05e%2BYG2CIMTo7A%3D%3D", "rvuoMYXOalndH3e9VZSUgA%3D%3D"));

		assertEquals("0PjFbQ0J05e+YG2CIMTo7A==", SkyveRememberMeServices.extractSeries(request, "remember"));
	}

	@Test
	void extractSeriesToleratesAMissingPadding() {
		String unpadded = cookieValue("pcSeries1", "t").replace("=", "");
		hasCookie(unpadded);

		assertEquals("pcSeries1", SkyveRememberMeServices.extractSeries(request, "remember"));
	}

	@Test
	void extractSeriesIsNullWithoutAUsableCookie() {
		when(request.getCookies()).thenReturn(null);
		assertNull(SkyveRememberMeServices.extractSeries(request, "remember"));

		when(request.getCookies()).thenReturn(new Cookie[] {new Cookie("JSESSIONID", "x")});
		assertNull(SkyveRememberMeServices.extractSeries(request, "remember"));

		hasCookie("not base 64 !");
		assertNull(SkyveRememberMeServices.extractSeries(request, "remember"));

		hasCookie("");
		assertNull(SkyveRememberMeServices.extractSeries(request, "remember"));

		hasCookie(Base64.getEncoder().encodeToString("one:two:three".getBytes(StandardCharsets.UTF_8)));
		assertNull(SkyveRememberMeServices.extractSeries(request, "remember"));

		hasCookie(Base64.getEncoder().encodeToString(":token".getBytes(StandardCharsets.UTF_8)));
		assertNull(SkyveRememberMeServices.extractSeries(request, "remember"));
	}
}
