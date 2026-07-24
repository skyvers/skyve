package org.skyve.impl.web;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.skyve.impl.cache.DefaultCaching;
import org.skyve.impl.util.UtilImpl;
import org.skyve.web.UserAgentType;

import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;

@SuppressWarnings("static-method")
class UserAgentTest {
	private static boolean originalForceNonPersistentCaching;

	@BeforeAll
	static void startCaching() {
		originalForceNonPersistentCaching = UtilImpl.FORCE_NON_PERSISTENT_CACHING;
		DefaultCaching.get().shutdown();
		UtilImpl.FORCE_NON_PERSISTENT_CACHING = true;
		DefaultCaching.get().startup();
	}

	@AfterAll
	static void stopCaching() {
		DefaultCaching.get().shutdown();
		UtilImpl.FORCE_NON_PERSISTENT_CACHING = originalForceNonPersistentCaching;
	}

	@Test
	void repeatedDetectionUsesOneRequestStateAttribute() {
		HttpServletRequest request = request("unknown-test-agent");

		assertThat(UserAgent.detectType(request), is(UserAgentType.other));
		assertThat(UserAgent.detectType(request), is(UserAgentType.other));
		verify(request, times(1)).setAttribute(anyString(), any());
		verify(request, times(1)).getHeader("User-Agent");
	}

	@Test
	void missingUserAgentHeaderIsOther() {
		HttpServletRequest request = request(null);

		assertThat(UserAgent.detectType(request), is(UserAgentType.other));
	}

	@Test
	void commonMobileUserAgentIsPhone() {
		HttpServletRequest request = request(
				"Mozilla/5.0 (iPhone; CPU iPhone OS 17_0 like Mac OS X) AppleWebKit/605.1.15 " +
				"(KHTML, like Gecko) Version/17.0 Mobile/15E148 Safari/604.1");

		assertThat(UserAgent.detectType(request), is(UserAgentType.phone));
	}

	@Test
	void touchEnabledDesktopIsTablet() {
		HttpServletRequest request = request(
				"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 " +
				"(KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36 SkyveTouchTest");
		when(request.getCookies()).thenReturn(new Cookie[] {new Cookie("touch", "1")});

		assertThat(UserAgent.detectType(request), is(UserAgentType.tablet));
	}

	private static HttpServletRequest request(String userAgent) {
		Map<String, Object> attributes = new HashMap<>();
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getAttribute(anyString())).thenAnswer(invocation -> attributes.get(invocation.getArgument(0)));
		when(request.getHeader("User-Agent")).thenReturn(userAgent);
		org.mockito.Mockito.doAnswer(invocation -> {
			attributes.put(invocation.getArgument(0), invocation.getArgument(1));
			return null;
		}).when(request).setAttribute(anyString(), any());
		return request;
	}
}
