package org.skyve.impl.web;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.router.UxUi;
import org.skyve.web.UserAgentType;

import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;

@SuppressWarnings("static-method")
class UserAgentTest {
	@Test
	void getTypeReturnsRequestAttributeWhenAlreadyResolved() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getAttribute(AbstractWebContext.USER_AGENT_TYPE_KEY)).thenReturn(UserAgentType.phone);

		assertThat(UserAgent.getType(request), is(UserAgentType.phone));
	}

	@Test
	void getTypeCachesUnknownAgentAsOther() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getCookies()).thenReturn(new Cookie[] {new Cookie("touch", "1")});
		when(request.getHeader("User-Agent")).thenReturn("unknown-test-agent");

		assertThat(UserAgent.getType(request), is(UserAgentType.other));
		verify(request).setAttribute(AbstractWebContext.USER_AGENT_TYPE_KEY, UserAgentType.other);
	}

	@Test
	void getTypeTreatsMissingUserAgentHeaderAsOther() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getCookies()).thenReturn(null);
		when(request.getHeader("User-Agent")).thenReturn(null);

		assertThat(UserAgent.getType(request), is(UserAgentType.other));
		verify(request).setAttribute(AbstractWebContext.USER_AGENT_TYPE_KEY, UserAgentType.other);
	}

	@Test
	void getTypeResolvesCommonMobileUserAgentAsPhone() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getHeader("User-Agent")).thenReturn(
				"Mozilla/5.0 (iPhone; CPU iPhone OS 17_0 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.0 Mobile/15E148 Safari/604.1");

		assertThat(UserAgent.getType(request), is(UserAgentType.phone));
		verify(request).setAttribute(AbstractWebContext.USER_AGENT_TYPE_KEY, UserAgentType.phone);
	}

	@Test
	void getTypePromotesTouchEnabledDesktopToTablet() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getCookies()).thenReturn(new Cookie[] {new Cookie("touch", "1")});
		when(request.getHeader("User-Agent")).thenReturn(
				"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36 SkyveTouchTest");

		assertThat(UserAgent.getType(request), is(UserAgentType.tablet));
		verify(request).setAttribute(AbstractWebContext.USER_AGENT_TYPE_KEY, UserAgentType.tablet);
	}

	@Test
	void getUxUiReturnsRequestAttributeWhenAlreadyResolved() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		UxUi uxui = UxUi.newPrimeFaces("desktop", "template", "theme");
		when(request.getAttribute(AbstractWebContext.UXUI)).thenReturn(uxui);

		assertThat(UserAgent.getUxUi(request), is(uxui));
	}

	@Test
	void isEmulatedReflectsEmulatedRequestAttribute() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getAttribute(AbstractWebContext.EMULATED_USER_AGENT_TYPE_KEY)).thenReturn(UserAgentType.desktop);

		assertTrue(UserAgent.isEmulated(request));
	}

	@Test
	void isEmulatedReturnsFalseWithoutEmulatedRequestAttribute() {
		HttpServletRequest request = mock(HttpServletRequest.class);

		assertFalse(UserAgent.isEmulated(request));
	}
}
