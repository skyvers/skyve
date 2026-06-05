package org.skyve.impl.web;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
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
	void isEmulatedReflectsEmulatedRequestAttribute() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getAttribute(AbstractWebContext.EMULATED_USER_AGENT_TYPE_KEY)).thenReturn(UserAgentType.desktop);

		assertTrue(UserAgent.isEmulated(request));
	}
}
