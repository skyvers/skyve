package org.skyve.metadata.router;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;

import jakarta.servlet.http.HttpServletRequest;

import org.junit.Test;
import org.skyve.web.UserAgentType;

@SuppressWarnings("static-method")
public class UxUiSelectorTest {

	/**
	 * Verifies that the default {@code emulate} method delegates to {@code select}.
	 */
	@Test
	public void emulateDefaultDelegatesToSelect() {
		UxUi expected = UxUi.newPrimeFaces("desktop", "template", "theme");
		UxUiSelector selector = (userAgentType, request) -> expected;

		HttpServletRequest mockRequest = mock(HttpServletRequest.class);
		UxUi result = selector.emulate(UserAgentType.desktop, mockRequest);

		assertThat(result, is(expected));
	}
}
