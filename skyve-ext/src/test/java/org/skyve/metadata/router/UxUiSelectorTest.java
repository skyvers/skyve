package org.skyve.metadata.router;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;

import jakarta.servlet.http.HttpServletRequest;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.MetaDataException;
import org.skyve.web.UserAgentType;

@SuppressWarnings("static-method")
class UxUiSelectorTest {

	/**
	 * Verifies that the default {@code emulate} method delegates to {@code select}.
	 */
	@Test
	void emulateDefaultDelegatesToSelect() {
		UxUi expected = UxUi.newPrimeFaces("desktop", "template", "theme");
		UxUiSelector selector = (userAgentType, request) -> expected;

		HttpServletRequest mockRequest = mock(HttpServletRequest.class);
		UxUi result = selector.emulate(UserAgentType.desktop, mockRequest);

		assertThat(result, is(expected));
	}

	@Test
	void resolveDefaultFailsFastWithoutInvokingSelect() {
		UxUiSelector selector = (userAgentType, request) -> {
			throw new AssertionError("resolve() must not invoke select()");
		};

		assertThrows(MetaDataException.class, () -> selector.resolve("trusted"));
	}
}
