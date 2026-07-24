package org.skyve.impl.web;

import static org.mockito.Mockito.when;

import org.skyve.metadata.router.UxUi;
import org.skyve.web.UserAgentType;

import jakarta.servlet.http.HttpServletRequest;

/** Installs the single private request-selection value on servlet mocks. */
public final class RequestUxUiSelectionTestUtil {
	private static final String REQUEST_SELECTION_KEY = UserAgent.class.getName() + ".selection";

	private RequestUxUiSelectionTestUtil() {
		// Utility class.
	}

	/** Installs and returns a request selection for a mocked request. */
	public static RequestUxUiSelection install(HttpServletRequest request,
			UserAgentType type,
			boolean emulated,
			UxUi uxui) {
		RequestUxUiSelection selection = new RequestUxUiSelection(type, emulated, uxui);
		when(request.getAttribute(REQUEST_SELECTION_KEY)).thenReturn(selection);
		return selection;
	}
}
