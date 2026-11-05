package org.skyve.impl.web;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.mock;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

@SuppressWarnings("static-method")
class WebContainerTest {
	@AfterEach
	void tearDown() {
		WebContainer.clear();
	}

	@Test
	void getHttpServletRequestResponseReturnsNullWhenNotSet() {
		assertNull(WebContainer.getHttpServletRequestResponse());
	}

	@Test
	void setAndGetHttpServletRequestResponse() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);

		WebContainer.setHttpServletRequestResponse(request, response);

		HttpServletRequestResponse result = WebContainer.getHttpServletRequestResponse();
		assertNotNull(result);
		assertSame(request, result.getRequest());
		assertSame(response, result.getResponse());
	}

	@Test
	void clearRemovesHttpServletRequestResponse() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);

		WebContainer.setHttpServletRequestResponse(request, response);
		WebContainer.clear();

		assertNull(WebContainer.getHttpServletRequestResponse());
	}
}
