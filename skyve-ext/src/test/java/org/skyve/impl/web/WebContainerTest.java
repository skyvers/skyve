package org.skyve.impl.web;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.mockito.Mockito.mock;

import org.junit.After;
import org.junit.Test;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

@SuppressWarnings("static-method")
public class WebContainerTest {
	@After
	public void tearDown() {
		WebContainer.clear();
	}

	@Test
	public void getHttpServletRequestResponseReturnsNullWhenNotSet() {
		assertNull(WebContainer.getHttpServletRequestResponse());
	}

	@Test
	public void setAndGetHttpServletRequestResponse() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);

		WebContainer.setHttpServletRequestResponse(request, response);

		HttpServletRequestResponse result = WebContainer.getHttpServletRequestResponse();
		assertNotNull(result);
		assertSame(request, result.getRequest());
		assertSame(response, result.getResponse());
	}

	@Test
	public void clearRemovesHttpServletRequestResponse() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);

		WebContainer.setHttpServletRequestResponse(request, response);
		WebContainer.clear();

		assertNull(WebContainer.getHttpServletRequestResponse());
	}
}
