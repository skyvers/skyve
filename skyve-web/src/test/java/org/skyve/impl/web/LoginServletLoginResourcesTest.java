package org.skyve.impl.web;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.repository.ProvidedRepository;

import jakarta.servlet.RequestDispatcher;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * Tests the login page resolution in {@link LoginServlet} without static mocking (see the disabled {@link LoginServletTest}).
 */
class LoginServletLoginResourcesTest {
	private final ProvidedRepository originalRepository = ProvidedRepositoryFactory.get();
	private final String originalCustomer = UtilImpl.CUSTOMER;

	@AfterEach
	void tearDown() {
		ProvidedRepositoryFactory.set(originalRepository);
		UtilImpl.CUSTOMER = originalCustomer;
	}

	@Test
	@SuppressWarnings("static-method")
	void defaultPagesAreUsedWhenCustomerHasNoLoginResources() throws Exception {
		UtilImpl.CUSTOMER = "demo";
		ProvidedRepository repository = mock(ProvidedRepository.class);
		Customer customer = mock(Customer.class); // getLoginResources() returns null
		when(repository.getCustomer("demo")).thenReturn(customer);
		ProvidedRepositoryFactory.set(repository);

		assertEquals("/pages/login.jsp", forwardedUrl(LoginServlet.LOGIN_PATH));
		assertEquals("/pages/loggedOut.jsp", forwardedUrl(LoginServlet.LOGGED_OUT_PATH));
	}

	private static String forwardedUrl(String servletPath) throws Exception {
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		RequestDispatcher dispatcher = mock(RequestDispatcher.class);
		when(request.getServletPath()).thenReturn(servletPath);
		when(request.getRequestDispatcher(anyString())).thenReturn(dispatcher);

		new LoginServlet().doGet(request, response);

		ArgumentCaptor<String> url = ArgumentCaptor.forClass(String.class);
		verify(request).getRequestDispatcher(url.capture());
		verify(dispatcher).forward(request, response);
		return url.getValue();
	}
}
