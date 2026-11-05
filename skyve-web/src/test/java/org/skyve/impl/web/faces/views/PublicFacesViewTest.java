package org.skyve.impl.web.faces.views;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.sail.mock.MockFacesContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.repository.ProvidedRepository;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;

@SuppressWarnings("static-method")
class PublicFacesViewTest {
	@Test
	void customerParameterGetterAndSetterRoundTrip() {
		PublicFacesView view = new PublicFacesView();

		view.setBizCustomerParameter("acme");

		assertEquals("acme", view.getBizCustomerParameter());
	}

	@Test
	void preRenderThrowsWhenCustomerParameterMissing() {
		PublicFacesView view = new PublicFacesView();

		try (MockFacesContext ignored = MockFacesContext.get()) {
			IllegalStateException ex = assertThrows(IllegalStateException.class, view::preRender);
			assertEquals("Malformed URL - this URL must have a 'c' parameter", ex.getMessage());
		}
	}

	@Test
	void preRenderRedirectsToTheNoPublicUserPageWhenTheCustomerHasNoPublicUser() {
		ProvidedRepository originalRepository = ProvidedRepositoryFactory.get();
		String originalCustomer = UtilImpl.CUSTOMER;
		ProvidedRepository repository = mock(ProvidedRepository.class);
		when(repository.getCustomer("acme")).thenReturn(mock(Customer.class));
		when(repository.retrievePublicUserName("acme")).thenReturn(null);
		MockHttpServletResponse response = new MockHttpServletResponse();
		PublicFacesView view = new PublicFacesView();
		view.setBizCustomerParameter("acme");

		ProvidedRepositoryFactory.set(repository);
		UtilImpl.CUSTOMER = null;
		try (MockFacesContext ignored = MockFacesContext.get(new MockHttpServletRequest(), response)) {
			view.preRender();
		}
		finally {
			UtilImpl.CUSTOMER = originalCustomer;
			ProvidedRepositoryFactory.set(originalRepository);
		}

		verify(repository).retrievePublicUserName("acme");
		String redirectedUrl = response.getRedirectedUrl();
		assertTrue((redirectedUrl != null) && redirectedUrl.endsWith("/pages/noPublicUser.jsp"), redirectedUrl);
	}
}
