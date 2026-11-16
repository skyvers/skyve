package org.skyve.impl.metadata.user;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.module.menu.Menu;
import org.skyve.metadata.module.menu.MenuItem;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.user.User;
import org.skyve.metadata.user.UserAccess;
import org.skyve.metadata.view.View.ViewType;

/**
 * Tests for {@link AccessProcessor}.
 */
@SuppressWarnings("static-method")
class AccessProcessorTest {

	@Test
	void processGivesAListHomeWithoutADefaultQueryTheHomeDocumentAggregateForAllUxUis() {
		CustomerImpl customer = mock(CustomerImpl.class);
		User user = mock(User.class);
		when(user.getCustomer()).thenReturn(customer);
		Module module = mock(Module.class);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getHomeRef()).thenReturn(ViewType.list);
		when(module.getHomeDocumentName()).thenReturn("Contact");
		when(module.getDocumentRefs()).thenReturn(Map.of("Contact", new DocumentRef())); // no default query
		Menu menu = mock(Menu.class);
		when(menu.getItems()).thenReturn(List.of(mock(MenuItem.class))); // an item outside of accesses
		ProvidedRepository repository = mock(ProvidedRepository.class);
		Router router = mock(Router.class);
		when(repository.getRouter()).thenReturn(router);
		Map<String, Set<String>> accesses = new HashMap<>();

		ProvidedRepository previous = ProvidedRepositoryFactory.isConfigured() ? ProvidedRepositoryFactory.get() : null;
		try {
			ProvidedRepositoryFactory.set(repository);
			new AccessProcessor(user, Map.of("admin", menu), accesses).process();
		}
		finally {
			if (previous == null) {
				ProvidedRepositoryFactory.clear();
			}
			else {
				ProvidedRepositoryFactory.set(previous);
			}
		}

		assertEquals(Map.of(UserAccess.documentAggregate("admin", "Contact").toString(), UserAccess.ALL_UX_UIS), accesses);
	}
}
