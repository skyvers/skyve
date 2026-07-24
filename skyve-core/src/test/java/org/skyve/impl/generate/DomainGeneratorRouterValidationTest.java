package org.skyve.impl.generate;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.router.Direct;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.metadata.repository.router.UxUiMetadata;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.repository.ProvidedRepository;

/** Proves that generation validation assembles and validates router metadata once per customer. */
class DomainGeneratorRouterValidationTest {
	@Test
	@SuppressWarnings("static-method")
	void validatesModuleToGlobalTargetOnceForCustomerWithNoDocuments() throws Exception {
		ProvidedRepository repository = mock(ProvidedRepository.class);
		Customer customer = mock(Customer.class);
		when(repository.getCustomer("empty")).thenReturn(customer);
		when(customer.getModules()).thenReturn(List.of());

		Router global = globalRouter();
		Router module = moduleTargeting("/global.xhtml", "global");
		Router effective = spy(globalRouter());
		effective.merge(module);
		when(repository.getGlobalRouter()).thenReturn(global);
		when(repository.getModuleRouters()).thenReturn(List.of(module));
		when(repository.getRouter()).thenReturn(effective);

		new TestDomainGenerator(repository).validateCustomer("empty");

		verify(repository, times(1)).getGlobalRouter();
		verify(repository, times(1)).getModuleRouters();
		verify(repository, times(1)).getRouter();
		verify(effective, times(1)).validateDirectTargets();
	}

	@Test
	@SuppressWarnings("static-method")
	void validatesRouterOnceWhileRetainingPerDocumentViewValidation() throws Exception {
		ProvidedRepository repository = mock(ProvidedRepository.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document first = mock(Document.class);
		Document second = mock(Document.class);
		when(repository.getCustomer("many")).thenReturn(customer);
		when(customer.getModules()).thenReturn(List.of(module));
		when(module.getName()).thenReturn("test");
		Map<String, DocumentRef> refs = new LinkedHashMap<>();
		refs.put("First", mock(DocumentRef.class));
		refs.put("Second", mock(DocumentRef.class));
		when(module.getDocumentRefs()).thenReturn(refs);
		when(module.getDocument(customer, "First")).thenReturn(first);
		when(module.getDocument(customer, "Second")).thenReturn(second);

		Router global = globalRouter();
		Router effective = spy(globalRouter());
		when(repository.getGlobalRouter()).thenReturn(global);
		when(repository.getModuleRouters()).thenReturn(List.of());
		when(repository.getRouter()).thenReturn(effective);

		new TestDomainGenerator(repository).validateCustomer("many");

		verify(repository, times(1)).getGlobalRouter();
		verify(repository, times(1)).getModuleRouters();
		verify(repository, times(1)).getRouter();
		verify(effective, times(1)).validateDirectTargets();
		verify(repository, times(1)).validateDocumentForGenerateDomain(customer, first);
		verify(repository, times(1)).validateDocumentForGenerateDomain(customer, second);
		verify(repository, times(4)).getView(org.mockito.ArgumentMatchers.eq("global"),
				org.mockito.ArgumentMatchers.same(customer),
				org.mockito.ArgumentMatchers.any(Document.class),
				org.mockito.ArgumentMatchers.anyString());
	}

	@Test
	@SuppressWarnings("static-method")
	void rejectsUnknownTargetThroughWholeMetadataValidation() {
		ProvidedRepository repository = mock(ProvidedRepository.class);
		Customer customer = mock(Customer.class);
		when(repository.getCustomer("invalid")).thenReturn(customer);
		when(customer.getModules()).thenReturn(List.of());
		Router global = globalRouter();
		Router effective = globalRouter();
		effective.getDirects().add(direct("/missing.xhtml", "missing"));
		when(repository.getGlobalRouter()).thenReturn(global);
		when(repository.getModuleRouters()).thenReturn(List.of());
		when(repository.getRouter()).thenReturn(effective);

		assertThrows(MetaDataException.class,
				() -> new TestDomainGenerator(repository).validateCustomer("invalid"));

		verify(repository, times(1)).getRouter();
	}

	private static Router globalRouter() {
		Router result = new Router();
		result.setUxuiSelectorClassName("router.Selector");
		UxUiMetadata uxui = new UxUiMetadata();
		uxui.setName("global");
		result.getUxUis().add(uxui);
		return result.convert("global router");
	}

	private static Router moduleTargeting(String path, String uxui) {
		Router result = new Router();
		result.getDirects().add(direct(path, uxui));
		return result.convert("module router");
	}

	private static Direct direct(String path, String uxui) {
		Direct result = new Direct();
		result.setPath(path);
		result.setUxui(uxui);
		return result;
	}

	private static final class TestDomainGenerator extends DomainGenerator {
		private TestDomainGenerator(ProvidedRepository repository) {
			super(false, false, false, DialectOptions.H2_NO_INDEXES, "", "", "", "", null);
			this.repository = repository;
		}

		@Override
		public void generate() {
			// Validation tests do not generate source files.
		}
	}
}
