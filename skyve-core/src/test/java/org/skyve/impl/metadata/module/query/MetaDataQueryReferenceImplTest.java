package org.skyve.impl.metadata.module.query;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.repository.ProvidedRepository;

@ExtendWith(MockitoExtension.class)
class MetaDataQueryReferenceImplTest {

	@Mock
	private Module owningModule;

	@Test
	@SuppressWarnings("static-method")
	void constructorStoresName() {
		MetaDataQueryReferenceImpl ref = new MetaDataQueryReferenceImpl("myQuery", "admin", "ContactQuery");
		assertThat(ref.getName(), is("myQuery"));
	}

	@Test
	@SuppressWarnings("static-method")
	void constructorStoresModuleRef() {
		MetaDataQueryReferenceImpl ref = new MetaDataQueryReferenceImpl("myQuery", "admin", "ContactQuery");
		assertThat(ref.getModuleRef(), is("admin"));
	}

	@Test
	@SuppressWarnings("static-method")
	void constructorStoresRef() {
		MetaDataQueryReferenceImpl ref = new MetaDataQueryReferenceImpl("myQuery", "admin", "ContactQuery");
		assertThat(ref.getRef(), is("ContactQuery"));
	}

	@Test
	@SuppressWarnings("static-method")
	void nullNameFallsBackToRef() {
		MetaDataQueryReferenceImpl ref = new MetaDataQueryReferenceImpl(null, "admin", "ContactQuery");
		assertThat(ref.getName(), is("ContactQuery"));
	}

	@Test
	void setOwningModuleRoundtrip() {
		MetaDataQueryReferenceImpl ref = new MetaDataQueryReferenceImpl("q", "admin", "ContactQuery");
		ref.setOwningModule(owningModule);
		assertThat(ref.getOwningModule(), is(owningModule));
	}

	@Test
	@SuppressWarnings("static-method")
	void defaultOwningModuleIsNull() {
		MetaDataQueryReferenceImpl ref = new MetaDataQueryReferenceImpl("q", "admin", "ContactQuery");
		assertNull(ref.getOwningModule());
	}

	@Test
	@SuppressWarnings("static-method")
	void toStringContainsFields() {
		MetaDataQueryReferenceImpl ref = new MetaDataQueryReferenceImpl("myQuery", "admin", "ContactQuery");
		String str = ref.toString();
		assertThat(str, containsString("admin"));
		assertThat(str, containsString("ContactQuery"));
	}

	@Test
	@SuppressWarnings("static-method")
	void getDocumentNameDelegatesToTheReferencedQuery() {
		ProvidedRepository repository = mock(ProvidedRepository.class);
		Module module = mock(Module.class);
		MetaDataQueryDefinition target = mock(MetaDataQueryDefinition.class);
		when(repository.getModule(null, "admin")).thenReturn(module);
		when(module.getMetaDataQuery("ContactQuery")).thenReturn(target);
		when(target.getDocumentName()).thenReturn("Contact");
		MetaDataQueryReferenceImpl ref = new MetaDataQueryReferenceImpl("q", "admin", "ContactQuery");

		withRepository(repository, () -> assertThat(ref.getDocumentName(), is("Contact")));
	}

	@Test
	void getDocumentNameThrowsWhenTheReferencedQueryIsNotDefined() {
		ProvidedRepository repository = mock(ProvidedRepository.class);
		Module module = mock(Module.class);
		when(repository.getModule(null, "admin")).thenReturn(module);
		when(owningModule.getName()).thenReturn("whosWho");
		MetaDataQueryReferenceImpl ref = new MetaDataQueryReferenceImpl("q", "admin", "ContactQuery");
		ref.setOwningModule(owningModule);

		withRepository(repository, () -> {
			MetaDataException e = assertThrows(MetaDataException.class, ref::getDocumentName);
			assertThat(e.getMessage(), is("Imported query admin.ContactQuery in module whosWho is not defined."));
		});
	}

	static void withRepository(ProvidedRepository repository, Runnable assertions) {
		ProvidedRepository previous = ProvidedRepositoryFactory.isConfigured() ? ProvidedRepositoryFactory.get() : null;
		try {
			ProvidedRepositoryFactory.set(repository);
			assertions.run();
		}
		finally {
			if (previous == null) {
				ProvidedRepositoryFactory.clear();
			}
			else {
				ProvidedRepositoryFactory.set(previous);
			}
		}
	}
}
