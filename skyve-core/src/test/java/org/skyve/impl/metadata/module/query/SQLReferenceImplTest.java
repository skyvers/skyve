package org.skyve.impl.metadata.module.query;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.SQLDefinition;
import org.skyve.metadata.repository.ProvidedRepository;

class SQLReferenceImplTest {

	@Test
	@SuppressWarnings("static-method")
	void getQueryDelegatesToTheReferencedQueryInTheRepositoryModule() {
		ProvidedRepository repository = mock(ProvidedRepository.class);
		Module module = mock(Module.class);
		SQLDefinition target = mock(SQLDefinition.class);
		when(repository.getModule(null, "admin")).thenReturn(module);
		when(module.getNullSafeSQL("ContactQuery")).thenReturn(target);
		when(target.getQuery()).thenReturn("SELECT 1");
		SQLReferenceImpl ref = new SQLReferenceImpl(null, "admin", "ContactQuery");

		MetaDataQueryReferenceImplTest.withRepository(repository, () -> assertThat(ref.getQuery(), is("SELECT 1")));
	}
}
