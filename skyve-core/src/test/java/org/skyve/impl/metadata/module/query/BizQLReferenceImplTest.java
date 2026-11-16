package org.skyve.impl.metadata.module.query;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.BizQLDefinition;
import org.skyve.metadata.repository.ProvidedRepository;

class BizQLReferenceImplTest {

	@Test
	@SuppressWarnings("static-method")
	void getQueryDelegatesToTheReferencedQueryInTheRepositoryModule() {
		ProvidedRepository repository = mock(ProvidedRepository.class);
		Module module = mock(Module.class);
		BizQLDefinition target = mock(BizQLDefinition.class);
		when(repository.getModule(null, "admin")).thenReturn(module);
		when(module.getBizQL("ContactQuery")).thenReturn(target);
		when(target.getQuery()).thenReturn("SELECT bean FROM {admin.Contact} AS bean");
		BizQLReferenceImpl ref = new BizQLReferenceImpl(null, "admin", "ContactQuery");

		MetaDataQueryReferenceImplTest.withRepository(repository, () -> assertThat(ref.getQuery(), is("SELECT bean FROM {admin.Contact} AS bean")));
	}
}
