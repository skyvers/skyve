package org.skyve.impl.job;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.quartz.JobExecutionException;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.metadata.repository.ProvidedRepository;

/**
 * Verifies that the metadata validation job validates each customer in the installed repository.
 */
@SuppressWarnings("static-method")
class ValidateMetaDataJobTest {
	private ProvidedRepository originalRepository;

	@BeforeEach
	void rememberRepository() {
		originalRepository = ProvidedRepositoryFactory.get();
	}

	@AfterEach
	void restoreRepository() {
		ProvidedRepositoryFactory.set(originalRepository);
	}

	@Test
	void executeValidatesEachCustomerInTheRepository() throws Exception {
		ProvidedRepository repository = mock(ProvidedRepository.class);
		when(repository.getAllCustomerNames()).thenReturn(List.of());
		ProvidedRepositoryFactory.set(repository);

		new ValidateMetaDataJob().execute(null);

		verify(repository).getAllCustomerNames();
	}

	@Test
	void executeFailsWhenThereIsNoRepository() {
		ProvidedRepositoryFactory.clear();

		JobExecutionException e = assertThrows(JobExecutionException.class, () -> new ValidateMetaDataJob().execute(null));

		assertThat(e.getMessage(), is("Error encountered whilst validating metadata"));
		assertThat(e.getCause(), instanceOf(IllegalStateException.class));
	}
}
