package org.skyve.impl.metadata.repository;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;

@ExtendWith(MockitoExtension.class)
class LockableDynamicRepositoryTest {

	private LockableDynamicRepository repo;

	@Mock
	private Module mockModule;

	@Mock
	private Document mockDocument;

	@BeforeEach
	void setUp() {
		repo = new LockableDynamicRepository();
	}

	@Test
	void withLockFunctionExecutesAndReturnsResult() {
		String result = repo.withLock(r -> "executed");
		assertEquals("executed", result);
	}

	@Test
	void withLockConsumerExecutes() {
		int[] called = {0};
		repo.withLock((Consumer<LockableDynamicRepository>) r -> called[0]++);
		assertEquals(1, called[0]);
	}

	@Test
	void withLockFunctionReceivesRepositoryArgument() {
		LockableDynamicRepository seen = repo.withLock(r -> r);
		assertThat(seen, is(repo));
	}

	@Test
	void getRouterReturnsNullForEmptyCache() {
		assertThat(repo.getRouter(), is(nullValue()));
	}

	@Test
	void getCustomerReturnsNullForEmptyCache() {
		assertThat(repo.getCustomer("nonexistent"), is(nullValue()));
	}

	@Test
	void getModuleWithNullCustomerReturnsNullForEmptyCache() {
		assertThat(repo.getModule(null, "nonexistent"), is(nullValue()));
	}

	@Test
	void vtableWithNullCustomerReturnsNullForEmptyCache() {
		assertThat(repo.vtable(null, "someKey"), is(nullValue()));
	}

	@Test
	void vtableWithCustomerReturnsNullForEmptyCache() {
		assertThat(repo.vtable("acme", "someKey"), is(nullValue()));
	}

	@Test
	void getDocumentWithNullCustomerReturnsNullForEmptyCache() {
		Map<String, Module.DocumentRef> refs = new HashMap<>();
		refs.put("SomeDocument", new Module.DocumentRef());
		Mockito.when(mockModule.getDocumentRefs()).thenReturn(refs);
		Mockito.when(mockModule.getName()).thenReturn("SomeModule");
		assertThat(repo.getDocument(null, mockModule, "SomeDocument"), is(nullValue()));
	}

	@Test
	void getMetaDataActionReturnsNullForEmptyCache() {
		assertThat(repo.getMetaDataAction(null, mockDocument, "SomeAction"), is(nullValue()));
	}

	@Test
	void getMetaDataBizletReturnsNullForEmptyCache() {
		assertThat(repo.getMetaDataBizlet(null, mockDocument), is(nullValue()));
	}

	@Test
	void withLockFunctionReleasesWriteLockAfterExecution() {
		// Lock should be released so a second call can proceed
		String first = repo.withLock(r -> "first");
		String second = repo.withLock(r -> "second");
		assertEquals("first", first);
		assertEquals("second", second);
	}

	@Test
	void withLockConsumerReleasesWriteLockAfterExecution() {
		int[] count = {0};
		repo.withLock((Consumer<LockableDynamicRepository>) r -> count[0]++);
		repo.withLock((Consumer<LockableDynamicRepository>) r -> count[0]++);
		assertEquals(2, count[0]);
	}
}
