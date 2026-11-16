package org.skyve.metadata.view.model.list;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;

/**
 * Tests the root node row that {@link RelationTreeModelFilter} makes for the edited bean, with a mocked current user.
 */
class RelationTreeModelFilterTest {
	@AfterEach
	@SuppressWarnings("static-method")
	void clearPersistenceThreadLocal() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		@SuppressWarnings("unchecked")
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		threadLocal.remove();
	}

	@Test
	@SuppressWarnings("static-method")
	void filterAddsTheBeanAsTheRootNodeLockedByTheCurrentUser() {
		Bean bean = bean();
		setUser("alice", bean);
		RelationTreeModelFilter<Bean> filter = new RelationTreeModelFilter<>(bean);

		List<Bean> rows = new ArrayList<>();
		filter.filter(rows);

		assertEquals(1, rows.size());
		Bean row = rows.get(0);
		Object lock = row.getDynamic(PersistentBean.LOCK_NAME);
		assertEquals("alice", (lock instanceof OptimisticLock optimisticLock) ? optimisticLock.getUsername() : null);
		// the bizId is suffixed with a random ID to handle cyclic object graphs
		String id = (String) row.getDynamic(Bean.DOCUMENT_ID);
		assertTrue((id != null) && id.startsWith("bean-1"), id);
	}

	@Test
	@SuppressWarnings("static-method")
	void filterThrowsWhenTheCurrentUserHasNoName() {
		Bean bean = bean();
		setUser(null, bean);
		RelationTreeModelFilter<Bean> filter = new RelationTreeModelFilter<>(bean);
		List<Bean> rows = new ArrayList<>();

		IllegalStateException e = assertThrows(IllegalStateException.class, () -> filter.filter(rows));
		assertEquals("The current user has no name", e.getMessage());
		assertTrue(rows.isEmpty());
	}

	private static Bean bean() {
		Bean result = mock(Bean.class);
		when(result.getBizId()).thenReturn("bean-1");
		when(result.getBizModule()).thenReturn("admin");
		when(result.getBizDocument()).thenReturn("Contact");
		when(result.getBizKey()).thenReturn("Alice");
		return result;
	}

	/**
	 * Puts a mocked persistence on the thread whose user has the given name and can resolve the bean's document.
	 */
	private static void setUser(String userName, Bean bean) {
		Document document = mock(Document.class);
		Module module = mock(Module.class);
		Customer customer = mock(Customer.class);
		when(customer.getModule(bean.getBizModule())).thenReturn(module);
		when(module.getDocument(customer, bean.getBizDocument())).thenReturn(document);
		User user = mock(User.class);
		when(user.getName()).thenReturn(userName);
		when(user.getCustomer()).thenReturn(customer);
		AbstractPersistence persistence = mock(AbstractPersistence.class,
														withSettings().defaultAnswer(CALLS_REAL_METHODS));
		persistence.setUser(user);
		persistence.setForThread();
	}
}
