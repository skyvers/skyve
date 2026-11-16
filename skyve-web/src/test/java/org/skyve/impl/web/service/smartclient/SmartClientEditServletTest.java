package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import org.junit.jupiter.api.Test;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.user.User;

/**
 * Tests the {@link SmartClientEditServlet} steps that need no persistence or metadata.
 */
class SmartClientEditServletTest {
	@Test
	@SuppressWarnings({"static-method", "boxing"})
	void deletingAPersistedBeanWithoutAnOptimisticLockIsAnInvariantFailure() throws Exception {
		User user = mock(User.class);
		Document document = mock(Document.class);
		PersistentBean beanToDelete = mock(PersistentBean.class);
		when(beanToDelete.getBizId()).thenReturn("bean-1");
		PersistentBean persisted = mock(PersistentBean.class); // getBizLock() returns null
		when(persisted.getBizId()).thenReturn("bean-1");
		when(persisted.getBizModule()).thenReturn("mod");
		when(persisted.getBizDocument()).thenReturn("Doc");
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		doReturn(persisted).when(persistence).retrieve(document, "bean-1");
		when(user.canDeleteDocument(document)).thenReturn(true);
		when(user.canReadBean(anyString(), anyString(), anyString(), any(), any(), any())).thenReturn(true);
		Method retrieveForDelete = SmartClientEditServlet.class.getDeclaredMethod("retrieveForDelete",
																					User.class,
																					Document.class,
																					PersistentBean.class,
																					AbstractPersistence.class);
		retrieveForDelete.setAccessible(true);

		InvocationTargetException e = assertThrows(InvocationTargetException.class,
													() -> retrieveForDelete.invoke(null, user, document, beanToDelete, persistence));

		IllegalStateException cause = assertInstanceOf(IllegalStateException.class, e.getCause());
		assertEquals("Persisted bean bean-1 has no optimistic lock", cause.getMessage());
	}
}
