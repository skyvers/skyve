package org.skyve.impl.web.faces.actions;

import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.metadata.user.User;

@SuppressWarnings({"static-method", "boxing"})
class RerenderActionTest {
	@AfterEach
	void tearDown() {
		clearThreadPersistence();
	}

	@Test
	void callbackReturnsWhenTargetBeanIsNull() throws Exception {
		User user = mock(User.class);
		bindPersistenceForUser(user);
		FacesView facesView = mock(FacesView.class);
		when(facesView.getBean()).thenReturn(null);

		RerenderAction action = new RerenderAction(facesView, "source", false);
		action.callback();
	}

	private static void bindPersistenceForUser(User user) {
		AbstractPersistence persistence = mock(AbstractPersistence.class, CALLS_REAL_METHODS);
		persistence.setUser(user);
		persistence.setForThread();
	}

	@SuppressWarnings("unchecked")
	private static void clearThreadPersistence() {
		try {
			Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
			field.setAccessible(true);
			ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
			threadLocal.remove();
		}
		catch (Exception e) {
			throw new IllegalStateException("Unable to clear thread local persistence", e);
		}
	}
}
