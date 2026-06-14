package org.skyve.impl.persistence.hibernate;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;

import org.hibernate.event.spi.PreUpdateEvent;
import org.hibernate.event.spi.PostDeleteEvent;
import org.hibernate.event.spi.PostLoadEvent;
import org.hibernate.event.spi.PreDeleteEvent;
import org.hibernate.metadata.ClassMetadata;
import org.hibernate.persister.entity.EntityPersister;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.SkyveException;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.user.User;

/**
 * Tests for {@link HibernateListener}.
 */
@SuppressWarnings("static-method")
class HibernateListenerTest {

	/** requiresPostCommitHanding() always returns false — verifies the contract. */
	@Test
	void testRequiresPostCommitHandingReturnsFalse() {
		HibernateListener listener = new HibernateListener();
		EntityPersister persister = mock(EntityPersister.class);
		assertFalse(listener.requiresPostCommitHanding(persister));
	}

	@Test
	void onPreUpdateInjectsFreshOptimisticLockIntoBeanAndState() throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		when(user.getName()).thenReturn("admin");
		when(persistence.getUser()).thenReturn(user);
		bindPersistenceToThread(persistence);

		try {
			PersistentBean bean = mock(PersistentBean.class);
			EntityPersister persister = mock(EntityPersister.class);
			ClassMetadata classMetadata = mock(ClassMetadata.class);
			when(persister.getClassMetadata()).thenReturn(classMetadata);
			when(classMetadata.getPropertyNames()).thenReturn(new String[] {"name", PersistentBean.LOCK_NAME, "description"});
			Object[] state = {"original", null, "unchanged"};
			PreUpdateEvent event = new PreUpdateEvent(bean, "id-1", state, new Object[state.length], persister, null);

			assertFalse(new HibernateListener().onPreUpdate(event));

			assertSame(state[1], beanLock(bean));
			OptimisticLock lock = (OptimisticLock) state[1];
			assertEquals("admin", lock.getUsername());
			assertEquals("original", state[0]);
			assertEquals("unchanged", state[2]);
		}
		finally {
			unbindPersistenceFromThread();
		}
	}

	@Test
	void onPostLoadDelegatesToPersistence() throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		bindPersistenceToThread(persistence);
		try {
			PersistentBean bean = mock(PersistentBean.class);

			new HibernateListener().onPostLoad(new PostLoadEvent(null).setEntity(bean));

			verify(persistence).postLoad(bean);
		}
		finally {
			unbindPersistenceFromThread();
		}
	}

	@Test
	void onPostLoadWrapsCheckedPersistenceFailures() throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		bindPersistenceToThread(persistence);
		try {
			PersistentBean bean = mock(PersistentBean.class);
			Exception failure = new Exception("post-load failed");
			HibernateListener listener = new HibernateListener();
			PostLoadEvent event = new PostLoadEvent(null).setEntity(bean);
			doThrow(failure).when(persistence)
							.postLoad(bean);

			DomainException thrown = assertThrows(DomainException.class,
					() -> listener.onPostLoad(event));

			assertSame(failure, thrown.getCause());
		}
		finally {
			unbindPersistenceFromThread();
		}
	}

	@Test
	void onPreDeleteDelegatesToPersistenceWithoutVeto() throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		bindPersistenceToThread(persistence);
		try {
			PersistentBean bean = mock(PersistentBean.class);
			PreDeleteEvent event = new PreDeleteEvent(bean, "id-1", new Object[0], mock(EntityPersister.class), null);

			assertFalse(new HibernateListener().onPreDelete(event));

			verify(persistence).preRemove(bean);
		}
		finally {
			unbindPersistenceFromThread();
		}
	}

	@Test
	void onPreDeleteWrapsCheckedPersistenceFailures() throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		bindPersistenceToThread(persistence);
		try {
			PersistentBean bean = mock(PersistentBean.class);
			Exception failure = new Exception("pre-remove failed");
			doThrow(failure).when(persistence).preRemove(bean);
			PreDeleteEvent event = new PreDeleteEvent(bean, "id-1", new Object[0], mock(EntityPersister.class), null);

			DomainException thrown = assertThrows(DomainException.class,
					() -> new HibernateListener().onPreDelete(event));

			assertSame(failure, thrown.getCause());
		}
		finally {
			unbindPersistenceFromThread();
		}
	}

	@Test
	void onPostDeleteDelegatesToPersistence() throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		bindPersistenceToThread(persistence);
		try {
			PersistentBean bean = mock(PersistentBean.class);
			PostDeleteEvent event = new PostDeleteEvent(bean, "id-1", new Object[0], mock(EntityPersister.class), null);

			new HibernateListener().onPostDelete(event);

			verify(persistence).postRemove(bean);
		}
		finally {
			unbindPersistenceFromThread();
		}
	}

	@Test
	void onPostDeletePropagatesSkyveExceptions() throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		bindPersistenceToThread(persistence);
		try {
			PersistentBean bean = mock(PersistentBean.class);
			SkyveException failure = new DomainException("post-remove failed");
			doThrow(failure).when(persistence).postRemove(bean);
			PostDeleteEvent event = new PostDeleteEvent(bean, "id-1", new Object[0], mock(EntityPersister.class), null);

			assertSame(failure, assertThrows(SkyveException.class,
					() -> new HibernateListener().onPostDelete(event)));
		}
		finally {
			unbindPersistenceFromThread();
		}
	}

	private static OptimisticLock beanLock(PersistentBean bean) {
		ArgumentCaptor<OptimisticLock> captor = ArgumentCaptor.forClass(OptimisticLock.class);
		verify(bean).setBizLock(captor.capture());
		return captor.getValue();
	}

	private static void bindPersistenceToThread(AbstractPersistence persistence) throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		@SuppressWarnings("unchecked")
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		threadLocal.set(persistence);
	}

	private static void unbindPersistenceFromThread() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		@SuppressWarnings("unchecked")
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		threadLocal.remove();
	}
}
