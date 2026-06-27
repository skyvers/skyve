package org.skyve.impl.cdi;

import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Function;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.DocumentPermissionScope;
import org.skyve.metadata.user.User;
import org.skyve.persistence.BizQL;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.persistence.SQL;

import jakarta.persistence.EntityManager;

@SuppressWarnings({ "static-method", "unchecked", "boxing", "resource", "java:S1220" })
class PersistenceInjectableTest {
	@Test
	void delegatesCorePersistenceLifecycleAndCrudOperations() throws Exception {
		TestContext context = newTestContext();

		withThreadLocalPersistence(context.persistence, () -> {
			assertSame(context.user, context.injectable.getUser());
			assertTrue(context.injectable.isPersisted(context.persistentBean));
			context.injectable.begin();
			context.injectable.rollback();
			context.injectable.setRollbackOnly();
			context.injectable.commit(true);
			context.injectable.evictAllCached();
			context.injectable.evictCached(context.bean);
			assertTrue(context.injectable.cached(context.bean));
			assertTrue(context.injectable.sharedCacheCollection("m", "d", "c", "o"));
			assertTrue(context.injectable.sharedCacheCollection(context.bean, "c"));
			assertTrue(context.injectable.sharedCacheBean("m", "d", "1"));
			assertTrue(context.injectable.sharedCacheBean(context.bean));
			context.injectable.evictAllSharedCache();
			context.injectable.evictSharedCacheCollections();
			context.injectable.evictSharedCacheCollections("m", "d", "c");
			context.injectable.evictSharedCacheCollection("m", "d", "c", "o");
			context.injectable.evictSharedCacheCollection(context.bean, "c");
			context.injectable.evictSharedCacheBeans();
			context.injectable.evictSharedCacheBeans("m", "d");
			context.injectable.evictSharedCachedBean("m", "d", "1");
			context.injectable.evictSharedCachedBean(context.bean);
			context.injectable.preMerge(context.document, context.persistentBean);
			assertSame(context.persistentBean, context.injectable.save(context.document, context.persistentBean));
			assertSame(context.persistentBean, context.injectable.save(context.persistentBean));
			assertSame(context.beans, context.injectable.save(context.beans));
			assertSame(context.beans, context.injectable.save(context.beanArray));
			assertSame(context.persistentBean, context.injectable.merge(context.document, context.persistentBean));
			assertSame(context.persistentBean, context.injectable.merge(context.persistentBean));
			assertSame(context.beans, context.injectable.merge(context.beans));
			assertSame(context.beans, context.injectable.merge(context.beanArray));
			context.injectable.postMerge(context.document, context.persistentBean);
			context.injectable.flush();
			context.injectable.delete(context.document, context.persistentBean);
			context.injectable.delete(context.persistentBean);
			context.injectable.upsertBeanTuple(context.persistentBean);
			context.injectable.upsertCollectionTuples(context.persistentBean, "c");
			context.injectable.insertCollectionTuples(context.persistentBean, "c");
			assertSame(context.bean, context.injectable.retrieve(context.document, "1"));
			assertSame(context.bean, context.injectable.retrieveAndLock(context.document, "1"));
			assertSame(context.bean, context.injectable.retrieve("m", "d", "1"));
			assertSame(context.bean, context.injectable.retrieveAndLock("m", "d", "1"));
			context.injectable.reindex(context.persistentBean);
			assertSame("ok", context.injectable.withDocumentPermissionScopes(context.scope, context.function));
			context.injectable.withDocumentPermissionScopes(context.scope, context.consumer);
		});

		verify(context.persistence).begin();
		verify(context.persistence).commit(true);
		verify(context.persistence, times(2)).save(context.document, context.persistentBean);
		verify(context.persistence, times(2)).merge(context.document, context.persistentBean);
		verify(context.persistence, times(2)).retrieveAndLock(context.document, "1");
	}

	@Test
	void delegatesCorePersistenceQueryFactoryOperations() throws Exception {
		TestContext context = newTestContext();

		withThreadLocalPersistence(context.persistence, () -> {
			assertSame(context.sql, context.injectable.newSQL("select 1"));
			assertSame(context.sql, context.injectable.newNamedSQL("m", "q"));
			assertSame(context.sql, context.injectable.newNamedSQL(context.module, "q"));
			assertSame(context.sql, context.injectable.newSQL("m", "d", "select 1"));
			assertSame(context.sql, context.injectable.newNamedSQL("m", "d", "q"));
			assertSame(context.sql, context.injectable.newSQL(context.document, "select 1"));
			assertSame(context.sql, context.injectable.newNamedSQL(context.document, "q"));
			assertSame(context.bizQL, context.injectable.newBizQL("from modules.test.AllAttributes"));
			assertSame(context.bizQL, context.injectable.newNamedBizQL("m", "q"));
			assertSame(context.bizQL, context.injectable.newNamedBizQL(context.module, "q"));
			assertSame(context.documentQuery, context.injectable.newNamedDocumentQuery("m", "q"));
			assertSame(context.documentQuery, context.injectable.newNamedDocumentQuery(context.module, "q"));
			assertSame(context.documentQuery, context.injectable.newDocumentQuery(context.document));
			assertSame(context.documentQuery, context.injectable.newDocumentQuery("m", "d"));
			assertSame(context.documentQuery,
					context.injectable.newDocumentQuery(context.document, "from x", "where x", "group by x", "order by x"));
			assertSame(context.documentQuery, context.injectable.newDocumentQuery(context.bean));
			assertSame(context.entityManager, context.injectable.getEntityManager());
		});

		verify(context.persistence).newNamedDocumentQuery(context.module, "q");
		verify(context.persistence).newNamedSQL(context.document, "q");
		verify(context.persistence).getEntityManager();
	}

	private static TestContext newTestContext() {
		TestContext context = new TestContext();
		context.persistence = mock(AbstractPersistence.class);
		context.injectable = new PersistenceInjectable();
		context.user = mock(User.class);
		context.customer = mock(Customer.class);
		context.bean = mock(Bean.class);
		context.persistentBean = mock(PersistentBean.class);
		context.document = mock(Document.class);
		context.module = mock(Module.class);
		context.sql = mock(SQL.class);
		context.bizQL = mock(BizQL.class);
		context.documentQuery = mock(DocumentQuery.class);
		context.entityManager = mock(EntityManager.class);
		context.beans = List.of(context.persistentBean);
		context.beanArray = new PersistentBean[] { context.persistentBean };
		context.scope = DocumentPermissionScope.customer;
		context.function = p -> "ok";
		context.consumer = p -> {
			// no-op consumer used only to verify delegation
		};

		doCallRealMethod().when(context.persistence).setUser(any(User.class));
		context.persistence.setUser(context.user);
		when(context.persistence.getUser()).thenReturn(context.user);
		when(context.user.getCustomer()).thenReturn(context.customer);
		when(context.persistentBean.getBizVersion()).thenReturn(Integer.valueOf(1));
		when(context.persistentBean.getBizModule()).thenReturn("m");
		when(context.persistentBean.getBizDocument()).thenReturn("d");
		when(context.customer.getModule("m")).thenReturn(context.module);
		when(context.module.getDocument(context.customer, "d")).thenReturn(context.document);
		when(context.persistence.cached(context.bean)).thenReturn(Boolean.TRUE);
		when(context.persistence.sharedCacheCollection("m", "d", "c", "o")).thenReturn(Boolean.TRUE);
		when(context.persistence.sharedCacheCollection(context.bean, "c")).thenReturn(Boolean.TRUE);
		when(context.persistence.sharedCacheBean("m", "d", "1")).thenReturn(Boolean.TRUE);
		when(context.persistence.sharedCacheBean(context.bean)).thenReturn(Boolean.TRUE);
		when(context.persistence.save(context.document, context.persistentBean)).thenReturn(context.persistentBean);
		when(context.persistence.save(context.beans)).thenReturn(context.beans);
		when(context.persistence.save(context.beanArray)).thenReturn(context.beans);
		when(context.persistence.merge(context.document, context.persistentBean)).thenReturn(context.persistentBean);
		when(context.persistence.merge(context.beans)).thenReturn(context.beans);
		when(context.persistence.merge(context.beanArray)).thenReturn(context.beans);
		when(context.persistence.retrieve(context.document, "1")).thenReturn(context.bean);
		when(context.persistence.retrieveAndLock(context.document, "1")).thenReturn(context.bean);
		when(context.persistence.withDocumentPermissionScopes(eq(context.scope), any(Function.class))).thenReturn("ok");
		doNothing().when(context.persistence).withDocumentPermissionScopes(eq(context.scope), any(Consumer.class));
		when(context.persistence.newSQL("select 1")).thenReturn(context.sql);
		when(context.persistence.newNamedSQL("m", "q")).thenReturn(context.sql);
		when(context.persistence.newNamedSQL(context.module, "q")).thenReturn(context.sql);
		when(context.persistence.newSQL("m", "d", "select 1")).thenReturn(context.sql);
		when(context.persistence.newNamedSQL("m", "d", "q")).thenReturn(context.sql);
		when(context.persistence.newSQL(context.document, "select 1")).thenReturn(context.sql);
		when(context.persistence.newNamedSQL(context.document, "q")).thenReturn(context.sql);
		when(context.persistence.newBizQL("from modules.test.AllAttributes")).thenReturn(context.bizQL);
		when(context.persistence.newNamedBizQL("m", "q")).thenReturn(context.bizQL);
		when(context.persistence.newNamedBizQL(context.module, "q")).thenReturn(context.bizQL);
		when(context.persistence.newNamedDocumentQuery("m", "q")).thenReturn(context.documentQuery);
		when(context.persistence.newNamedDocumentQuery(context.module, "q")).thenReturn(context.documentQuery);
		when(context.persistence.newDocumentQuery(context.document)).thenReturn(context.documentQuery);
		when(context.persistence.newDocumentQuery("m", "d")).thenReturn(context.documentQuery);
		when(context.persistence.newDocumentQuery(context.document,
				"from x",
				"where x",
				"group by x",
				"order by x")).thenReturn(context.documentQuery);
		when(context.persistence.newDocumentQuery(context.bean)).thenReturn(context.documentQuery);
		when(context.persistence.getEntityManager()).thenReturn(context.entityManager);

		return context;
	}

	private static class TestContext {
		private AbstractPersistence persistence;
		private PersistenceInjectable injectable;
		private User user;
		private Customer customer;
		private Bean bean;
		private PersistentBean persistentBean;
		private Document document;
		private Module module;
		private SQL sql;
		private BizQL bizQL;
		private DocumentQuery documentQuery;
		private EntityManager entityManager;
		private List<PersistentBean> beans;
		private PersistentBean[] beanArray;
		private DocumentPermissionScope scope;
		private Function<Persistence, String> function;
		private Consumer<Persistence> consumer;
	}

	private interface ThrowingRunnable {
		void run() throws Exception;
	}

	private static void withThreadLocalPersistence(AbstractPersistence persistence, ThrowingRunnable runnable)
	throws Exception {
		ThreadLocal<AbstractPersistence> threadLocal = getThreadLocalPersistence();
		AbstractPersistence original = threadLocal.get();
		try {
			threadLocal.set(persistence);
			runnable.run();
		}
		finally {
			if (original == null) {
				threadLocal.remove();
			}
			else {
				threadLocal.set(original);
			}
		}
	}

	private static ThreadLocal<AbstractPersistence> getThreadLocalPersistence() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		return (ThreadLocal<AbstractPersistence>) field.get(null);
	}
}
