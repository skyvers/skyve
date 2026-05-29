package org.skyve.impl.cdi;

import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.SortedMap;
import java.util.TreeMap;

import org.junit.Test;
import org.skyve.impl.persistence.AbstractPersistence;

@SuppressWarnings("static-method")
public class SkyveCDIProducerTest {
	@Test
	public void producerMethodsReturnExpectedInjectablesAndStash() throws Exception {
		SortedMap<String, Object> stash = new TreeMap<>();
		stash.put("k", "v");

		assertSame(SkyveCDIProducer.class, SkyveCDIProducer.class.getDeclaredConstructor().newInstance().getClass());

		withThreadLocalPersistence(stash, () -> {
			assertTrue(SkyveCDIProducer.getPersistence() instanceof PersistenceInjectable);
			assertTrue(SkyveCDIProducer.getUser() instanceof UserInjectable);
			assertTrue(SkyveCDIProducer.getCustomer() instanceof CustomerInjectable);
			assertSame(stash, SkyveCDIProducer.getStash());
			assertTrue(SkyveCDIProducer.getRepository() instanceof RepositoryInjectable);
			assertTrue(SkyveCDIProducer.getAddInManager() instanceof AddInManagerInjectable);
			assertTrue(SkyveCDIProducer.getReporting() instanceof ReportingInjectable);
			assertTrue(SkyveCDIProducer.getCaching() instanceof CachingInjectable);
			assertTrue(SkyveCDIProducer.getJobScheduler() instanceof JobSchedulerInjectable);
			assertTrue(SkyveCDIProducer.getNumberGenerator() instanceof NumberGeneratorInjectable);
			assertTrue(SkyveCDIProducer.getGeoIPService() instanceof GeoIPServiceInjectable);
			assertTrue(SkyveCDIProducer.getMailService() instanceof MailServiceInjectable);
		});
	}

	private interface ThrowingRunnable {
		void run() throws Exception;
	}

	private static void withThreadLocalPersistence(SortedMap<String, Object> stash, ThrowingRunnable runnable)
	throws Exception {
		ThreadLocal<AbstractPersistence> threadLocal = getThreadLocalPersistence();
		AbstractPersistence original = threadLocal.get();
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		when(persistence.getStash()).thenReturn(stash);
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

	@SuppressWarnings("unchecked")
	private static ThreadLocal<AbstractPersistence> getThreadLocalPersistence() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		return (ThreadLocal<AbstractPersistence>) field.get(null);
	}
}