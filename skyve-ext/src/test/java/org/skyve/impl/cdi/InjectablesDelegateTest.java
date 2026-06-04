package org.skyve.impl.cdi;

import static org.junit.Assert.assertSame;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.lang.reflect.Field;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Date;
import java.util.List;

import org.junit.Test;
import org.pf4j.PluginManager;
import org.skyve.domain.Bean;
import org.skyve.domain.number.NumberGenerator;
import org.skyve.impl.addin.PF4JAddInManager;
import org.skyve.impl.domain.number.NumberGeneratorStaticSingleton;
import org.skyve.impl.geoip.GeoIPServiceStaticSingleton;
import org.skyve.impl.job.JobSchedulerStaticSingleton;
import org.skyve.impl.mail.MailServiceStaticSingleton;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.util.UtilImpl;
import org.skyve.job.JobDescription;
import org.skyve.job.JobSchedule;
import org.skyve.job.JobScheduler;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.user.User;
import org.skyve.util.GeoIPService;
import org.skyve.util.IPGeolocation;
import org.skyve.util.Mail;
import org.skyve.util.MailDispatchOutcome;
import org.skyve.util.MailService;
import org.skyve.web.BackgroundTask;

@SuppressWarnings({ "static-method", "boxing", "java:S8692" }) // system clock OK
public class InjectablesDelegateTest {
	@Test
	public void repositoryInjectableDelegatesToCoreRepository() throws Exception {
		ProvidedRepository original = getProvidedRepository();
		ProvidedRepository repository = mock(ProvidedRepository.class);
		try {
			setProvidedRepository(repository);
			RepositoryInjectable injectable = new RepositoryInjectable();
			File file = new File("target/test-resource.txt");
			Router router = mock(Router.class);
			Customer customer = mock(Customer.class);
			Document document = mock(Document.class);
			UserImpl user = mock(UserImpl.class);
			Object factory = new Object();

			when(repository.findResourceFile("/x", "cust", "mod")).thenReturn(file);
			when(repository.getRouter()).thenReturn(router);
			when(repository.getCustomer("cust")).thenReturn(customer);
			when(repository.getDataFactory(customer, document)).thenReturn(factory);
			when(repository.retrieveUser("mike")).thenReturn(user);

			assertSame(file, injectable.findResourceFile("/x", "cust", "mod"));
			assertSame(router, injectable.getRouter());
			assertSame(customer, injectable.getCustomer("cust"));
			assertSame(factory, injectable.getDataFactory(customer, document));
			assertSame(user, injectable.retrieveUser("mike"));
		}
		finally {
			setProvidedRepository(original);
		}
	}

	@Test
	public void numberGeneratorInjectableDelegatesToStaticSingleton() {
		NumberGenerator original = NumberGeneratorStaticSingleton.get();
		NumberGenerator generator = mock(NumberGenerator.class);
		try {
			NumberGeneratorStaticSingleton.set(generator);
			NumberGeneratorInjectable injectable = new NumberGeneratorInjectable();
			when(generator.next("INV", "sales", "Invoice", "number", 6)).thenReturn("INV000123");

			String next = injectable.next("INV", "sales", "Invoice", "number", 6);

			assertSame("INV000123", next);
		}
		finally {
			NumberGeneratorStaticSingleton.set(original);
		}
	}

	@Test
	public void mailServiceInjectableDelegatesToEffectiveMailService() throws Exception {
		MailService originalConfigured = getMailServiceField("configuredInstance");
		MailService originalEffective = getMailServiceField("effectiveInstance");
		MailService service = mock(MailService.class);
		try {
			MailServiceStaticSingleton.set(service);
			MailServiceInjectable injectable = new MailServiceInjectable();
			Mail mail = mock(Mail.class);
			List<Mail> mails = List.of(mail);
			MailDispatchOutcome one = MailDispatchOutcome.sent("id-1");
			MailDispatchOutcome bulk = MailDispatchOutcome.sent("id-2");
			when(service.dispatchMail(mail)).thenReturn(one);
			when(service.dispatchBulkMail(mails)).thenReturn(bulk);

			try (ByteArrayOutputStream out = new ByteArrayOutputStream()) {
				injectable.writeMail(mail, out);
				injectable.sendMail(mail);
				injectable.sendBulkMail(mails);
				assertSame(one, injectable.dispatchMail(mail));
				assertSame(bulk, injectable.dispatchBulkMail(mails));

				verify(service).writeMail(mail, out);
				verify(service, times(2)).dispatchMail(mail);
				verify(service, times(2)).dispatchBulkMail(mails);
			}
		}
		finally {
			setMailServiceField("configuredInstance", originalConfigured);
			setMailServiceField("effectiveInstance", originalEffective);
		}
	}

	@Test
	public void addInManagerInjectableDelegatesToSingletonManager() throws Exception {
		PF4JAddInManager manager = PF4JAddInManager.get();
		PluginManager pluginManager = mock(PluginManager.class);
		Runnable extension = mock(Runnable.class);
		try {
			setPluginManager(manager, pluginManager);
			when(pluginManager.getExtensions(Runnable.class)).thenReturn(List.of(extension));

			AddInManagerInjectable injectable = new AddInManagerInjectable();
			assertSame(extension, injectable.getExtension(Runnable.class));
			injectable.shutdown();

			verify(pluginManager).getExtensions(Runnable.class);
			verify(pluginManager).stopPlugins();
		}
		finally {
			setPluginManager(manager, null);
		}
	}

	@Test
	public void addInManagerInjectableStartupUsesConfiguredAddinsDirectory() throws Exception {
		PF4JAddInManager manager = PF4JAddInManager.get();
		String originalAddinsDirectory = UtilImpl.ADDINS_DIRECTORY;
		Path tempDir = Files.createTempDirectory("addin-injectable-");
		try {
			setPluginManager(manager, null);
			UtilImpl.ADDINS_DIRECTORY = tempDir.toString();
			AddInManagerInjectable injectable = new AddInManagerInjectable();

			injectable.startup();

			org.junit.Assert.assertNotNull(getPluginManager(manager));
		}
		finally {
			manager.shutdown();
			setPluginManager(manager, null);
			UtilImpl.ADDINS_DIRECTORY = originalAddinsDirectory;
		}
	}

	@Test
	public void jobSchedulerInjectableDelegatesToStaticSingleton() {
		JobScheduler original = JobSchedulerStaticSingleton.get();
		JobScheduler scheduler = mock(JobScheduler.class);
		JobSchedulerInjectable injectable = new JobSchedulerInjectable();
		JobMetaData job = mock(JobMetaData.class);
		Bean bean = mock(Bean.class);
		User user = mock(User.class);
		JobSchedule schedule = mock(JobSchedule.class);
		@SuppressWarnings("unchecked")
		Class<? extends BackgroundTask<Bean>> taskClass = (Class<? extends BackgroundTask<Bean>>) (Class<?>) BackgroundTask.class;
		List<JobDescription> jobs = List.of(mock(JobDescription.class));
		try {
			JobSchedulerStaticSingleton.set(scheduler);
			when(scheduler.getCustomerRunningJobs()).thenReturn(jobs);
			when(scheduler.cancelJob("instance-1")).thenReturn(Boolean.TRUE);

			injectable.startup();
			injectable.runOneShotJob(job, bean, user);
			injectable.runOneShotJob(job, bean, user, 1);
			injectable.runBackgroundTask(taskClass, user, "wid");
			injectable.scheduleOneShotJob(job, bean, user, new Date());
			injectable.scheduleJob(schedule, user);
			injectable.unscheduleJob("u1", "cust");
			injectable.scheduleReport(schedule, user);
			injectable.unscheduleReport("u2", "cust");
			injectable.runContentGarbageCollector();
			injectable.validateMetaData();
			injectable.preRestore();
			injectable.runRestoreJob(bean);
			injectable.postRestore(true);
			injectable.shutdown();

			assertSame(jobs, injectable.getCustomerRunningJobs());
			org.junit.Assert.assertTrue(injectable.cancelJob("instance-1"));

			verify(scheduler).startup();
			verify(scheduler).runOneShotJob(job, bean, user);
			verify(scheduler).runOneShotJob(job, bean, user, 1);
			verify(scheduler).runBackgroundTask(taskClass, user, "wid");
			verify(scheduler).scheduleOneShotJob(eq(job), eq(bean), eq(user), any(Date.class));
			verify(scheduler).scheduleJob(schedule, user);
			verify(scheduler).unscheduleJob("u1", "cust");
			verify(scheduler).scheduleReport(schedule, user);
			verify(scheduler).unscheduleReport("u2", "cust");
			verify(scheduler).runContentGarbageCollector();
			verify(scheduler).validateMetaData();
			verify(scheduler, times(2)).preRestore();
			verify(scheduler).postRestore(true);
			verify(scheduler).shutdown();
			verify(scheduler).getCustomerRunningJobs();
			verify(scheduler).cancelJob("instance-1");
		}
		finally {
			JobSchedulerStaticSingleton.set(original);
		}
	}

	@Test
	public void geoIpServiceInjectableDelegatesToStaticSingleton() {
		GeoIPService original = GeoIPServiceStaticSingleton.get();
		GeoIPService service = mock(GeoIPService.class);
		IPGeolocation location = IPGeolocation.EMPTY;
		try {
			GeoIPServiceStaticSingleton.set(service);
			GeoIPServiceInjectable injectable = new GeoIPServiceInjectable();
			when(service.geolocate("8.8.8.8")).thenReturn(location);

			assertSame(location, injectable.geolocate("8.8.8.8"));
			verify(service).geolocate("8.8.8.8");
		}
		finally {
			GeoIPServiceStaticSingleton.set(original);
		}
	}

	private static ProvidedRepository getProvidedRepository() throws Exception {
		Field field = ProvidedRepositoryFactory.class.getDeclaredField("repository");
		field.setAccessible(true);
		return (ProvidedRepository) field.get(null);
	}

	private static void setProvidedRepository(ProvidedRepository repository) throws Exception {
		Field field = ProvidedRepositoryFactory.class.getDeclaredField("repository");
		field.setAccessible(true);
		field.set(null, repository);
	}

	private static MailService getMailServiceField(String name) throws Exception {
		Field field = MailServiceStaticSingleton.class.getDeclaredField(name);
		field.setAccessible(true);
		return (MailService) field.get(null);
	}

	private static void setMailServiceField(String name, MailService value) throws Exception {
		Field field = MailServiceStaticSingleton.class.getDeclaredField(name);
		field.setAccessible(true);
		field.set(null, value);
	}

	private static void setPluginManager(PF4JAddInManager manager, PluginManager pluginManager) throws Exception {
		Field field = PF4JAddInManager.class.getDeclaredField("plugInManager");
		field.setAccessible(true);
		field.set(manager, pluginManager);
	}

	private static PluginManager getPluginManager(PF4JAddInManager manager) throws Exception {
		Field field = PF4JAddInManager.class.getDeclaredField("plugInManager");
		field.setAccessible(true);
		return (PluginManager) field.get(manager);
	}
}
