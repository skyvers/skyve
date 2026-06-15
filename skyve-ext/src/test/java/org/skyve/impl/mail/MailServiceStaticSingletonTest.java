package org.skyve.impl.mail;

import static org.junit.Assert.assertNotNull;

import java.lang.reflect.Field;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.skyve.util.MailService;

@SuppressWarnings("static-method")
public class MailServiceStaticSingletonTest {

	private static void clearStaticState() throws Exception {
		Field configured = MailServiceStaticSingleton.class.getDeclaredField("configuredInstance");
		configured.setAccessible(true);
		configured.set(null, null);

		Field effective = MailServiceStaticSingleton.class.getDeclaredField("effectiveInstance");
		effective.setAccessible(true);
		effective.set(null, null);
	}

	@Before
	public void setUp() throws Exception {
		clearStaticState();
	}

	/** Reset static state after each test so tests do not interfere. */
	@After
	public void tearDown() throws Exception {
		clearStaticState();
	}

	@Test(expected = IllegalStateException.class)
	public void getThrowsWhenNotInitialised() {
		MailServiceStaticSingleton.get();
	}

	@Test(expected = IllegalStateException.class)
	public void getEffectiveThrowsWhenNotInitialised() {
		MailServiceStaticSingleton.getEffective();
	}

	@Test
	public void setAndGetReturnConfiguredInstance() {
		MailService impl = new SMTPMailService();
		MailServiceStaticSingleton.set(impl);
		assertNotNull(MailServiceStaticSingleton.get());
	}

	@Test
	public void setAndGetEffectiveReturnsDecorated() {
		MailServiceStaticSingleton.set(new SMTPMailService());
		assertNotNull(MailServiceStaticSingleton.getEffective());
	}

	@Test
	public void setDefaultInitialisesWithSMTPService() {
		MailServiceStaticSingleton.setDefault();
		assertNotNull(MailServiceStaticSingleton.get());
		assertNotNull(MailServiceStaticSingleton.getEffective());
	}
}
