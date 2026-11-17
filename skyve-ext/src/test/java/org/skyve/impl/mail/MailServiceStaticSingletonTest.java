package org.skyve.impl.mail;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.lang.reflect.Field;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.util.MailService;

@SuppressWarnings("static-method")
class MailServiceStaticSingletonTest {

	private static void clearStaticState() throws Exception {
		Field configured = MailServiceStaticSingleton.class.getDeclaredField("configuredInstance");
		configured.setAccessible(true);
		configured.set(null, null);

		Field effective = MailServiceStaticSingleton.class.getDeclaredField("effectiveInstance");
		effective.setAccessible(true);
		effective.set(null, null);
	}

	@BeforeEach
	void setUp() throws Exception {
		clearStaticState();
	}

	/** Reset static state after each test so tests do not interfere. */
	@AfterEach
	void tearDown() throws Exception {
		clearStaticState();
	}

	@Test
	void getThrowsWhenNotInitialised() {
		assertThrows(IllegalStateException.class, MailServiceStaticSingleton::get);
	}

	@Test
	void getEffectiveThrowsWhenNotInitialised() {
		assertThrows(IllegalStateException.class, MailServiceStaticSingleton::getEffective);
	}

	@Test
	void setAndGetReturnConfiguredInstance() {
		MailService impl = new SMTPMailService();
		MailServiceStaticSingleton.set(impl);
		assertNotNull(MailServiceStaticSingleton.get());
	}

	@Test
	void setAndGetEffectiveReturnsDecorated() {
		MailServiceStaticSingleton.set(new SMTPMailService());
		assertNotNull(MailServiceStaticSingleton.getEffective());
	}

	@Test
	void setDefaultInitialisesWithSMTPService() {
		MailServiceStaticSingleton.setDefault();
		assertNotNull(MailServiceStaticSingleton.get());
		assertNotNull(MailServiceStaticSingleton.getEffective());
	}
}
