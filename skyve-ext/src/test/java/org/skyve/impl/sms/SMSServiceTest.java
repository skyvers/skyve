package org.skyve.impl.sms;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;

import org.junit.Test;
import org.skyve.util.SMSService;

@SuppressWarnings("static-method")
public class SMSServiceTest {

	// ---- SMSServiceStaticSingleton ----

	@Test
	public void singletonGetReturnsNullInitially() {
		SMSServiceStaticSingleton.set(null);
		assertNull(SMSServiceStaticSingleton.get());
	}

	@Test
	public void singletonSetAndGetReturnsSetService() {
		SMSService svc = new NoOpSMSService();
		SMSServiceStaticSingleton.set(svc);
		try {
			assertSame(svc, SMSServiceStaticSingleton.get());
		}
		finally {
			SMSServiceStaticSingleton.set(null);
		}
	}

	@Test
	public void singletonSetDefaultSetsNoOpService() {
		SMSServiceStaticSingleton.setDefault();
		try {
			assertNotNull(SMSServiceStaticSingleton.get());
		}
		finally {
			SMSServiceStaticSingleton.set(null);
		}
	}

	// ---- NoOpSMSService ----

	@Test
	public void noOpTextWithTwoArgsReturnsFalse() {
		NoOpSMSService svc = new NoOpSMSService();
		assertFalse(svc.text("+61400000000", "Test message"));
	}

	@Test
	public void noOpTextWithThreeArgsReturnsFalse() {
		NoOpSMSService svc = new NoOpSMSService();
		assertFalse(svc.text("+61400000000", "+61411111111", "Test message"));
	}
}
