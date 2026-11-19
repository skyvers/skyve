package org.skyve.impl.sms;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.skyve.util.SMSService;

@SuppressWarnings("static-method")
class SMSServiceTest {

	// ---- SMSServiceStaticSingleton ----

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void singletonGetReturnsNullInitially() {
		SMSServiceStaticSingleton.set(null);
		assertNull(SMSServiceStaticSingleton.get());
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void singletonSetAndGetReturnsSetService() {
		SMSService svc = new NoOpSMSService();
		SMSServiceStaticSingleton.set(svc);
		try {
			assertSame(svc, SMSServiceStaticSingleton.get());
		}
		finally {
			SMSServiceStaticSingleton.set(null);
		}
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void singletonSetDefaultSetsNoOpService() {
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
	void noOpTextWithTwoArgsReturnsFalse() {
		NoOpSMSService svc = new NoOpSMSService();
		assertFalse(svc.text("+61400000000", "Test message"));
	}

	@Test
	void noOpTextWithThreeArgsReturnsFalse() {
		NoOpSMSService svc = new NoOpSMSService();
		assertFalse(svc.text("+61400000000", "+61411111111", "Test message"));
	}
}
