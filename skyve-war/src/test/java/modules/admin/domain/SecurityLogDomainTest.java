package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
class SecurityLogDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderCreatesBean() {
		SecurityLog bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(SecurityLog.MODULE_NAME, SecurityLog.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void moduleAndDocumentNames() {
		SecurityLog bean = SecurityLog.newInstance();
		assertEquals("admin", bean.getBizModule());
		assertEquals("SecurityLog", bean.getBizDocument());
	}

	@Test
	void usernameSetAndGet() {
		SecurityLog bean = SecurityLog.newInstance();
		bean.setUsername("testuser");
		assertEquals("testuser", bean.getUsername());
	}

	@Test
	void sourceIPSetAndGet() {
		SecurityLog bean = SecurityLog.newInstance();
		bean.setSourceIP("192.168.1.100");
		assertEquals("192.168.1.100", bean.getSourceIP());
	}

	@Test
	void eventTypeSetAndGet() {
		SecurityLog bean = SecurityLog.newInstance();
		bean.setEventType("LOGIN_SUCCESS");
		assertEquals("LOGIN_SUCCESS", bean.getEventType());
	}

	@Test
	void eventMessageSetAndGet() {
		SecurityLog bean = SecurityLog.newInstance();
		bean.setEventMessage("User authenticated successfully");
		assertEquals("User authenticated successfully", bean.getEventMessage());
	}

	@Test
	void threadIdSetAndGet() {
		SecurityLog bean = SecurityLog.newInstance();
		bean.setThreadId(Long.valueOf(42L));
		assertEquals(Long.valueOf(42L), bean.getThreadId());
	}

	@Test
	void provenanceSetAndGet() {
		SecurityLog bean = SecurityLog.newInstance();
		bean.setProvenance("web");
		assertEquals("web", bean.getProvenance());
	}
}
