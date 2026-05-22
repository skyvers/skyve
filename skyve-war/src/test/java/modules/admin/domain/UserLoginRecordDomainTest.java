package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
public class UserLoginRecordDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderCreatesBean() throws Exception {
		UserLoginRecord bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(UserLoginRecord.MODULE_NAME, UserLoginRecord.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void moduleAndDocumentNames() throws Exception {
		UserLoginRecord bean = UserLoginRecord.newInstance();
		assertEquals("admin", bean.getBizModule());
		assertEquals("UserLoginRecord", bean.getBizDocument());
	}

	@Test
	void userNameSetAndGet() throws Exception {
		UserLoginRecord bean = UserLoginRecord.newInstance();
		bean.setUserName("testuser");
		assertEquals("testuser", bean.getUserName());
	}

	@Test
	void failedSetAndGet() throws Exception {
		UserLoginRecord bean = UserLoginRecord.newInstance();
		bean.setFailed(Boolean.TRUE);
		assertEquals(Boolean.TRUE, bean.getFailed());
	}

	@Test
	void ipAddressSetAndGet() throws Exception {
		UserLoginRecord bean = UserLoginRecord.newInstance();
		bean.setIpAddress("10.0.0.1");
		assertEquals("10.0.0.1", bean.getIpAddress());
	}

	@Test
	void countryCodeSetAndGet() throws Exception {
		UserLoginRecord bean = UserLoginRecord.newInstance();
		bean.setCountryCode("AU");
		assertEquals("AU", bean.getCountryCode());
	}
}
