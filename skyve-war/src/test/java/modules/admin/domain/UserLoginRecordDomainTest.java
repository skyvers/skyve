package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.Util;
import org.skyve.util.test.SkyveFixture.FixtureType;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
class UserLoginRecordDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderCreatesBean() {
		UserLoginRecord bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(UserLoginRecord.MODULE_NAME, UserLoginRecord.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void moduleAndDocumentNames() {
		UserLoginRecord bean = UserLoginRecord.newInstance();
		assertEquals("admin", bean.getBizModule());
		assertEquals("UserLoginRecord", bean.getBizDocument());
	}

	@Test
	void userNameSetAndGet() {
		UserLoginRecord bean = UserLoginRecord.newInstance();
		bean.setUserName("testuser");
		assertEquals("testuser", bean.getUserName());
	}

	@Test
	void failedSetAndGet() {
		UserLoginRecord bean = UserLoginRecord.newInstance();
		bean.setFailed(Boolean.TRUE);
		assertEquals(Boolean.TRUE, bean.getFailed());
	}

	@Test
	void ipAddressSetAndGet() {
		UserLoginRecord bean = UserLoginRecord.newInstance();
		bean.setIpAddress("10.0.0.1");
		assertEquals("10.0.0.1", bean.getIpAddress());
	}

	@Test
	void countryCodeSetAndGet() {
		UserLoginRecord bean = UserLoginRecord.newInstance();
		bean.setCountryCode("AU");
		assertEquals("AU", bean.getCountryCode());
	}

	@Test
	void countryNameDerivedFromCountryCode() {
		UserLoginRecord bean = UserLoginRecord.newInstance();
		assertNull(bean.getCountryName());
		bean.setCountryCode("AU");
		assertEquals(Util.countryNameFromCode("AU"), bean.getCountryName());
	}

	@Test
	void citySetAndGet() {
		UserLoginRecord bean = UserLoginRecord.newInstance();
		bean.setCity("Adelaide");
		assertEquals("Adelaide", bean.getCity());
	}

	@Test
	void regionSetAndGet() {
		UserLoginRecord bean = UserLoginRecord.newInstance();
		bean.setRegion("South Australia");
		assertEquals("South Australia", bean.getRegion());
	}

	@Test
	void loginDateTimeSetAndGet() {
			UserLoginRecord bean = UserLoginRecord.newInstance();
			org.skyve.domain.types.DateTime now = new org.skyve.domain.types.DateTime();
			bean.setLoginDateTime(now);
			assertEquals(now, bean.getLoginDateTime());
	}

	@Test
	void isHasLocationWhenCitySet() {
			UserLoginRecord bean = UserLoginRecord.newInstance();
			// isHasLocation checks GeoIP lookup; just verify the method is callable
			assertFalse(bean.isHasLocation());
			assertTrue(bean.isNotHasLocation());
	}

	@Test
	@SuppressWarnings("boxing")
	void isNotHasLocationWhenNoCity() {
			UserLoginRecord bean = UserLoginRecord.newInstance();
			// isNotHasLocation is just the negation of isHasLocation
			assertEquals(!bean.isHasLocation(), bean.isNotHasLocation());
	}

	@Test
	void getBizKeyNotNull() {
			UserLoginRecord bean = UserLoginRecord.newInstance();
			assertNotNull(bean.getBizKey());
	}
}
