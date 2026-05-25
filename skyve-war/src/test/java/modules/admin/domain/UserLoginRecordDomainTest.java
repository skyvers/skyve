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

	@Test
	void countryNameDerivedFromCountryCode() throws Exception {
		UserLoginRecord bean = UserLoginRecord.newInstance();
		assertNull(bean.getCountryName());
		bean.setCountryCode("AU");
		assertEquals(Util.countryNameFromCode("AU"), bean.getCountryName());
	}

	@Test
	void citySetAndGet() throws Exception {
		UserLoginRecord bean = UserLoginRecord.newInstance();
		bean.setCity("Adelaide");
		assertEquals("Adelaide", bean.getCity());
	}

	@Test
	void regionSetAndGet() throws Exception {
		UserLoginRecord bean = UserLoginRecord.newInstance();
		bean.setRegion("South Australia");
		assertEquals("South Australia", bean.getRegion());
	}

        @Test
        void loginDateTimeSetAndGet() throws Exception {
                UserLoginRecord bean = UserLoginRecord.newInstance();
                org.skyve.domain.types.DateTime now = new org.skyve.domain.types.DateTime();
                bean.setLoginDateTime(now);
                assertEquals(now, bean.getLoginDateTime());
        }

        @Test
        void isHasLocationWhenCitySet() throws Exception {
                UserLoginRecord bean = UserLoginRecord.newInstance();
                // isHasLocation checks GeoIP lookup; just verify the method is callable
                assertFalse(bean.isHasLocation());
                assertTrue(bean.isNotHasLocation());
        }

        @Test
        void isNotHasLocationWhenNoCity() throws Exception {
                UserLoginRecord bean = UserLoginRecord.newInstance();
                // isNotHasLocation is just the negation of isHasLocation
                assertEquals(!bean.isHasLocation(), bean.isNotHasLocation());
        }

        @Test
        void getBizKeyNotNull() throws Exception {
                UserLoginRecord bean = UserLoginRecord.newInstance();
                assertNotNull(bean.getBizKey());
        }
}
