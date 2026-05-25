package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.domain.UserMonthlyHits.Device;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
public class UserMonthlyHitsDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderCreatesBean() throws Exception {
		UserMonthlyHits bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(UserMonthlyHits.MODULE_NAME, UserMonthlyHits.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void moduleAndDocumentNames() throws Exception {
		UserMonthlyHits bean = UserMonthlyHits.newInstance();
		assertEquals("admin", bean.getBizModule());
		assertEquals("UserMonthlyHits", bean.getBizDocument());
	}

	@Test
	void userNameSetAndGet() throws Exception {
		UserMonthlyHits bean = UserMonthlyHits.newInstance();
		bean.setUserName("testuser");
		assertEquals("testuser", bean.getUserName());
	}

	@Test
	void hitYearSetAndGet() throws Exception {
		UserMonthlyHits bean = UserMonthlyHits.newInstance();
		bean.setHitYear(Integer.valueOf(2024));
		assertEquals(Integer.valueOf(2024), bean.getHitYear());
	}

	@Test
	void hitMonthSetAndGet() throws Exception {
		UserMonthlyHits bean = UserMonthlyHits.newInstance();
		bean.setHitMonth(Integer.valueOf(6));
		assertEquals(Integer.valueOf(6), bean.getHitMonth());
	}

        @Test
        @SuppressWarnings("static-method")
        void deviceFromCode() {
                assertEquals(Device.phone, Device.fromCode("P"));
                assertEquals(Device.desktop, Device.fromCode("D"));
        }

        @Test
        @SuppressWarnings("static-method")
        void deviceFromCodeUnknownReturnsNull() {
                assertNull(Device.fromCode("Z"));
        }

        @Test
        @SuppressWarnings("static-method")
        void deviceToCode() {
                assertEquals("P", Device.phone.toCode());
                assertEquals("T", Device.tablet.toCode());
        }

        @Test
        @SuppressWarnings("static-method")
        void deviceToDomainValues() {
                assertEquals(4, Device.toDomainValues().size());
        }

        @Test
        @SuppressWarnings("static-method")
        void deviceFromLocalisedDescription() {
                assertNotNull(Device.fromLocalisedDescription(Device.phone.toLocalisedDescription()));
        }

        @Test
        @SuppressWarnings("static-method")
        void deviceFromLocalisedDescriptionUnknownReturnsNull() {
                assertNull(Device.fromLocalisedDescription("ZZZnotexist"));
        }
}
