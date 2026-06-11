package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
class UserTokenDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderCreatesBean() {
		UserToken bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(UserToken.MODULE_NAME, UserToken.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void moduleAndDocumentNames() {
		UserToken bean = UserToken.newInstance();
		assertEquals("admin", bean.getBizModule());
		assertEquals("UserToken", bean.getBizDocument());
	}

	@Test
	void userNameSetAndGet() {
		UserToken bean = UserToken.newInstance();
		bean.setUserName("tokenuser");
		assertEquals("tokenuser", bean.getUserName());
	}

	@Test
	void seriesSetAndGet() {
		UserToken bean = UserToken.newInstance();
		bean.setSeries("series-abc-123");
		assertEquals("series-abc-123", bean.getSeries());
	}

	@Test
	void tokenSetAndGet() {
		UserToken bean = UserToken.newInstance();
		bean.setToken("tok-xyz-789");
		assertEquals("tok-xyz-789", bean.getToken());
	}

        @Test
        void getBizKeyNotNull() {
                UserToken bean = UserToken.newInstance();
                assertNotNull(bean.getBizKey());
        }

        @Test
        void lastUsedSetAndGet() {
                UserToken bean = UserToken.newInstance();
                org.skyve.domain.types.Timestamp now = new org.skyve.domain.types.Timestamp();
                bean.setLastUsed(now);
                assertEquals(now, bean.getLastUsed());
        }
}
