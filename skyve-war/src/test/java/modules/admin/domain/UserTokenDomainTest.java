package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
public class UserTokenDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderCreatesBean() throws Exception {
		UserToken bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(UserToken.MODULE_NAME, UserToken.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void moduleAndDocumentNames() throws Exception {
		UserToken bean = UserToken.newInstance();
		assertEquals("admin", bean.getBizModule());
		assertEquals("UserToken", bean.getBizDocument());
	}

	@Test
	void userNameSetAndGet() throws Exception {
		UserToken bean = UserToken.newInstance();
		bean.setUserName("tokenuser");
		assertEquals("tokenuser", bean.getUserName());
	}

	@Test
	void seriesSetAndGet() throws Exception {
		UserToken bean = UserToken.newInstance();
		bean.setSeries("series-abc-123");
		assertEquals("series-abc-123", bean.getSeries());
	}

	@Test
	void tokenSetAndGet() throws Exception {
		UserToken bean = UserToken.newInstance();
		bean.setToken("tok-xyz-789");
		assertEquals("tok-xyz-789", bean.getToken());
	}
}
