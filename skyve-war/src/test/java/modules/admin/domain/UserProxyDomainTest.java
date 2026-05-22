package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
public class UserProxyDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderCreatesBean() throws Exception {
		UserProxy bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(UserProxy.MODULE_NAME, UserProxy.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void moduleAndDocumentNames() throws Exception {
		UserProxy bean = UserProxy.newInstance();
		assertEquals("admin", bean.getBizModule());
		assertEquals("UserProxy", bean.getBizDocument());
	}

	@Test
	void userNameSetAndGet() throws Exception {
		UserProxy bean = UserProxy.newInstance();
		bean.setUserName("proxyuser");
		assertEquals("proxyuser", bean.getUserName());
	}

	@Test
	void inactiveSetAndGet() throws Exception {
		UserProxy bean = UserProxy.newInstance();
		bean.setInactive(Boolean.FALSE);
		assertEquals(Boolean.FALSE, bean.getInactive());
	}
}
