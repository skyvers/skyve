package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
public class UserAccountDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderCreatesBean() throws Exception {
		UserAccount bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(UserAccount.MODULE_NAME, UserAccount.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void moduleAndDocumentNames() throws Exception {
		UserAccount bean = UserAccount.newInstance();
		assertEquals("admin", bean.getBizModule());
		assertEquals("UserAccount", bean.getBizDocument());
	}

	@Test
	void sessionsListInitialized() throws Exception {
		UserAccount bean = UserAccount.newInstance();
		assertNotNull(bean.getSessions());
	}

	@Test
	void secondFactorPreferredMethodSetAndGet() throws Exception {
		UserAccount bean = UserAccount.newInstance();
		bean.setSecondFactorPreferredMethod(UserAccount.SecondFactorPreferredMethod.email);
		assertEquals(UserAccount.SecondFactorPreferredMethod.email, bean.getSecondFactorPreferredMethod());
	}

	@Test
	void secondFactorEnumFromCode() {
		UserAccount.SecondFactorPreferredMethod method = UserAccount.SecondFactorPreferredMethod.fromCode("A");
		assertEquals(UserAccount.SecondFactorPreferredMethod.authenticator, method);
	}

	@Test
	void secondFactorEnumValues() {
		assertNotNull(UserAccount.SecondFactorPreferredMethod.values());
		assertEquals(3, UserAccount.SecondFactorPreferredMethod.values().length);
	}
}
