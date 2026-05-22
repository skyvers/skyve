package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
public class UserDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderCreatesUser() throws Exception {
		User bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(User.MODULE_NAME, User.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void moduleAndDocumentNames() throws Exception {
		User bean = User.newInstance();
		assertEquals("admin", bean.getBizModule());
		assertEquals("User", bean.getBizDocument());
	}

	@Test
	void userNameSetAndGet() throws Exception {
		User bean = User.newInstance();
		bean.setUserName("testuser");
		assertEquals("testuser", bean.getUserName());
	}

	@Test
	void passwordHashSetAndGet() throws Exception {
		User bean = User.newInstance();
		bean.setPassword("hash123");
		assertEquals("hash123", bean.getPassword());
	}

	@Test
	void activeFlagSetAndGet() throws Exception {
		User bean = User.newInstance();
		bean.setInactive(Boolean.TRUE);
		assertEquals(Boolean.TRUE, bean.getInactive());
		bean.setInactive(Boolean.FALSE);
		assertEquals(Boolean.FALSE, bean.getInactive());
	}

	@Test
	void wizardStateEnumValues() {
		assertNotNull(User.WizardState.values());
		assertEquals("confirmContact", User.WizardState.confirmContact.toCode());
	}

	@Test
	void groupSelectionEnumValues() {
		assertNotNull(User.GroupSelection.values());
	}
}
