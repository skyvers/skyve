package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
public class GroupDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderCreatesBean() throws Exception {
		Group bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(Group.MODULE_NAME, Group.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void moduleAndDocumentNames() throws Exception {
		Group bean = Group.newInstance();
		assertEquals("admin", bean.getBizModule());
		assertEquals("Group", bean.getBizDocument());
	}

	@Test
	void nameSetAndGet() throws Exception {
		Group bean = Group.newInstance();
		bean.setName("Administrators");
		assertEquals("Administrators", bean.getName());
	}

	@Test
	void descriptionSetAndGet() throws Exception {
		Group bean = Group.newInstance();
		bean.setDescription("Admin group description");
		assertEquals("Admin group description", bean.getDescription());
	}

	@Test
	void rolesListInitialized() throws Exception {
		Group bean = Group.newInstance();
		assertNotNull(bean.getRoles());
	}
}
