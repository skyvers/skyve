package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

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

	@Test
	void addAndRemoveRolesElement() throws Exception {
		Group bean = Group.newInstance();
		GroupRole role = GroupRole.newInstance();
		role.setRoleName("admin.BasicUser");
		assertTrue(bean.addRolesElement(role));
		assertEquals(1, bean.getRoles().size());
		assertTrue(bean.removeRolesElement(role));
		assertEquals(0, bean.getRoles().size());
	}

	@Test
	void addRolesElementAtIndex() throws Exception {
		Group bean = Group.newInstance();
		GroupRole role = GroupRole.newInstance();
		role.setRoleName("admin.BasicUser");
		bean.addRolesElement(0, role);
		assertEquals(1, bean.getRoles().size());
		GroupRole removed = bean.removeRolesElement(0);
		assertNotNull(removed);
	}

	@Test
	void candidateRolesListInitialized() throws Exception {
		Group bean = Group.newInstance();
		assertNotNull(bean.getCandidateRoles());
	}

	@Test
	void addAndRemoveCandidateRolesElement() throws Exception {
		Group bean = Group.newInstance();
		int initialSize = bean.getCandidateRoles().size();
		GroupRole role = GroupRole.newInstance();
		role.setRoleName("admin.BasicUser");
		assertTrue(bean.addCandidateRolesElement(role));
		assertEquals(initialSize + 1, bean.getCandidateRoles().size());
		assertTrue(bean.removeCandidateRolesElement(role));
		assertEquals(initialSize, bean.getCandidateRoles().size());
	}

	@Test
	void addCandidateRolesElementAtIndex() throws Exception {
		Group bean = Group.newInstance();
		int initialSize = bean.getCandidateRoles().size();
		GroupRole role = GroupRole.newInstance();
		role.setRoleName("admin.BasicUser");
		bean.addCandidateRolesElement(0, role);
		assertEquals(initialSize + 1, bean.getCandidateRoles().size());
		GroupRole removed = bean.removeCandidateRolesElement(0);
		assertNotNull(removed);
	}
}
