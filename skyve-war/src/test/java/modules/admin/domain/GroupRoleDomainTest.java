package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
public class GroupRoleDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderCreatesBean() throws Exception {
		GroupRole bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(GroupRole.MODULE_NAME, GroupRole.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void moduleAndDocumentNames() throws Exception {
		GroupRole bean = GroupRole.newInstance();
		assertEquals("admin", bean.getBizModule());
		assertEquals("GroupRole", bean.getBizDocument());
	}

	@Test
	void roleNameSetAndGet() throws Exception {
		GroupRole bean = GroupRole.newInstance();
		bean.setRoleName("admin.BasicUser");
		assertEquals("admin.BasicUser", bean.getRoleName());
	}

	@Test
	void bizOrdinalSetAndGet() throws Exception {
		GroupRole bean = GroupRole.newInstance();
		bean.setBizOrdinal(Integer.valueOf(5));
		assertEquals(Integer.valueOf(5), bean.getBizOrdinal());
	}
}
