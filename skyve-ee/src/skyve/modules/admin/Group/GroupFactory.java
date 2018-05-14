package modules.admin.Group;

import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.domain.Group;
import modules.admin.domain.GroupRole;

public class GroupFactory {

	@SkyveFixture(types = FixtureType.crud)
	public static Group crudInstance() throws Exception {
		Group group = new DataBuilder().build(Group.MODULE_NAME, Group.DOCUMENT_NAME);

		GroupRole role = new DataBuilder().fixture(FixtureType.crud).build(GroupRole.MODULE_NAME, GroupRole.DOCUMENT_NAME);
		group.getRoles().add(role);
		role.setParent(group);

		return group;
	}

}
