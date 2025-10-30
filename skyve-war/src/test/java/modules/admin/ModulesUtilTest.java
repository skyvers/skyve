package modules.admin;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.Test;

import modules.admin.Group.GroupExtension;
import modules.admin.domain.GroupRole;
import util.AbstractH2Test;

public class ModulesUtilTest extends AbstractH2Test {

	@Test
	@SuppressWarnings("static-method")
	void configureGroup() {
		// Create test roles
		GroupRole testRole1 = GroupRole.newInstance();
		testRole1.setRoleName("admin.TestRole1");
		assertThat(testRole1, is(notNullValue()));

		GroupRole testRole2 = GroupRole.newInstance();
		testRole2.setRoleName("admin.TestRole2");
		assertThat(testRole2, is(notNullValue()));

		// New Group
		GroupExtension newGroup = ModulesUtil.configureGroup("TestGroup", "admin.TestRole1", "admin.TestRole2");
		assertThat(newGroup, is(notNullValue()));

		// Attempting to create new group with name and roles of existing group
		GroupExtension testExistingGroup = ModulesUtil.configureGroup("TestGroup", "admin.TestRole1", "admin.TestRole2");
		assertThat(testExistingGroup, is(notNullValue()));
		assertThat(testExistingGroup, is(newGroup));

		// Attempting to create new group with name of existing group but missing role
		GroupExtension testExistingGroupWithMissingRole = ModulesUtil.configureGroup("TestGroup", "admin.TestRole1");
		assertThat(testExistingGroupWithMissingRole, is(notNullValue()));
		assertThat(testExistingGroupWithMissingRole, is(testExistingGroup));
	}
}
