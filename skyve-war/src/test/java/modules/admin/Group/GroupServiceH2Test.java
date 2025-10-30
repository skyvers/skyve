package modules.admin.Group;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.Test;

import jakarta.inject.Inject;
import modules.admin.domain.GroupRole;
import util.AbstractH2Test;

public class GroupServiceH2Test extends AbstractH2Test {
	
	@Inject
	private transient GroupService groupService;

	@Test
	void configureGroup() {
		// Create test roles
		GroupRole testRole1 = GroupRole.newInstance();
		testRole1.setRoleName("admin.TestRole1");
		assertThat(testRole1, is(notNullValue()));

		GroupRole testRole2 = GroupRole.newInstance();
		testRole2.setRoleName("admin.TestRole2");
		assertThat(testRole2, is(notNullValue()));

		// New Group
		GroupExtension newGroup = groupService.configureGroup("TestGroup", "admin.TestRole1", "admin.TestRole2");
		assertThat(newGroup, is(notNullValue()));

		// Attempting to create new group with name and roles of existing group
		GroupExtension testExistingGroup = groupService.configureGroup("TestGroup", "admin.TestRole1", "admin.TestRole2");
		assertThat(testExistingGroup, is(notNullValue()));
		assertThat(testExistingGroup, is(newGroup));

		// Attempting to create new group with name of existing group but missing role
		GroupExtension testExistingGroupWithMissingRole = groupService.configureGroup("TestGroup", "admin.TestRole1");
		assertThat(testExistingGroupWithMissingRole, is(notNullValue()));
		assertThat(testExistingGroupWithMissingRole, is(testExistingGroup));
	}
}
