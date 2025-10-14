package modules.admin;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.metadata.MetaDataException;
import org.skyve.persistence.DocumentQuery;
import org.skyve.util.DataBuilder;
import org.skyve.util.Util;
import org.skyve.util.test.SkyveFixture;

import modules.admin.Contact.ContactExtension;
import modules.admin.Group.GroupExtension;
import modules.admin.User.UserExtension;
import modules.admin.domain.Contact;
import modules.admin.domain.Group;
import modules.admin.domain.GroupRole;
import modules.admin.domain.User;
import util.AbstractH2Test;

public class ModulesUtilTest extends AbstractH2Test {

	private DataBuilder db;
	private ContactExtension contact = null;

	@BeforeEach
	void setUpTests() {
		db = new DataBuilder().fixture(SkyveFixture.FixtureType.crud);
	}

	@AfterEach
	void tearDownTeats() {
		// nothing to see here
	}

	@Test
	void createAdminUserFromContactWithGroupShouldFailIfNoContact() {
		// given
		contact = null;

		// when
		Executable executable = () -> ModulesUtil.createAdminUserFromContactWithGroup(contact, "groupName", "homeModuleName",
				false);

		// then
		DomainException e = assertThrows(DomainException.class, executable);
		assertThat(e.getMessage(),
				is(Util.nullSafeI18n("admin.modulesUtils.createAdminUserFromContactWithGroup.exception.contact")));
	}

	@Test
	void createAdminUserFromContactWithGroupShouldFailIfNoGroupName() {
		// given
		contact = Contact.newInstance();

		// when
		Executable executable = () -> ModulesUtil.createAdminUserFromContactWithGroup(contact, null, "homeModuleName", false);

		// then
		DomainException e = assertThrows(DomainException.class, executable);
		assertThat(e.getMessage(),
				is(Util.nullSafeI18n("admin.modulesUtils.createAdminUserFromContactWithGroup.exception.groupName")));
	}

	@Test
	void createAdminUserFromContactWithGroupShouldFailIfNoValidGroupName() {
		// given
		contact = Contact.newInstance();
		GroupExtension group = Group.newInstance();
		group.setName("Admin");

		// when
		Executable executable = () -> ModulesUtil.createAdminUserFromContactWithGroup(contact, "Admin", "homeModuleName", false);

		// then
		DomainException e = assertThrows(DomainException.class, executable);
		assertThat(e.getMessage(),
				is(Util.nullSafeI18n("admin.modulesUtils.createAdminUserFromContactWithGroup.exception.invalidGroup")));
	}

	@Test
	void createAdminUserFromContactWithGroupShouldFailIfNoValidHomeModuleName() {
		// given
		contact = db.build(Contact.MODULE_NAME, Contact.DOCUMENT_NAME);

		String homeModuleName = "homeModuleName";

		GroupExtension group = db.build(Group.MODULE_NAME, Group.DOCUMENT_NAME);
		group.setName("Admin");
		CORE.getPersistence().save(group);

		// when
		Executable executable = () -> ModulesUtil.createAdminUserFromContactWithGroup(contact, "Admin", homeModuleName, false);

		// then
		MetaDataException e = assertThrows(MetaDataException.class, executable);
		assertThat(e.getMessage(), containsString("Module " + homeModuleName + " does not exist"));
	}

	@Test
	@SuppressWarnings("null")
	void createAdminUserFromContactWithGroupShould() {
		// given
		contact = db.build(Contact.MODULE_NAME, Contact.DOCUMENT_NAME);

		GroupExtension group = db.build(Group.MODULE_NAME, Group.DOCUMENT_NAME);
		group.setName("TestGroup");
		group = CORE.getPersistence().save(group);

		// when
		ModulesUtil.createAdminUserFromContactWithGroup(contact, "TestGroup", Contact.MODULE_NAME, false);

		// then
		DocumentQuery q = CORE.getPersistence().newDocumentQuery(User.MODULE_NAME, User.DOCUMENT_NAME);
		q.getFilter().addEquals(User.userNamePropertyName, contact.getEmail1());
		UserExtension result = q.beanResult();

		assertThat(result, is(notNullValue()));
		assertThat(result.getContact(), is(contact));
	}

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
