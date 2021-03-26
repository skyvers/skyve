package modules.admin;

import static org.hamcrest.CoreMatchers.*;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.skyve.CORE;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.MetaDataException;
import org.skyve.persistence.DocumentQuery;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture;

import modules.admin.Group.GroupExtension;
import modules.admin.User.UserExtension;
import modules.admin.domain.Contact;
import modules.admin.domain.Group;
import modules.admin.domain.User;
import util.AbstractH2TestForJUnit5;

public class ModulesUtilTest extends AbstractH2TestForJUnit5 {

	private DataBuilder db;
	private Contact contact = null;

	@BeforeEach
	void setUpTests() {
		db = new DataBuilder().fixture(SkyveFixture.FixtureType.crud);
	}

	@AfterEach
	void tearDownTeats() {
	}

	@Test
	void createAdminUserFromContactWithGroupShouldFailIfNoContact() {
		// given
		contact = null;

		// when
		Executable executable = () -> ModulesUtil.createAdminUserFromContactWithGroup(contact, "groupName", "homeModuleName",
				false);

		// then
		assertThrows(ValidationException.class, executable);

	}

	@Test
	void createAdminUserFromContactWithGroupShouldFailIfNoGroupName() {
		// given
		Contact contact = Contact.newInstance();

		// when
		Executable executable = () -> ModulesUtil.createAdminUserFromContactWithGroup(contact, null, "homeModuleName", false);

		// then
		assertThrows(ValidationException.class, executable);
	}

	@Test
	void createAdminUserFromContactWithGroupShouldFailIfNoValidGroupName() {
		// given
		Contact contact = Contact.newInstance();
		GroupExtension group = GroupExtension.newInstance();
		group.setName("Admin");

		// when
		Executable executable = () -> ModulesUtil.createAdminUserFromContactWithGroup(contact, "Admin", "homeModuleName", false);

		// then
		assertThrows(ValidationException.class, executable);
	}

	@Test
	void createAdminUserFromContactWithGroupShouldFailIfNoValidHomeModuleName() {
		// given
		Contact contact = db.build(Contact.MODULE_NAME, Contact.DOCUMENT_NAME);

		GroupExtension group = db.build(Group.MODULE_NAME, Group.DOCUMENT_NAME);
		group.setName("Admin");
		CORE.getPersistence().save(group);

		// when
		Executable executable = () -> ModulesUtil.createAdminUserFromContactWithGroup(contact, "Admin", "homeModuleName", false);

		// then
		assertThrows(MetaDataException.class, executable);
	}

	@Test
	void createAdminUserFromContactWithGroupShould() {
		// given
		Contact contact = db.build(Contact.MODULE_NAME, Contact.DOCUMENT_NAME);

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
}
