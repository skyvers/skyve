package modules.admin.UserList;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.domain.messages.ValidationException;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.Contact.ContactExtension;
import modules.admin.Group.GroupExtension;
import modules.admin.domain.Group;
import modules.admin.domain.Contact;
import modules.admin.domain.Contact.ContactType;
import modules.admin.domain.User;
import modules.admin.domain.UserList;
import util.AbstractH2Test;

/**
 * Tests deterministic validation branches of bulk user creation.
 */
@SuppressWarnings("static-method")
class BulkUserCreationJobTest extends AbstractH2Test {
	@Test
	void cancelReturnsNull() {
		assertNull(new BulkUserCreationJob().cancel());
	}

	@Test
	void kickoffJobWithoutGroupsThrowsValidationException() {
		UserList bean = UserList.newInstance();
		bean.setUserInvitiationEmailList("person@example.com");

		ValidationException exception = assertThrows(ValidationException.class,
				() -> BulkUserCreationJob.kickoffJob(bean, null));

		assertEquals("You must select at least one permission group for invited users", exception.getMessages().get(0).getText());
	}

	@Test
	void kickoffJobWithoutEmailListThrowsValidationException() {
		UserList bean = UserList.newInstance();
		bean.getUserInvitationGroups().add(Group.newInstance());

		ValidationException exception = assertThrows(ValidationException.class,
				() -> BulkUserCreationJob.kickoffJob(bean, null));

		assertEquals("Enter one or more email addresses, separated by space ( ), comma (,) or semicolon (;).",
				exception.getMessages().get(0).getText());
	}

	@Test
	void getValidatedContactsSplitsSpaceCommaAndSemicolonSeparatedEmails() {
		UserList bean = UserList.newInstance();
		bean.setUserInvitiationEmailList("one@example.com, two@example.com;three@example.com four@example.com");

		List<ContactExtension> contacts = BulkUserCreationJob.getValidatedContacts(bean);

		assertEquals(4, contacts.size());
		assertEquals("one@example.com", contacts.get(0).getEmail1());
		assertEquals("two@example.com", contacts.get(1).getName());
		assertEquals("three@example.com", contacts.get(2).getEmail1());
		assertEquals("four@example.com", contacts.get(3).getName());
	}

	@Test
	void getValidatedContactsWithInvalidEmailThrowsMeaningfulValidationException() {
		UserList bean = UserList.newInstance();
		bean.setUserInvitiationEmailList("valid@example.com invalid-email");

		ValidationException exception = assertThrows(ValidationException.class,
				() -> BulkUserCreationJob.getValidatedContacts(bean));

		assertEquals("'invalid-email' is not a valid Email Address", exception.getMessages().get(0).getText());
	}

	@Test
	void createUserFromContactReturnsNullAndLogsWhenUserAlreadyExists() throws Exception {
		String email = "bulk-existing-" + UUID.randomUUID() + "@example.com";
		DataBuilder db = new DataBuilder().fixture(FixtureType.crud);
		User existing = db.build(User.MODULE_NAME, User.DOCUMENT_NAME);
		existing.setUserName(email);
		existing.getContact().setEmail1(email);
		CORE.getPersistence().save(existing);

		ContactExtension contact = Contact.newInstance();
		contact.setName(email);
		contact.setContactType(ContactType.person);
		contact.setEmail1(email);
		List<String> log = new ArrayList<>();

		User created = BulkUserCreationJob.createUserFromContact(contact, UserList.newInstance(), log);

		assertNull(created);
		assertEquals(1, log.size());
		assertEquals("The user '" + email + "' already exists - no action will be taken", log.get(0));
	}

	@Test
	void createUserFromContactReturnsNullAndLogsWhenContactCannotBeSaved() throws Exception {
		String email = "bulk-invalid-" + UUID.randomUUID() + "@example.com";
		ContactExtension contact = Contact.newInstance();
		contact.setEmail1(email);
		UserList bean = UserList.newInstance();
		List<String> log = new ArrayList<>();

		User created = BulkUserCreationJob.createUserFromContact(contact, bean, log);

		assertNull(created);
		assertEquals(1, log.size());
		assertTrue(log.get(0).contains("could not be created"));
	}

	@Test
	void createUserFromContactCreatesUserWithDefaultModuleWhenValid() throws Exception {
		String email = "bulk-create-" + UUID.randomUUID() + "@example.com";
		ContactExtension contact = Contact.newInstance();
		contact.setName(email);
		contact.setContactType(ContactType.person);
		contact.setEmail1(email);
		UserList bean = UserList.newInstance();
		bean.setDefaultModuleName("admin");
		bean.getUserInvitationGroups().add(savedGroup());
		List<String> log = new ArrayList<>();

		User created = BulkUserCreationJob.createUserFromContact(contact, bean, log);

		assertEquals(email, created.getUserName());
		assertEquals("admin", created.getHomeModule());
		assertEquals(Boolean.TRUE, created.getPasswordExpired());
		assertTrue(log.isEmpty());
	}

	@Test
	void executeCreatesUsersWithoutSendingInvitationsWhenBulkEmailDisabled() throws Exception {
		String email = "bulk-execute-" + UUID.randomUUID() + "@example.com";
		UserList bean = UserList.newInstance();
		bean.setUserInvitiationEmailList(email);
		bean.setBulkCreateWithEmail(Boolean.FALSE);
		bean.getUserInvitationGroups().add(savedGroup());
		BulkUserCreationJob job = new BulkUserCreationJob();
		job.setBean(bean);

		job.execute();

		assertEquals(100, job.getPercentComplete());
		assertEquals("Job to create new users has commenced", job.getLog().get(0));
		assertTrue(job.getLog().stream().anyMatch(entry -> entry.contains("New user '" + email + "'created ok")));
		assertTrue(job.getLog().get(job.getLog().size() - 1).contains("1 users created"));
	}

	private static GroupExtension savedGroup() {
		GroupExtension group = new DataBuilder().fixture(FixtureType.crud).build(Group.MODULE_NAME, Group.DOCUMENT_NAME);
		group.setName("bulk-" + UUID.randomUUID().toString().substring(0, 8));
		return CORE.getPersistence().save(group);
	}
}
