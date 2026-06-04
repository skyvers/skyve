package modules.admin.UserList;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ValidationException;

import modules.admin.Contact.ContactExtension;
import modules.admin.domain.Group;
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
}
