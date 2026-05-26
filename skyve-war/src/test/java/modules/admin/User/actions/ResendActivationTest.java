package modules.admin.User.actions;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.DomainException;
import org.skyve.metadata.controller.ServerSideActionResult;

import modules.admin.User.UserExtension;
import modules.admin.Contact.ContactExtension;

/**
 * Tests for User ResendActivation action.
 */
@SuppressWarnings("static-method")
class ResendActivationTest {

	@Test
	void executeWithNullBeanReturnsNullBean() throws Exception {
		ResendActivation action = new ResendActivation();
		// bean == null → skips block, returns ServerSideActionResult with null bean
		ServerSideActionResult<UserExtension> result = action.execute(null, null);
		assertNotNull(result);
		assertNull(result.getBean());
	}

	@Test
	void executeWithNullContactReturnsBean() throws Exception {
		ResendActivation action = new ResendActivation();
		UserExtension user = new UserExtension();
		// contact is null → condition is false, skips block, returns bean
		ServerSideActionResult<UserExtension> result = action.execute(user, null);
		assertNotNull(result);
	}

	@Test
	void executeWithNullEmailThrowsDomainException() throws Exception {
		ResendActivation action = new ResendActivation();
		UserExtension user = new UserExtension();
		ContactExtension contact = new ContactExtension();
		contact.setEmail1(null);
		user.setContact(contact);
		// contact != null but email1 is null → throws DomainException
		assertThrows(DomainException.class, () -> action.execute(user, null));
	}
}
