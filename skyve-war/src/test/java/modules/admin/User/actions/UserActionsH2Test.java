package modules.admin.User.actions;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.sail.mock.MockWebContext;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.User.UserExtension;
import modules.admin.domain.Contact;
import modules.admin.domain.Contact.ContactType;
import modules.admin.domain.User;
import util.AbstractH2Test;

/**
 * H2-backed tests for User actions: GenerateUniqueUserName and Check validation.
 */
public class UserActionsH2Test extends AbstractH2Test {

	private DataBuilder db;
	private UserExtension userBean;
	private MockWebContext webContext;

	@BeforeEach
	void setup() throws Exception {
		db = new DataBuilder().fixture(FixtureType.crud);
		userBean = db.build(User.MODULE_NAME, User.DOCUMENT_NAME);
		webContext = new MockWebContext();
	}

	// ---- GenerateUniqueUserName ----

	@Test
	void generateUniqueUserNameWithNullContactThrowsValidationException() {
		userBean.setContact(null);

		assertThrows(ValidationException.class,
				() -> GenerateUniqueUserName.generateUniqueUserNameFromContactName(userBean));
	}

	@Test
	void generateUniqueUserNameWithContactNameReturnsUsername() throws Exception {
		modules.admin.Contact.ContactExtension contact = Contact.newInstance();
		contact.setName("John Doe");
		contact.setContactType(ContactType.person);
		contact.setEmail1("johndoe@test.com");
		contact = CORE.getPersistence().save(contact);
		userBean.setContact(contact);

		String result = GenerateUniqueUserName.generateUniqueUserNameFromContactName(userBean);
		assertThat(result, is(notNullValue()));
	}

	@Test
	void generateUniqueUserNameExecuteReturnsBean() throws Exception {
		modules.admin.Contact.ContactExtension contact = Contact.newInstance();
		contact.setName("Jane Smith");
		contact.setContactType(ContactType.person);
		contact.setEmail1("jane@test.com");
		contact = CORE.getPersistence().save(contact);
		userBean.setContact(contact);

		GenerateUniqueUserName action = new GenerateUniqueUserName();
		ServerSideActionResult<User> result = action.execute(userBean, webContext);
		assertThat(result, is(notNullValue()));
		assertThat(result.getBean(), is(notNullValue()));
	}

	// ---- Check validation ----

	@Test
	void checkWithBothNameAndEmailBlankThrowsValidationException() {
		userBean.setSearchContactName(null);
		userBean.setSearchEmail(null);

		Check action = new Check();
		assertThrows(ValidationException.class, () -> action.execute(userBean, webContext));
	}

	@Test
	void checkWithBlankStringNamesThrowsValidationException() {
		userBean.setSearchContactName("  ");
		userBean.setSearchEmail("  ");

		Check action = new Check();
		assertThrows(ValidationException.class, () -> action.execute(userBean, webContext));
	}

	@Test
	void checkWithEmailOnlyReturnsResult() throws Exception {
		userBean.setSearchContactName(null);
		userBean.setSearchEmail("test@example.com");

		Check action = new Check();
		ServerSideActionResult<UserExtension> result = action.execute(userBean, webContext);
		assertThat(result, is(notNullValue()));
	}
}
