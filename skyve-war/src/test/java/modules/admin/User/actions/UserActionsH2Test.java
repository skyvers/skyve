package modules.admin.User.actions;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.lang.reflect.Field;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.sail.mock.MockWebContext;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import jakarta.inject.Inject;
import modules.admin.User.UserService;
import modules.admin.User.UserExtension;
import modules.admin.domain.Contact;
import modules.admin.domain.Contact.ContactType;
import modules.admin.domain.User;
import modules.admin.domain.User.WizardState;
import util.AbstractH2Test;

/**
 * H2-backed tests for User actions: GenerateUniqueUserName and Check validation.
 */
class UserActionsH2Test extends AbstractH2Test {

	private DataBuilder db;
	private UserExtension userBean;
	private MockWebContext webContext;
	@Inject
	private transient UserService userService;

	@BeforeEach
	void setup() {
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

	@Test
	void newActionSeedsContactFromSearchAndAdvancesWizard() throws Exception {
		userBean.setSearchContactName("New Contact");
		userBean.setSearchEmail("new-contact-" + System.nanoTime() + "@example.com");
		userBean.setWizardState(WizardState.confirmContact);
		New action = new New();
		injectUserService(action);

		ServerSideActionResult<UserExtension> result = action.execute(userBean, webContext);

		assertThat(result.getBean(), is(userBean));
		assertTrue(userBean.getCandidateContacts().isEmpty());
		assertThat(userBean.getContact(), is(notNullValue()));
		assertThat(userBean.getContact().getName(), is("New Contact"));
		assertThat(userBean.getContact().getEmail1(), is(notNullValue()));
		assertThat(userBean.getContact().getContactType(), is(ContactType.person));
		assertThat(userBean.getWizardState(), is(WizardState.confirmUserNameAndPassword));
	}

	private void injectUserService(New action) throws Exception {
		Field field = New.class.getDeclaredField("userService");
		field.setAccessible(true);
		field.set(action, userService);
	}
}
