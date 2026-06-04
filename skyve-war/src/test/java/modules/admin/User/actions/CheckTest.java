package modules.admin.User.actions;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.UUID;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.util.DataBuilder;
import org.skyve.util.Util;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.User.UserExtension;
import modules.admin.domain.User;
import util.AbstractH2Test;

class CheckTest extends AbstractH2Test {
	private DataBuilder db;
	private UserExtension user1, user2;
	private Check action;
	private String searchToken;

	@BeforeEach
	void setup() {
		db = new DataBuilder().fixture(FixtureType.crud);
		searchToken = "check" + UUID.randomUUID().toString().replace("-", "");

		user1 = db.build(User.MODULE_NAME, User.DOCUMENT_NAME);
		user1.getContact().setEmail1(searchToken + "1@check.com");
		user2 = db.build(User.MODULE_NAME, User.DOCUMENT_NAME);
		user2.getContact().setEmail1(searchToken + "2@check.com");

		user1 = CORE.getPersistence().save(user1);
		user2 = CORE.getPersistence().save(user2);

		action = new Check();
	}

	@Test
	void testExecuteEmailSearchEmptySearchThrowsException() {
		// create the test data
		UserExtension searchUser = User.newInstance();
		searchUser.setSearchEmail(" ");

		// call the method under test
		ValidationException e = assertThrows(ValidationException.class, () -> {
			action.execute(searchUser, null);
		});

		// verify the result
		assertEquals(1, e.getMessages().size());
		assertThat(e.getMessages().get(0).getText(), is(Util.i18n("admin.user.actions.check.required")));
	}

	@Test
	void testExecuteEmailSearchNullSearchThrowsException() {
		// create the test data
		UserExtension searchUser = User.newInstance();
		searchUser.setSearchEmail(null);

		// call the method under test
		ValidationException e = assertThrows(ValidationException.class, () -> {
			action.execute(searchUser, null);
		});

		// verify the result
		assertEquals(1, e.getMessages().size());
		assertThat(e.getMessages().get(0).getText(), is(Util.i18n("admin.user.actions.check.required")));
	}

	@Test
	void testExecuteEmailSearchCleansSearchString() throws Exception {
		// create the test data
		UserExtension searchUser = User.newInstance();
		searchUser.setSearchEmail(searchToken + " 2 @check.com ");

		// call the method under test
		ServerSideActionResult<UserExtension> result = action.execute(searchUser, null);

		// verify the result
		assertEquals(1, result.getBean().getCandidateContacts().size());
		assertThat(result.getBean().getCandidateContacts().get(0).getContact(), is(user2.getContact()));
	}

	@Test
	void testExecuteEmailSearchExactMatch() throws Exception {
		// create the test data
		UserExtension searchUser = User.newInstance();
		searchUser.setSearchEmail(user2.getContact().getEmail1());

		// call the method under test
		ServerSideActionResult<UserExtension> result = action.execute(searchUser, null);

		// verify the result
		assertEquals(1, result.getBean().getCandidateContacts().size());
		assertThat(result.getBean().getCandidateContacts().get(0).getContact(), is(user2.getContact()));
	}

	@Test
	void testExecuteEmailSearchMatchesForward() throws Exception {
		// create the test data
		UserExtension searchUser = User.newInstance();
		searchUser.setSearchEmail(searchToken);

		// call the method under test
		ServerSideActionResult<UserExtension> result = action.execute(searchUser, null);
		
		// verify the result
		assertEquals(2, result.getBean().getCandidateContacts().size());
	}

	@Test
	void testExecuteEmailSearchMatchesWithin() throws Exception {
		// create the test data
		UserExtension searchUser = User.newInstance();
		searchUser.setSearchEmail(searchToken.substring(1));

		// call the method under test
		ServerSideActionResult<UserExtension> result = action.execute(searchUser, null);

		// verify the result
		assertEquals(2, result.getBean().getCandidateContacts().size());
	}
}
