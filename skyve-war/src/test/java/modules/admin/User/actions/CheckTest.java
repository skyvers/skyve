package modules.admin.User.actions;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

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

	@BeforeEach
	void setup() {
		db = new DataBuilder().fixture(FixtureType.crud);

		user1 = db.build(User.MODULE_NAME, User.DOCUMENT_NAME);
		user1.getContact().setEmail1("check1@check.com");
		user2 = db.build(User.MODULE_NAME, User.DOCUMENT_NAME);
		user2.getContact().setEmail1("check2@check.com");

		user1 = CORE.getPersistence().save(user1);
		user2 = CORE.getPersistence().save(user2);

		action = new Check();
	}

	@Test
	@SuppressWarnings("boxing")
	void testExecuteEmailSearchEmptySearchThrowsException() {
		// create the test data
		UserExtension searchUser = User.newInstance();
		searchUser.setSearchEmail(" ");

		// call the method under test
		ValidationException e = assertThrows(ValidationException.class, () -> {
			action.execute(searchUser, null);
		});

		// verify the result
		assertThat(e.getMessages().size(), is(1));
		assertThat(e.getMessages().get(0).getText(), is(Util.i18n("admin.user.actions.check.required")));
	}

	@Test
	@SuppressWarnings("boxing")
	void testExecuteEmailSearchNullSearchThrowsException() {
		// create the test data
		UserExtension searchUser = User.newInstance();
		searchUser.setSearchEmail(null);

		// call the method under test
		ValidationException e = assertThrows(ValidationException.class, () -> {
			action.execute(searchUser, null);
		});

		// verify the result
		assertThat(e.getMessages().size(), is(1));
		assertThat(e.getMessages().get(0).getText(), is(Util.i18n("admin.user.actions.check.required")));
	}

	@Test
	@SuppressWarnings("boxing")
	void testExecuteEmailSearchCleansSearchString() throws Exception {
		// create the test data
		UserExtension searchUser = User.newInstance();
		searchUser.setSearchEmail("check 2 @check.com ");

		// call the method under test
		ServerSideActionResult<UserExtension> result = action.execute(searchUser, null);

		// verify the result
		assertThat(result.getBean().getCandidateContacts().size(), is(1));
		assertThat(result.getBean().getCandidateContacts().get(0).getContact(), is(user2.getContact()));
	}

	@Test
	@SuppressWarnings("boxing")
	void testExecuteEmailSearchExactMatch() throws Exception {
		// create the test data
		UserExtension searchUser = User.newInstance();
		searchUser.setSearchEmail(user2.getContact().getEmail1());

		// call the method under test
		ServerSideActionResult<UserExtension> result = action.execute(searchUser, null);

		// verify the result
		assertThat(result.getBean().getCandidateContacts().size(), is(1));
		assertThat(result.getBean().getCandidateContacts().get(0).getContact(), is(user2.getContact()));
	}

	@Test
	@SuppressWarnings("boxing")
	void testExecuteEmailSearchMatchesForward() throws Exception {
		// create the test data
		UserExtension searchUser = User.newInstance();
		searchUser.setSearchEmail("check");

		// call the method under test
		ServerSideActionResult<UserExtension> result = action.execute(searchUser, null);
		
		// verify the result
		assertThat(result.getBean().getCandidateContacts().size(), is(2));
	}

	@Test
	@SuppressWarnings("boxing")
	void testExecuteEmailSearchMatchesWithin() throws Exception {
		// create the test data
		UserExtension searchUser = User.newInstance();
		searchUser.setSearchEmail("heck");

		// call the method under test
		ServerSideActionResult<UserExtension> result = action.execute(searchUser, null);

		// verify the result
		assertThat(result.getBean().getCandidateContacts().size(), is(2));
	}
}
