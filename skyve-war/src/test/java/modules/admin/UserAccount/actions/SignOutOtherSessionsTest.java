package modules.admin.UserAccount.actions;

import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.sail.mock.MockWebContext;
import org.skyve.impl.web.WebContainer;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;

import modules.admin.domain.UserAccount;
import util.AbstractH2Test;

/**
 * The generated action test calls the action with no request, which it needs (the cookie and session to keep);
 * this one gives it a request. UserAccountFactory stops generateDomain from generating that test.
 * SignOutOtherSessionsH2Test covers the behaviour.
 */
class SignOutOtherSessionsTest extends AbstractH2Test {
	@BeforeEach
	@SuppressWarnings("static-method")
	void setRequest() {
		WebContainer.setHttpServletRequestResponse(new MockHttpServletRequest(), new MockHttpServletResponse());
	}

	@AfterEach
	@SuppressWarnings("static-method")
	void clearRequest() {
		WebContainer.clear();
	}

	@Test
	@SuppressWarnings("static-method")
	void executeWithARequestReturnsTheAccount() throws Exception {
		UserAccount account = UserAccount.newInstance();

		UserAccount result = new SignOutOtherSessions().execute(account, new MockWebContext()).getBean();

		assertNotNull(result);
	}
}
