package modules.admin.Communication.actions;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.domain.Communication;
import util.AbstractH2Test;

/**
 * Tests for the AddUnsubscribeLink action.
 */
class AddUnsubscribeLinkH2Test extends AbstractH2Test {

	private DataBuilder db;
	private AddUnsubscribeLink action;

	@BeforeEach
	void setup() {
		db = new DataBuilder().fixture(FixtureType.crud);
		action = new AddUnsubscribeLink();
	}

	@Test
	void testExecuteAddsLinkToNullBody() throws Exception {
		// setup the test data
		Communication communication = db.build(Communication.MODULE_NAME, Communication.DOCUMENT_NAME);
		communication.setBody(null);

		// call the method under test
		ServerSideActionResult<Communication> result = action.execute(communication, null);

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.getBean(), is(notNullValue()));
		assertThat(result.getBean().getBody(), is(AddUnsubscribeLink.UNSUBSCRIBE_LINK));
	}

	@Test
	void testExecuteAppendsLinkToExistingBody() throws Exception {
		// setup the test data
		Communication communication = db.build(Communication.MODULE_NAME, Communication.DOCUMENT_NAME);
		String originalBody = "<p>Hello, this is a test communication.</p>";
		communication.setBody(originalBody);

		// call the method under test
		ServerSideActionResult<Communication> result = action.execute(communication, null);

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.getBean(), is(notNullValue()));
		String expectedBody = originalBody + "<p/>" + AddUnsubscribeLink.UNSUBSCRIBE_LINK;
		assertThat(result.getBean().getBody(), is(expectedBody));
	}

	@Test
	void testExecuteDoesNotAddDuplicateLink() throws Exception {
		// setup the test data
		Communication communication = db.build(Communication.MODULE_NAME, Communication.DOCUMENT_NAME);
		String bodyWithLink = "<p>Test body</p>" + AddUnsubscribeLink.UNSUBSCRIBE_LINK;
		communication.setBody(bodyWithLink);

		// call the method under test
		ServerSideActionResult<Communication> result = action.execute(communication, null);

		// verify the result - body should remain unchanged
		assertThat(result, is(notNullValue()));
		assertThat(result.getBean(), is(notNullValue()));
		assertThat(result.getBean().getBody(), is(bodyWithLink));
	}

	@Test
	void testExecuteDoesNotAddDuplicateLinkCaseInsensitive() throws Exception {
		// setup the test data
		Communication communication = db.build(Communication.MODULE_NAME, Communication.DOCUMENT_NAME);
		// Link with different case
		String bodyWithLink = "<p>Test body</p><A HREF=\"{unsubscribeUrl}\">UNSUBSCRIBE</A>";
		communication.setBody(bodyWithLink);

		// call the method under test
		ServerSideActionResult<Communication> result = action.execute(communication, null);

		// verify the result - body should remain unchanged since it contains the link (case insensitive)
		assertThat(result, is(notNullValue()));
		assertThat(result.getBean(), is(notNullValue()));
		assertThat(result.getBean().getBody(), is(bodyWithLink));
	}

	@Test
	void testExecuteAddsLinkToEmptyStringBody() throws Exception {
		// setup the test data
		Communication communication = db.build(Communication.MODULE_NAME, Communication.DOCUMENT_NAME);
		communication.setBody("");

		// call the method under test
		ServerSideActionResult<Communication> result = action.execute(communication, null);

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.getBean(), is(notNullValue()));
		assertThat(result.getBean().getBody(), is("<p/>" + AddUnsubscribeLink.UNSUBSCRIBE_LINK));
	}
}
