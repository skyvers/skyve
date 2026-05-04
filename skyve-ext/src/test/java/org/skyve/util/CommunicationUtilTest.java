package org.skyve.util;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.app.AppConstants;
import org.skyve.domain.app.admin.Communication;
import org.skyve.domain.app.admin.Communication.ActionType;
import org.skyve.domain.app.admin.Tag;
import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.types.DateTime;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.util.CommunicationUtil.CommunicationCalendarItem;
import org.skyve.util.CommunicationUtil.ResponseMode;

class CommunicationUtilTest {

	private Communication communication;

	@BeforeEach
	public void setup() {
		// setup mocks
		communication = mock(Communication.class);
	}

	@Test
	void testGetResultsThrowsWhenModuleMissing() {
		ValidationException e = assertThrows(ValidationException.class, () -> CommunicationUtil.getResults(communication));
		assertThat(e.getMessage(), containsString(AppConstants.MODULE_NAME_ATTRIBUTE_NAME));
	}

	@Test
	void testGetResultsThrowsWhenDocumentMissing() {
		when(communication.getModuleName()).thenReturn("admin");

		ValidationException e = assertThrows(ValidationException.class, () -> CommunicationUtil.getResults(communication));
		assertThat(e.getMessage(), containsString(AppConstants.DOCUMENT_NAME_ATTRIBUTE_NAME));
	}

	@Test
	void testGetResultsThrowsWhenTagMissing() {
		when(communication.getModuleName()).thenReturn("admin");
		when(communication.getDocumentName()).thenReturn("Communication");

		ValidationException e = assertThrows(ValidationException.class, () -> CommunicationUtil.getResults(communication));
		assertThat(e.getMessage(), containsString(AppConstants.TAG_ATTRIBUTE_NAME));
	}

	@Test
	void testGetResultsWithSendImmediatelyAction() throws Exception {
		communication = communicationForResults(ActionType.sendImmediately, 3L);

		String result = CommunicationUtil.getResults(communication);

		assertThat(result, is("3 communications for Communication will be sent immediately."));
	}

	@Test
	void testGetResultsWithSaveForBulkSendAction() throws Exception {
		communication = communicationForResults(ActionType.saveForBulkSend, 7L);

		String result = CommunicationUtil.getResults(communication);

		assertThat(result, is("7 communications for Communication will be created as a batch for download."));
	}

	@Test
	void testGetResultsWithTestBindingsAction() throws Exception {
		communication = communicationForResults(ActionType.testBindingsAndOutput, 1L);

		String result = CommunicationUtil.getResults(communication);

		assertThat(result, is("1 communications for Communication will be tested."));
	}

	@SuppressWarnings("static-method")
	@Test
	void testFormatCommunicationMessageReturnsNullForNullExpression() throws Exception {
		assertThat(CommunicationUtil.formatCommunicationMessage(null, null), is((String) null));
	}

	@SuppressWarnings({ "static-method", "boxing" })
	@Test
	void testFormatCommunicationMessageReplacesResetPasswordPlaceholder() throws Exception {
		String expression = "Use this link: " + CommunicationUtil.SPECIAL_LOGOUT_URL;

		String result = CommunicationUtil.formatCommunicationMessage(null, expression);

		assertThat(result.contains(CommunicationUtil.SPECIAL_LOGOUT_URL), is(false));
		assertThat(result, containsString("resetPassword.jsp"));
	}

	@SuppressWarnings("static-method")
	@Test
	void testFormatCommunicationMessageReplacesBeanUrlAndContextForFirstBean() throws Exception {
		Bean bean = mock(Bean.class);
		when(bean.getBizModule()).thenReturn("admin");
		when(bean.getBizDocument()).thenReturn("Communication");
		when(bean.getBizId()).thenReturn("12345");
		String expression = "View " + CommunicationUtil.SPECIAL_BEAN_URL + " from " + CommunicationUtil.SPECIAL_CONTEXT;

		String result = CommunicationUtil.formatCommunicationMessage(null, expression, bean);

		assertThat(result, containsString(Util.getDocumentUrl(bean)));
		assertThat(result, containsString(Util.getBaseUrl()));
		assertThat(result.contains(CommunicationUtil.SPECIAL_BEAN_URL), is(false));
		assertThat(result.contains(CommunicationUtil.SPECIAL_CONTEXT), is(false));
	}

	@SuppressWarnings("static-method")
	@Test
	void testFormatCommunicationMessageLeavesBeanPlaceholdersWhenFirstBeanIsNull() throws Exception {
		String expression = "No bindings here";

		String result = CommunicationUtil.formatCommunicationMessage(null, expression, new Bean[] { null });

		assertThat(result, is(expression));
	}

	@SuppressWarnings("static-method")
	@Test
	void testHtmlEncloseWrapsAndConvertsPlainText() throws Exception {
		String result = (String) invokePrivate("htmlEnclose", new Class<?>[] { String.class }, "Line one\nLine two");

		assertThat(result, is("<html><body>Line one<br>\nLine two<br>\n</body></html>"));
	}

	@SuppressWarnings({ "static-method", "boxing" })
	@Test
	void testResolveAndValidateEmailAddressListReturnsEmptyForNullInput() throws Exception {
		@SuppressWarnings("unchecked")
		List<String> result = (List<String>) invokePrivate("resolveAndValidateEmailAddressList",
				new Class<?>[] { String.class, ResponseMode.class, Document.class, Document.class },
				null,
				ResponseMode.EXPLICIT,
				null,
				null);

		assertThat(result.isEmpty(), is(true));
	}

	@Test
	void testGenerateCalendarAttachmentsBuildsLinksAndIcsPayload() throws Exception {
		when(communication.getCalendarTitleExpression()).thenReturn("Team Sync");
		when(communication.getCalendarDescriptionExpression()).thenReturn("Weekly status meeting");
		when(communication.getCalendarStartTime()).thenReturn(new DateTime(1_700_000_000_000L));
		when(communication.getCalendarEndTime()).thenReturn(new DateTime(1_700_000_360_000L));

		CommunicationCalendarItem item = (CommunicationCalendarItem) invokePrivate("generateCalendarAttachments",
				new Class<?>[] { Customer.class, Communication.class, Bean[].class },
				null,
				communication,
				new Bean[0]);

		assertThat(item.getGoogleCalendarLink(), containsString("google.com/calendar/render"));
		assertThat(item.getYahooCalendarLink(), containsString("calendar.yahoo.com"));
		String ics = new String(item.getIcsFileAttachment(), StandardCharsets.UTF_8);
		assertThat(ics, containsString("BEGIN:VCALENDAR"));
		assertThat(ics, containsString("SUMMARY:Team Sync"));
	}

	@SuppressWarnings("boxing")
	private static Communication communicationForResults(ActionType actionType, long count) {
		Communication communication = mock(Communication.class);
		Tag tag = mock(Tag.class);
		when(communication.getModuleName()).thenReturn("admin");
		when(communication.getDocumentName()).thenReturn("Communication");
		when(communication.getTag()).thenReturn(tag);
		when(communication.getActionType()).thenReturn(actionType);
		when(tag.countDocument("admin", "Communication")).thenReturn(count);
		return communication;
	}

	private static Object invokePrivate(String methodName, Class<?>[] parameterTypes, Object... args) throws Exception {
		Method method = CommunicationUtil.class.getDeclaredMethod(methodName, parameterTypes);
		method.setAccessible(true);
		return method.invoke(null, args);
	}
}
