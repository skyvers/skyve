package org.skyve.util;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.OutputStream;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.function.Function;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.app.AppConstants;
import org.skyve.domain.app.admin.Communication;
import org.skyve.domain.app.admin.Communication.ActionType;
import org.skyve.domain.app.admin.Communication.FormatType;
import org.skyve.domain.app.admin.Contact;
import org.skyve.domain.app.admin.Subscription;
import org.skyve.domain.app.admin.Tag;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.types.DateTime;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.content.NoOpContentManager;
import org.skyve.impl.mail.MailServiceStaticSingleton;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.DocumentPermissionScope;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentFilter;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.CommunicationUtil.CommunicationCalendarItem;
import org.skyve.util.CommunicationUtil.ResponseMode;
import org.skyve.web.WebContext;

class CommunicationUtilTest {

	private Communication communication;
	private MailService originalMailService;
	private Class<? extends AbstractContentManager> originalContentManagerClass;

	@BeforeEach
	void setup() {
		// setup mocks
		communication = mock(Communication.class);
		try {
			originalMailService = MailServiceStaticSingleton.get();
		}
		catch (@SuppressWarnings("unused") IllegalStateException e) {
			originalMailService = null;
		}
		originalContentManagerClass = AbstractContentManager.IMPLEMENTATION_CLASS;
	}

	@AfterEach
	void teardown() throws Exception {
		unbindPersistenceFromThread();
		if (originalMailService != null) {
			MailServiceStaticSingleton.set(originalMailService);
		}
		AbstractContentManager.IMPLEMENTATION_CLASS = originalContentManagerClass;
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

	@Test
	void testGetResultsWithNoActionOnlyReportsCountAndDocument() throws Exception {
		communication = communicationForResults(null, 2L);

		String result = CommunicationUtil.getResults(communication);

		assertThat(result, is("2 communications for Communication"));
	}

	@SuppressWarnings("static-method")
	@Test
	void testFormatCommunicationMessageReturnsNullForNullExpression() throws Exception {
		assertThat(CommunicationUtil.formatCommunicationMessage(null, null), is((String) null));
	}

	@SuppressWarnings("static-method")
	@Test
	void testFormatCommunicationMessageReplacesResetPasswordPlaceholder() throws Exception {
		String expression = "Use this link: " + CommunicationUtil.SPECIAL_LOGOUT_URL;

		String result = CommunicationUtil.formatCommunicationMessage(null, expression);

		assertFalse(result.contains(CommunicationUtil.SPECIAL_LOGOUT_URL));
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
		assertFalse(result.contains(CommunicationUtil.SPECIAL_BEAN_URL));
		assertFalse(result.contains(CommunicationUtil.SPECIAL_CONTEXT));
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
	void testFormatCommunicationMessageWithEmptyBeansOnlyReplacesResetPasswordPlaceholder() throws Exception {
		String expression = CommunicationUtil.SPECIAL_BEAN_URL + " " + CommunicationUtil.SPECIAL_CONTEXT + " " + CommunicationUtil.SPECIAL_LOGOUT_URL;

		String result = CommunicationUtil.formatCommunicationMessage(null, expression, new Bean[0]);

		assertThat(result, containsString(CommunicationUtil.SPECIAL_BEAN_URL));
		assertThat(result, containsString(CommunicationUtil.SPECIAL_CONTEXT));
		assertFalse(result.contains(CommunicationUtil.SPECIAL_LOGOUT_URL));
	}

	@SuppressWarnings("static-method")
	@Test
	void testHtmlEncloseWrapsAndConvertsPlainText() throws Exception {
		String result = (String) invokePrivate("htmlEnclose", new Class<?>[] { String.class }, "Line one\nLine two");

		assertThat(result, is("<html><body>Line one<br>\nLine two<br>\n</body></html>"));
	}

	@SuppressWarnings("static-method")
	@Test
	void testHtmlEnclosePreservesExistingHtmlAndSkipsBreaksAfterTags() throws Exception {
		String result = (String) invokePrivate("htmlEnclose", new Class<?>[] { String.class },
				"<html>\n<body>\n<p>Hello</p>\nPlain\n</body>\n</html>");

		assertThat(result, is("<html>\n<body>\n<p>Hello</p>\nPlain<br>\n</body>\n</html>\n"));
	}

	@SuppressWarnings("static-method")
	@Test
	void testHtmlEncloseAddsOnlyMissingClosingMarkup() throws Exception {
		String result = (String) invokePrivate("htmlEnclose", new Class<?>[] { String.class }, "<html><body>Plain");

		assertThat(result, is("<html><body>Plain<br>\n</body></html>"));
	}

	@SuppressWarnings("static-method")
	@Test
	void testResolveAndValidateEmailAddressListReturnsEmptyForNullInput() throws Exception {
		@SuppressWarnings("unchecked")
		List<String> result = (List<String>) invokePrivate("resolveAndValidateEmailAddressList",
				new Class<?>[] { String.class, ResponseMode.class, Document.class, Document.class },
				null,
				ResponseMode.EXPLICIT,
				null,
				null);

		assertTrue(result.isEmpty());
	}

	@SuppressWarnings({"unchecked", "static-method"})
	@Test
	void testResolveAndValidateEmailAddressListUsesSubscriptionPreferredAddress() throws Exception {
		AbstractPersistence persistence = bindMockPersistenceWithUser();
		Persistence callbackPersistence = mock(Persistence.class);
		DocumentQuery query = mock(DocumentQuery.class);
		DocumentFilter filter = mock(DocumentFilter.class);
		Subscription subscription = mock(Subscription.class);
		when(persistence.withDocumentPermissionScopes(eq(DocumentPermissionScope.customer), any(Function.class)))
				.thenAnswer(invocation -> ((Function<Persistence, String>) invocation.getArgument(1)).apply(callbackPersistence));
		when(callbackPersistence.newDocumentQuery(AppConstants.ADMIN_MODULE_NAME, AppConstants.SUBSCRIPTION_DOCUMENT_NAME)).thenReturn(query);
		when(query.getFilter()).thenReturn(filter);
		when(query.beanResult()).thenReturn(subscription);
		when(subscription.getPreferredReceiverIdentifier()).thenReturn("preferred@example.com");

		List<String> result = (List<String>) invokePrivate("resolveAndValidateEmailAddressList",
				new Class<?>[] { String.class, ResponseMode.class, Document.class, Document.class },
				" original@example.com ",
				ResponseMode.EXPLICIT,
				mock(Document.class),
				mock(Document.class));

		assertThat(result, is(List.of("preferred@example.com")));
	}

	@SuppressWarnings({"unchecked", "static-method"})
	@Test
	void testResolveAndValidateEmailAddressListThrowsForDeclinedSubscriptionInExplicitMode() throws Exception {
		AbstractPersistence persistence = bindMockPersistenceWithUser();
		Persistence callbackPersistence = mock(Persistence.class);
		DocumentQuery query = mock(DocumentQuery.class);
		DocumentFilter filter = mock(DocumentFilter.class);
		Subscription subscription = mock(Subscription.class);
		Document communicationDoc = mock(Document.class);
		Document subscriptionDoc = mock(Document.class);
		when(persistence.withDocumentPermissionScopes(eq(DocumentPermissionScope.customer), any(Function.class)))
				.thenAnswer(invocation -> ((Function<Persistence, String>) invocation.getArgument(1)).apply(callbackPersistence));
		when(callbackPersistence.newDocumentQuery(AppConstants.ADMIN_MODULE_NAME, AppConstants.SUBSCRIPTION_DOCUMENT_NAME)).thenReturn(query);
		when(query.getFilter()).thenReturn(filter);
		when(query.beanResult()).thenReturn(subscription);
		when(subscription.getDeclined()).thenReturn(Boolean.TRUE);
		when(communicationDoc.getLocalisedSingularAlias()).thenReturn("Communication");
		when(subscriptionDoc.getLocalisedSingularAlias()).thenReturn("Subscription");

		DomainException e = assertThrows(DomainException.class, () -> invokePrivate("resolveAndValidateEmailAddressList",
				new Class<?>[] { String.class, ResponseMode.class, Document.class, Document.class },
				"declined@example.com",
				ResponseMode.EXPLICIT,
				communicationDoc,
				subscriptionDoc));

		assertThat(e.getMessage(), containsString("declined@example.com"));
		assertThat(e.getMessage(), containsString(AppConstants.DECLINED_ATTRIBUTE_NAME));
	}

	@SuppressWarnings({"unchecked", "static-method"})
	@Test
	void testResolveAndValidateEmailAddressListThrowsForInvalidAddressInExplicitMode() throws Exception {
		AbstractPersistence persistence = bindMockPersistenceWithUser();
		when(persistence.withDocumentPermissionScopes(eq(DocumentPermissionScope.customer), any(Function.class)))
				.thenReturn("not-an-email");

		assertThrows(ValidationException.class, () -> invokePrivate("resolveAndValidateEmailAddressList",
				new Class<?>[] { String.class, ResponseMode.class, Document.class, Document.class },
				"not-an-email",
				ResponseMode.EXPLICIT,
				mock(Document.class),
				mock(Document.class)));
	}

	@SuppressWarnings({"unchecked", "static-method"})
	@Test
	void testGetSystemCommunicationByDescriptionQueriesByDescription() throws Exception {
		AbstractPersistence persistence = bindMockPersistenceWithUser();
		Persistence callbackPersistence = mock(Persistence.class);
		DocumentQuery query = mock(DocumentQuery.class);
		DocumentFilter filter = mock(DocumentFilter.class);
		Communication expected = mock(Communication.class);
		when(persistence.withDocumentPermissionScopes(eq(DocumentPermissionScope.customer), any(Function.class)))
				.thenAnswer(invocation -> ((Function<Persistence, Communication>) invocation.getArgument(1)).apply(callbackPersistence));
		when(callbackPersistence.newDocumentQuery(AppConstants.ADMIN_MODULE_NAME, AppConstants.COMMUNICATION_DOCUMENT_NAME)).thenReturn(query);
		when(query.getFilter()).thenReturn(filter);
		when(query.beanResult()).thenReturn(expected);

		Communication result = CommunicationUtil.getSystemCommunicationByDescription("System notification");

		assertThat(result, is(expected));
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

	@Test
	void testGenerateCalendarAttachmentsHandlesNullTitleAndDescription() throws Exception {
		when(communication.getCalendarStartTime()).thenReturn(new DateTime(1_700_000_000_000L));
		when(communication.getCalendarEndTime()).thenReturn(new DateTime(1_700_000_360_000L));

		CommunicationCalendarItem item = (CommunicationCalendarItem) invokePrivate("generateCalendarAttachments",
				new Class<?>[] { Customer.class, Communication.class, Bean[].class },
				null,
				communication,
				new Bean[0]);

		String ics = new String(item.getIcsFileAttachment(), StandardCharsets.UTF_8);
		assertThat(item.getGoogleCalendarLink(), containsString("text=null"));
		assertThat(item.getYahooCalendarLink(), containsString("title=null"));
		assertThat(ics, containsString("SUMMARY:null"));
	}

	@Test
	@SuppressWarnings("boxing")
	void testSendActionBuildsMailAndGrowls() throws Exception {
		CaptureMailService mailService = new CaptureMailService();
		MailServiceStaticSingleton.set(mailService);
		AbstractContentManager.IMPLEMENTATION_CLASS = NoOpContentManager.class;
		configureExecutableCommunication("sender@example.com", "to@example.com; alt@example.com", "cc@example.com", true);
		when(communication.getIncludeCalendar()).thenReturn(Boolean.TRUE);
		when(communication.getCalendarTitleExpression()).thenReturn("Team Sync");
		when(communication.getCalendarDescriptionExpression()).thenReturn("Weekly status meeting");
		when(communication.getCalendarStartTime()).thenReturn(new DateTime(1_700_000_000_000L));
		when(communication.getCalendarEndTime()).thenReturn(new DateTime(1_700_000_360_000L));
		WebContext webContext = mock(WebContext.class);

		CommunicationUtil.send(webContext, communication, CommunicationUtil.RunMode.ACTION, ResponseMode.EXPLICIT,
				new MailAttachment[] { new MailAttachment("extra.txt", "extra".getBytes(StandardCharsets.UTF_8), org.skyve.content.MimeType.plain) });

		assertThat(mailService.sendCount, is(1));
		assertThat(mailService.lastSend.getSenderEmailAddress(), is("sender@example.com"));
		assertThat(mailService.lastSend.getRecipientEmailAddresses(), is(java.util.Set.of("alt@example.com", "to@example.com")));
		assertThat(mailService.lastSend.getCcEmailAddresses(), is(java.util.Set.of("cc@example.com")));
		assertThat(mailService.lastSend.getBccEmailAddresses(), is(java.util.Set.of("admin@example.com")));
		assertThat(mailService.lastSend.getSubject(), is("Subject"));
		assertThat(mailService.lastSend.getBody(), containsString("<html><body>Body<br>"));
		assertThat(mailService.lastSend.getBody(), containsString("google.com/calendar/render"));
		assertThat(mailService.lastSend.getAttachments().size(), is(3));
		verify(webContext).growl(MessageSeverity.info, CommunicationUtil.SENT_SUCCESSFULLY_MESSAGE);
	}

	@Test
	@SuppressWarnings("boxing")
	void testSendTestModeValidatesButDoesNotDispatchMail() throws Exception {
		CaptureMailService mailService = new CaptureMailService();
		MailServiceStaticSingleton.set(mailService);
		AbstractContentManager.IMPLEMENTATION_CLASS = NoOpContentManager.class;
		configureExecutableCommunication(null, "to@example.com", null, false);

		CommunicationUtil.send(communication, CommunicationUtil.RunMode.TEST, ResponseMode.EXPLICIT, null);

		assertThat(mailService.sendCount, is(0));
		assertThat(mailService.writeCount, is(0));
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
		try {
			return method.invoke(null, args);
		}
		catch (InvocationTargetException e) {
			Throwable cause = e.getCause();
			if (cause instanceof Exception exception) {
				throw exception;
			}
			if (cause instanceof Error error) {
				throw error;
			}
			throw new RuntimeException(cause);
		}
	}

	private static AbstractPersistence bindMockPersistenceWithUser() throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		when(persistence.getUser()).thenReturn(user);
		bindPersistenceToThread(persistence);
		return persistence;
	}

	@SuppressWarnings("unchecked")
	private void configureExecutableCommunication(String sendFrom, String sendTo, String ccTo, boolean monitorBcc) throws Exception {
		AbstractPersistence persistence = bindMockPersistenceWithUser();
		User user = persistence.getUser();
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document communicationDocument = mock(Document.class);
		Document subscriptionDocument = mock(Document.class);
		org.skyve.domain.app.admin.User adminUser = mock(org.skyve.domain.app.admin.User.class);
		Contact contact = mock(Contact.class);
		Persistence callbackPersistence = mock(Persistence.class);
		DocumentQuery query = mock(DocumentQuery.class);
		DocumentFilter filter = mock(DocumentFilter.class);

		when(user.getCustomer()).thenReturn(customer);
		when(user.getId()).thenReturn("admin-user");
		when(customer.getModule(AppConstants.ADMIN_MODULE_NAME)).thenReturn(module);
		when(module.getDocument(customer, AppConstants.COMMUNICATION_DOCUMENT_NAME)).thenReturn(communicationDocument);
		when(module.getDocument(customer, AppConstants.SUBSCRIPTION_DOCUMENT_NAME)).thenReturn(subscriptionDocument);
		setPersistenceUser(persistence, user);
		when(module.getDocument(customer, AppConstants.USER_DOCUMENT_NAME)).thenReturn(communicationDocument);
		doReturn(adminUser).when(persistence).retrieve(communicationDocument, "admin-user");
		when(adminUser.getContact()).thenReturn(contact);
		when(contact.getEmail1()).thenReturn("admin@example.com");
		when(persistence.withDocumentPermissionScopes(eq(DocumentPermissionScope.customer), any(Function.class)))
				.thenAnswer(invocation -> ((Function<Persistence, String>) invocation.getArgument(1)).apply(callbackPersistence));
		when(callbackPersistence.newDocumentQuery(AppConstants.ADMIN_MODULE_NAME, AppConstants.SUBSCRIPTION_DOCUMENT_NAME)).thenReturn(query);
		when(query.getFilter()).thenReturn(filter);
		when(query.beanResult()).thenReturn(null);

		when(communication.getSendFrom()).thenReturn(sendFrom);
		when(communication.getSendTo()).thenReturn(sendTo);
		when(communication.getCcTo()).thenReturn(ccTo);
		when(communication.getMonitorBcc()).thenReturn(Boolean.valueOf(monitorBcc));
		when(communication.getSubject()).thenReturn("Subject");
		when(communication.getBody()).thenReturn("Body");
		when(communication.getFormatType()).thenReturn(FormatType.email);
		when(communication.getIncludeCalendar()).thenReturn(Boolean.FALSE);
	}

	private static void bindPersistenceToThread(AbstractPersistence persistence) throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		@SuppressWarnings("unchecked")
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		threadLocal.set(persistence);
	}

	private static void setPersistenceUser(AbstractPersistence persistence, User user) throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("user");
		field.setAccessible(true);
		field.set(persistence, user);
	}

	private static void unbindPersistenceFromThread() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		@SuppressWarnings("unchecked")
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		threadLocal.remove();
	}

	// ======== CommunicationCalendarItem ========

	@SuppressWarnings("static-method")
	@Test
	void testCommunicationCalendarItemThreeArgConstructor() {
		byte[] ics = "BEGIN:VCALENDAR".getBytes();
		CommunicationUtil.CommunicationCalendarItem item = new CommunicationUtil.CommunicationCalendarItem("google-link", "yahoo-link", ics);
		assertThat(item.getGoogleCalendarLink(), is("google-link"));
		assertThat(item.getYahooCalendarLink(), is("yahoo-link"));
		assertThat(item.getIcsFileAttachment(), is(ics));
	}

        @SuppressWarnings("static-method")
        @Test
        void testRunModeAndActionTypeEnumsAreAccessible() {
                assertThat(CommunicationUtil.RunMode.ACTION, notNullValue());
                assertThat(CommunicationUtil.RunMode.TEST, notNullValue());
                assertThat(CommunicationUtil.ActionType.FILE, notNullValue());
                assertThat(CommunicationUtil.ActionType.SMTP, notNullValue());
        }

        @SuppressWarnings("static-method")
        @Test
        void testCommunicationCalendarItemNoArgConstructorAndSetters() {
                CommunicationUtil.CommunicationCalendarItem item = new CommunicationUtil.CommunicationCalendarItem();
                item.setGoogleCalendarLink("g-link");
                item.setYahooCalendarLink("y-link");
                byte[] ics = "BEGIN:VCALENDAR".getBytes();
                item.setIcsFileAttachment(ics);
                assertThat(item.getGoogleCalendarLink(), is("g-link"));
                assertThat(item.getYahooCalendarLink(), is("y-link"));
                assertThat(item.getIcsFileAttachment(), is(ics));
        }

        @SuppressWarnings("static-method")
        @Test
	        void testResponseModeEnumsAreAccessible() {
	                assertThat(CommunicationUtil.ResponseMode.EXPLICIT, notNullValue());
	                assertThat(CommunicationUtil.ResponseMode.SILENT, notNullValue());
	        }

	private static class CaptureMailService implements MailService {
		int sendCount;
		int writeCount;
		Mail lastSend;

		@Override
		public void writeMail(@jakarta.annotation.Nonnull Mail mail, @jakarta.annotation.Nonnull OutputStream out) {
			writeCount++;
		}

		@Override
		public void sendMail(@jakarta.annotation.Nonnull Mail mail) {
			sendCount++;
			lastSend = mail;
		}

		@Override
		public void sendBulkMail(@jakarta.annotation.Nonnull List<Mail> mails) {
			// not used by these tests
		}
	}
	}
