package org.skyve.impl.mail;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.skyve.content.MimeType;
import org.skyve.impl.metadata.user.SuperUser;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.user.User;
import org.skyve.util.Mail;
import org.skyve.util.MailAttachment;
import org.skyve.util.MailDispatchOutcome;

class MailLogUtilTest {
	private final List<MailLogUtil.MailLogEntry> entries = new ArrayList<>();

	@BeforeEach
	void beforeEach() {
		MailLogUtil.setRecorderForTesting(entries::add);
	}

	@AfterEach
	void afterEach() {
		MailLogUtil.clearRecorderForTesting();
	}

	@SuppressWarnings("boxing")
	@Test
	void testLogMailCapturesMetadataAndRedactsBody() {
		Mail mail = new Mail().from("sender@skyve.org")
								.addTo("to2@skyve.org")
								.addTo("to1@skyve.org")
								.addCC("cc@skyve.org")
								.addBCC("bcc@skyve.org")
								.subject("Subject")
								.body("<p>Hello<br/>Your verification code is 123456</p>");
		mail.attach(new MailAttachment("report.txt", new byte[] { 1, 2, 3 }, MimeType.plain));
		mail.attach(new MailAttachment(null, new byte[] { 9 }, MimeType.plain));
		mail.getAttachments().add(null);

		MailDispatchOutcome outcome = MailDispatchOutcome.sent("smtp", "message-1", "accepted", "queued");
		MailLogUtil.logMail(mail, outcome);

		assertThat(entries.size(), is(1));
		MailLogUtil.MailLogEntry entry = entries.get(0);
		assertThat(entry.getToRecipients(), is("to1@skyve.org, to2@skyve.org"));
		assertThat(entry.getCcRecipients(), is("cc@skyve.org"));
		assertThat(entry.getBccRecipients(), is("bcc@skyve.org"));
		assertThat(entry.getSubject(), is("Subject"));
		assertThat(entry.getBodyExcerpt(), is("[REDACTED]"));
		assertThat(entry.getAttachmentFileNames(), is("report.txt"));
		assertThat(entry.getDispatchStatus(), is("SENT"));
		assertThat(entry.getProvider(), is("smtp"));
		assertThat(entry.getProviderMessageId(), is("message-1"));
		assertThat(entry.getRelayStatus(), is("accepted"));
		assertThat(entry.getRelayDetail(), is("queued"));
		assertThat(entry.getErrorDetail(), is((String) null));
		assertThat(entry.getIsBulk(), is(Boolean.FALSE));
		assertThat(entry.getMailCount(), is(Long.valueOf(1)));
		assertThat(entry.getRecipientCount(), is(Long.valueOf(4)));
		assertThat(entry.getHasMultipleSubjects(), is(Boolean.FALSE));
		assertThat(entry.getSubjectVariantCount(), is(Long.valueOf(1)));
		assertThat(entry.getHasMultipleBodies(), is(Boolean.FALSE));
		assertThat(entry.getBodyVariantCount(), is(Long.valueOf(1)));
	}

	@SuppressWarnings("boxing")
	@Test
	void testLogBulkMailTracksVariantsAndHandlesEmptyRecipientJoins() {
		Mail first = new Mail().subject("Subject 1").body("<div>One</div>");
		first.attach(new MailAttachment("first.txt", new byte[] { 1 }, MimeType.plain));
		first.getAttachments().add(null);

		Mail second = new Mail().subject("Subject 2").body("Security code is 123456");

		MailDispatchOutcome outcome = MailDispatchOutcome.failed("provider", "relay failed");
		MailLogUtil.logBulkMail(Arrays.asList(first, second), outcome);

		assertThat(entries.size(), is(1));
		MailLogUtil.MailLogEntry entry = entries.get(0);
		assertThat(entry.getToRecipients(), is((String) null));
		assertThat(entry.getCcRecipients(), is((String) null));
		assertThat(entry.getBccRecipients(), is((String) null));
		assertThat(entry.getSubject(), is("Subject 1"));
		assertThat(entry.getBodyExcerpt(), is("[REDACTED]"));
		assertThat(entry.getAttachmentFileNames(), is("first.txt"));
		assertThat(entry.getDispatchStatus(), is("FAILED"));
		assertThat(entry.getProvider(), is("provider"));
		assertThat(entry.getErrorDetail(), is("relay failed"));
		assertThat(entry.getIsBulk(), is(Boolean.TRUE));
		assertThat(entry.getMailCount(), is(Long.valueOf(2)));
		assertThat(entry.getRecipientCount(), is(Long.valueOf(0)));
		assertThat(entry.getHasMultipleSubjects(), is(Boolean.TRUE));
		assertThat(entry.getSubjectVariantCount(), is(Long.valueOf(2)));
		assertThat(entry.getHasMultipleBodies(), is(Boolean.TRUE));
		assertThat(entry.getBodyVariantCount(), is(Long.valueOf(2)));
	}

	@SuppressWarnings("boxing")
	@Test
	void testLogBulkMailWithEmptyListProducesEmptyEntry() {
		MailLogUtil.logBulkMail(List.of(), MailDispatchOutcome.skipped("provider", "empty"));

		assertThat(entries.size(), is(1));
		MailLogUtil.MailLogEntry entry = entries.get(0);
		assertThat(entry.getToRecipients(), is((String) null));
		assertThat(entry.getCcRecipients(), is((String) null));
		assertThat(entry.getBccRecipients(), is((String) null));
		assertThat(entry.getSubject(), is((String) null));
		assertThat(entry.getBodyExcerpt(), is((String) null));
		assertThat(entry.getAttachmentFileNames(), is((String) null));
		assertThat(entry.getDispatchStatus(), is("SKIPPED"));
		assertThat(entry.getProvider(), is("provider"));
		assertThat(entry.getRelayDetail(), is("empty"));
		assertThat(entry.getIsBulk(), is(Boolean.TRUE));
		assertThat(entry.getMailCount(), is(Long.valueOf(0)));
		assertThat(entry.getRecipientCount(), is(Long.valueOf(0)));
		assertThat(entry.getHasMultipleSubjects(), is(Boolean.FALSE));
		assertThat(entry.getSubjectVariantCount(), is(Long.valueOf(0)));
		assertThat(entry.getHasMultipleBodies(), is(Boolean.FALSE));
		assertThat(entry.getBodyVariantCount(), is(Long.valueOf(0)));
	}

	@SuppressWarnings("boxing")
	@Test
	void testLogMailWithoutRecorderSkipsPersistenceWhenNoPersistenceConfigured() {
		Class<? extends AbstractPersistence> originalImplementationClass = AbstractPersistence.IMPLEMENTATION_CLASS;
		try {
			AbstractPersistence.IMPLEMENTATION_CLASS = null;
			MailLogUtil.clearRecorderForTesting();
			MailLogUtil.logMail(new Mail().subject("Subject").body("Body"), MailDispatchOutcome.sent("smtp"));
		}
		finally {
			AbstractPersistence.IMPLEMENTATION_CLASS = originalImplementationClass;
		}
	}

	@SuppressWarnings("unchecked")
	@Test
	void testCreateMailLogUserUsesCurrentUserId() throws Exception {
		User user = Mockito.mock(User.class);
		Mockito.when(user.getId()).thenReturn("user-1");
		Mockito.when(user.getCustomerName()).thenReturn("customer-1");

		SuperUser result = (SuperUser) invokePrivate("createMailLogUser", new Class<?>[] { User.class }, user);

		assertThat(result, instanceOf(SuperUser.class));
		assertThat(result.getId(), is("user-1"));
		assertThat(result.getCustomerName(), is("customer-1"));
	}

	@SuppressWarnings("unchecked")
	@Test
	void testCreateMailLogUserFallsBackToAnonymousCustomer() throws Exception {
		String originalCustomer = UtilImpl.CUSTOMER;
		try {
			UtilImpl.CUSTOMER = "fallback-customer";
			User user = Mockito.mock(User.class);
			Mockito.when(user.getId()).thenReturn(null);
			Mockito.when(user.getCustomerName()).thenReturn(null);

			SuperUser result = (SuperUser) invokePrivate("createMailLogUser", new Class<?>[] { User.class }, user);

			assertThat(result, instanceOf(SuperUser.class));
			assertThat(result.getId(), is("mailLogUser"));
			assertThat(result.getCustomerName(), is("fallback-customer"));
		}
		finally {
			UtilImpl.CUSTOMER = originalCustomer;
		}
	}

	@Test
	void testCreateMailLogUserReturnsNullWhenCustomerCannotBeResolved() throws Exception {
		String originalCustomer = UtilImpl.CUSTOMER;
		try {
			UtilImpl.CUSTOMER = null;
			User user = Mockito.mock(User.class);
			Mockito.when(user.getId()).thenReturn(null);
			Mockito.when(user.getCustomerName()).thenReturn(null);

			Object result = invokePrivate("createMailLogUser", new Class<?>[] { User.class }, user);

			assertThat(result, is((Object) null));
		}
		finally {
			UtilImpl.CUSTOMER = originalCustomer;
		}
	}

	private static Object invokePrivate(String methodName, Class<?>[] parameterTypes, Object... args) throws Exception {
		Method method = MailLogUtil.class.getDeclaredMethod(methodName, parameterTypes);
		method.setAccessible(true);
		return method.invoke(null, args);
	}
}
