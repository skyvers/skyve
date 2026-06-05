package org.skyve.util;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.OutputStream;
import java.lang.reflect.Method;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.app.admin.SecurityLog;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.mail.MailServiceStaticSingleton;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.util.UtilImpl;

@SuppressWarnings("java:S4144")
class SecurityUtilTest {
	private final CaptureMailService capture = new CaptureMailService();

	private MailService originalMailService;
	private String originalArchiveName;
	private String originalEnvironmentIdentifier;
	private String originalSecurityNotificationsEmailAddress;
	private String originalSupportEmailAddress;
	private String originalSmtp;
	private String originalSmtpSender;
	private String originalSmtpTestRecipient;
	private boolean originalSmtpTestBogusSend;

	@BeforeEach
	void beforeEach() {
		MailServiceStaticSingleton.setDefault();
		originalMailService = MailServiceStaticSingleton.get();
		originalArchiveName = UtilImpl.ARCHIVE_NAME;
		originalEnvironmentIdentifier = UtilImpl.ENVIRONMENT_IDENTIFIER;
		originalSecurityNotificationsEmailAddress = UtilImpl.SECURITY_NOTIFICATIONS_EMAIL_ADDRESS;
		originalSupportEmailAddress = UtilImpl.SUPPORT_EMAIL_ADDRESS;
		originalSmtp = UtilImpl.SMTP;
		originalSmtpSender = UtilImpl.SMTP_SENDER;
		originalSmtpTestRecipient = UtilImpl.SMTP_TEST_RECIPIENT;
		originalSmtpTestBogusSend = UtilImpl.SMTP_TEST_BOGUS_SEND;

		MailServiceStaticSingleton.set(capture);
		UtilImpl.ARCHIVE_NAME = "SkyveTest";
		UtilImpl.ENVIRONMENT_IDENTIFIER = "TEST";
		UtilImpl.SECURITY_NOTIFICATIONS_EMAIL_ADDRESS = "security@skyve.org";
		UtilImpl.SUPPORT_EMAIL_ADDRESS = "support@skyve.org";
		UtilImpl.SMTP = "mail.skyve.org";
		UtilImpl.SMTP_SENDER = "noreply@skyve.org";
		UtilImpl.SMTP_TEST_RECIPIENT = null;
		UtilImpl.SMTP_TEST_BOGUS_SEND = false;
	}

	@AfterEach
	void afterEach() {
		MailServiceStaticSingleton.set(originalMailService);
		UtilImpl.ARCHIVE_NAME = originalArchiveName;
		UtilImpl.ENVIRONMENT_IDENTIFIER = originalEnvironmentIdentifier;
		UtilImpl.SECURITY_NOTIFICATIONS_EMAIL_ADDRESS = originalSecurityNotificationsEmailAddress;
		UtilImpl.SUPPORT_EMAIL_ADDRESS = originalSupportEmailAddress;
		UtilImpl.SMTP = originalSmtp;
		UtilImpl.SMTP_SENDER = originalSmtpSender;
		UtilImpl.SMTP_TEST_RECIPIENT = originalSmtpTestRecipient;
		UtilImpl.SMTP_TEST_BOGUS_SEND = originalSmtpTestBogusSend;
	}

	@Test
	void testEmailUsesExtMailService() throws Exception {
		SecurityLog securityLog = mock(SecurityLog.class);
		when(securityLog.getTimestamp()).thenReturn(new Timestamp());
		when(securityLog.getEventType()).thenReturn("Login Failure");
		when(securityLog.getEventMessage()).thenReturn("Bad password");
		when(securityLog.getSourceIP()).thenReturn("127.0.0.1");

		invokeEmail(securityLog);

		assertEquals(1, capture.sendCount);
		assertTrue(capture.lastSend.getRecipientEmailAddresses().contains("security@skyve.org"));
		assertThat(capture.lastSend.getSenderEmailAddress(), is("noreply@skyve.org"));
		assertThat(capture.lastSend.getSubject(), is("[SkyveTest - TEST] Security Log Entry - Login Failure"));
		assertThat(capture.lastSend.getBody(), containsString("Event Message: Bad password"));
	}

	@Test
	void testEmailFallsBackToSupportAddressAndIncludesOptionalFields() throws Exception {
		UtilImpl.SECURITY_NOTIFICATIONS_EMAIL_ADDRESS = null;
		SecurityLog securityLog = mock(SecurityLog.class);
		when(securityLog.getTimestamp()).thenReturn(new Timestamp());
		when(securityLog.getThreadId()).thenReturn(Long.valueOf(42));
		when(securityLog.getThreadName()).thenReturn("worker");
		when(securityLog.getSourceIP()).thenReturn("192.0.2.7");
		when(securityLog.getUsername()).thenReturn("admin");
		when(securityLog.getLoggedInUserId()).thenReturn("U1");
		when(securityLog.getEventType()).thenReturn("Password Change");
		when(securityLog.getEventMessage()).thenReturn("Changed");
		when(securityLog.getProvenance()).thenReturn("SecurityUtilTest.java:1");

		invokeEmail(securityLog);

		assertEquals(1, capture.sendCount);
		assertTrue(capture.lastSend.getRecipientEmailAddresses().contains("support@skyve.org"));
		assertThat(capture.lastSend.getBody(), containsString("Thread ID: 42"));
		assertThat(capture.lastSend.getBody(), containsString("Thread Name: worker"));
		assertThat(capture.lastSend.getBody(), containsString("Username: admin"));
		assertThat(capture.lastSend.getBody(), containsString("Logged in User ID: U1"));
		assertThat(capture.lastSend.getBody(), containsString("Provenance: SecurityUtilTest.java:1"));
	}

	@Test
	void testEmailDoesNotSendWithoutRecipientAddress() throws Exception {
		UtilImpl.SECURITY_NOTIFICATIONS_EMAIL_ADDRESS = null;
		UtilImpl.SUPPORT_EMAIL_ADDRESS = null;

		invokeEmail(mock(SecurityLog.class));

		assertEquals(0, capture.sendCount);
	}

	@Test
	void testEmailDoesNotSendWhenSmtpIsLocalhost() throws Exception {
		UtilImpl.SMTP = "localhost";

		invokeEmail(mock(SecurityLog.class));

		assertEquals(0, capture.sendCount);
	}

	@Test
	@SuppressWarnings("static-method")
	void testFormatTimestampReturnsNullForNullTimestamp() throws Exception {
		Method method = SecurityUtil.class.getDeclaredMethod("formatTimestampWithServerAndUTCZone", Timestamp.class);
		method.setAccessible(true);

		org.junit.jupiter.api.Assertions.assertNull(method.invoke(null, new Object[] { null }));
	}

	private static void invokeEmail(SecurityLog securityLog) throws Exception {
		Method method = SecurityUtil.class.getDeclaredMethod("email", SecurityLog.class);
		method.setAccessible(true);
		method.invoke(null, securityLog);
	}

	// ======== getSourceIpAddress() ========

	@SuppressWarnings("static-method")
	@Test
	void testGetSourceIpAddressFromForwardedHeader() {
		jakarta.servlet.http.HttpServletRequest request = mock(jakarta.servlet.http.HttpServletRequest.class);
		when(request.getHeader("Forwarded")).thenReturn("for=203.0.113.5; proto=https");
		when(request.getHeader("X-Forwarded-For")).thenReturn(null);
		String ip = SecurityUtil.getSourceIpAddress(request);
		assertThat(ip, is("203.0.113.5"));
	}

	@SuppressWarnings("static-method")
	@Test
	void testGetSourceIpAddressFromForwardedHeaderWithoutForFieldFallsBackToRemoteAddr() {
		// Forwarded header present but no "for=" field — should fall through to remote addr
		jakarta.servlet.http.HttpServletRequest request = mock(jakarta.servlet.http.HttpServletRequest.class);
		when(request.getHeader("Forwarded")).thenReturn("proto=https; host=example.com");
		when(request.getHeader("X-Forwarded-For")).thenReturn(null);
		when(request.getRemoteAddr()).thenReturn("10.0.0.2");
		String ip = SecurityUtil.getSourceIpAddress(request);
		assertThat(ip, is("10.0.0.2"));
	}

	@SuppressWarnings("static-method")
	@Test
	void testGetSourceIpAddressFromXForwardedForHeader() {
		jakarta.servlet.http.HttpServletRequest request = mock(jakarta.servlet.http.HttpServletRequest.class);
		when(request.getHeader("Forwarded")).thenReturn(null);
		when(request.getHeader("X-Forwarded-For")).thenReturn("198.51.100.10, 192.0.2.1");
		String ip = SecurityUtil.getSourceIpAddress(request);
		assertThat(ip, is("198.51.100.10"));
	}

	@SuppressWarnings("static-method")
	@Test
	void testGetSourceIpAddressFromRemoteAddr() {
		jakarta.servlet.http.HttpServletRequest request = mock(jakarta.servlet.http.HttpServletRequest.class);
		when(request.getHeader("Forwarded")).thenReturn(null);
		when(request.getHeader("X-Forwarded-For")).thenReturn(null);
		when(request.getRemoteAddr()).thenReturn("10.0.0.1");
		String ip = SecurityUtil.getSourceIpAddress(request);
		assertThat(ip, is("10.0.0.1"));
	}

	// ======== getProvenance() ========

	@SuppressWarnings("static-method")
	@Test
	void testGetProvenanceReturnsFirstStackTraceElement() {
		Exception e = new RuntimeException("test");
		String provenance = SecurityUtil.getProvenance(e);
		assertThat(provenance, containsString("SecurityUtilTest"));
	}

	@SuppressWarnings("static-method")
	@Test
	void testGetProvenanceReturnsNullForEmptyStackTrace() {
		Exception e = new RuntimeException("test");
		e.setStackTrace(new StackTraceElement[0]);
		String provenance = SecurityUtil.getProvenance(e);
		assertThat(provenance, org.hamcrest.CoreMatchers.nullValue());
	}

	// ======== createDelegatingPasswordEncoder() ========

	@SuppressWarnings("static-method")
	@Test
	void testCreateDelegatingPasswordEncoderIsNotNull() {
		org.springframework.security.crypto.password.PasswordEncoder encoder = SecurityUtil.createDelegatingPasswordEncoder();
		assertThat(encoder, org.hamcrest.CoreMatchers.notNullValue());
	}

	@SuppressWarnings("static-method")
	@Test
	void testCreateDelegatingPasswordEncoderCanEncode() {
		org.springframework.security.crypto.password.PasswordEncoder encoder = SecurityUtil.createDelegatingPasswordEncoder();
		String encoded = encoder.encode("password");
		assertThat(encoded, containsString("{argon2}"));
	}

	@SuppressWarnings("static-method")
	@Test
	void testCreateDelegatingPasswordEncoderCanMatch() {
		org.springframework.security.crypto.password.PasswordEncoder encoder = SecurityUtil.createDelegatingPasswordEncoder();
		String encoded = encoder.encode("myPassword");
		assertTrue(encoder.matches("myPassword", encoded));
	}

	// ======== hashPassword() ========

	@SuppressWarnings("static-method")
	@Test
	void testHashPasswordReturnsArgon2Prefix() {
		// Default algorithm is argon2
		String hashed = SecurityUtil.hashPassword("secret");
		assertThat(hashed, containsString("{argon2}"));
	}

	@SuppressWarnings("static-method")
	@Test
	void testHashPasswordBcrypt() {
		String originalAlgorithm = UtilImpl.PASSWORD_HASHING_ALGORITHM;
		try {
			UtilImpl.PASSWORD_HASHING_ALGORITHM = "bcrypt";
			String hashed = SecurityUtil.hashPassword("secret");
			assertThat(hashed, containsString("{bcrypt}"));
		} finally {
			UtilImpl.PASSWORD_HASHING_ALGORITHM = originalAlgorithm;
		}
	}

	@SuppressWarnings("static-method")
	@Test
	void testHashPasswordPbkdf2() {
		String originalAlgorithm = UtilImpl.PASSWORD_HASHING_ALGORITHM;
		try {
			UtilImpl.PASSWORD_HASHING_ALGORITHM = "pbkdf2";
			String hashed = SecurityUtil.hashPassword("secret");
			assertThat(hashed, containsString("{pbkdf2}"));
		} finally {
			UtilImpl.PASSWORD_HASHING_ALGORITHM = originalAlgorithm;
		}
	}

	@SuppressWarnings("static-method")
	@Test
	void testHashPasswordScrypt() {
		String originalAlgorithm = UtilImpl.PASSWORD_HASHING_ALGORITHM;
		try {
			UtilImpl.PASSWORD_HASHING_ALGORITHM = "scrypt";
			String hashed = SecurityUtil.hashPassword("secret");
			assertThat(hashed, containsString("{scrypt}"));
		} finally {
			UtilImpl.PASSWORD_HASHING_ALGORITHM = originalAlgorithm;
		}
	}

	@SuppressWarnings("static-method")
	@Test
	void testHashPasswordSHA1() {
		String originalAlgorithm = UtilImpl.PASSWORD_HASHING_ALGORITHM;
		try {
			UtilImpl.PASSWORD_HASHING_ALGORITHM = "SHA1";
			String hashed = SecurityUtil.hashPassword("secret");
			assertThat(hashed, org.hamcrest.CoreMatchers.notNullValue());
		} finally {
			UtilImpl.PASSWORD_HASHING_ALGORITHM = originalAlgorithm;
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void testHashPasswordUnsupportedAlgorithmThrowsDomainException() {
		// covers SecurityUtil.hashPassword@379 (else throw new DomainException)
		String originalAlgorithm = UtilImpl.PASSWORD_HASHING_ALGORITHM;
		try {
			UtilImpl.PASSWORD_HASHING_ALGORITHM = "unsupported";
			assertThrows(DomainException.class, () -> SecurityUtil.hashPassword("test"));
		} finally {
			UtilImpl.PASSWORD_HASHING_ALGORITHM = originalAlgorithm;
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void testCreateDelegatingPasswordEncoderReturnsNonNull() {
		org.springframework.security.crypto.password.PasswordEncoder encoder = SecurityUtil.createDelegatingPasswordEncoder();
		assertThat(encoder, org.hamcrest.CoreMatchers.notNullValue());
	}

	@Test
	@SuppressWarnings("static-method")
	void testCreateDelegatingPasswordEncoderCanEncodeAndMatch() {
		org.springframework.security.crypto.password.PasswordEncoder encoder = SecurityUtil.createDelegatingPasswordEncoder();
		String encoded = encoder.encode("myPassword");
		assertTrue(encoder.matches("myPassword", encoded));
	}

	private static class CaptureMailService implements MailService {
		private Mail lastSend;
		private int sendCount;

		@Override
		public void writeMail(Mail mail, OutputStream out) {
			// no-op
		}

		@Override
		public void sendMail(Mail mail) {
			lastSend = mail;
			sendCount++;
		}

		@Override
		public void sendBulkMail(List<Mail> mails) {
			// no-op
		}
	}
}
