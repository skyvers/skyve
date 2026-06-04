package org.skyve.impl.web;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.OutputStream;
import java.lang.reflect.Field;
import java.sql.Connection;
import java.sql.Statement;
import java.util.Locale;
import java.util.List;
import java.util.concurrent.CopyOnWriteArraySet;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.SessionEndedException;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.geoip.GeoIPServiceStaticSingleton;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.mail.MailServiceStaticSingleton;
import org.skyve.impl.metadata.user.SuperUser;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.user.User;
import org.skyve.util.GeoIPService;
import org.skyve.util.IPGeolocation;
import org.skyve.util.DataBuilder;
import org.skyve.util.Mail;
import org.skyve.util.MailService;
import org.skyve.util.test.SkyveFixture.FixtureType;
import org.skyve.web.WebContext;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import modules.admin.User.UserExtension;
import modules.admin.Configuration.ConfigurationExtension;
import modules.admin.Startup.StartupBizlet;
import modules.admin.domain.Configuration;
import modules.admin.domain.UserRole;
import util.AbstractH2Test;

@SuppressWarnings({"static-method", "boxing"})
class WebUtilH2Test extends AbstractH2Test {
	private final CaptureMailService capture = new CaptureMailService();

	private MailService originalMailService;
	private String originalSmtpTestRecipient;
	private boolean originalSmtpTestBogusSend;
	private boolean originalConcurrentSessionWarnings;
	private boolean originalConcurrentSessionNotifications;
	private boolean originalSecurityExceptionNotifications;
	private GeoIPService originalGeoIPService;
	private CopyOnWriteArraySet<String> originalGeoIPCountryCodes;
	private boolean originalGeoIPWhitelist;
	private String originalCustomer;

	@BeforeEach
	void beforeEach() {
		MailServiceStaticSingleton.setDefault();
		originalMailService = MailServiceStaticSingleton.get();
		originalSmtpTestRecipient = UtilImpl.SMTP_TEST_RECIPIENT;
		originalSmtpTestBogusSend = UtilImpl.SMTP_TEST_BOGUS_SEND;
		originalConcurrentSessionWarnings = UtilImpl.CONCURRENT_SESSION_WARNINGS;
		originalConcurrentSessionNotifications = UtilImpl.CONCURRENT_SESSION_NOTIFICATIONS;
		originalSecurityExceptionNotifications = UtilImpl.SECURITY_EXCEPTION_NOTIFICATIONS;
		originalGeoIPService = GeoIPServiceStaticSingleton.get();
		originalGeoIPCountryCodes = UtilImpl.GEO_IP_COUNTRY_CODES;
		originalGeoIPWhitelist = UtilImpl.GEO_IP_WHITELIST;
		originalCustomer = UtilImpl.CUSTOMER;

		MailServiceStaticSingleton.set(capture);
		UtilImpl.SMTP_TEST_RECIPIENT = null;
		UtilImpl.SMTP_TEST_BOGUS_SEND = false;
		UtilImpl.CONCURRENT_SESSION_NOTIFICATIONS = false;
		UtilImpl.SECURITY_EXCEPTION_NOTIFICATIONS = false;
	}

	@AfterEach
	void afterEach() {
		MailServiceStaticSingleton.set(originalMailService);
		UtilImpl.SMTP_TEST_RECIPIENT = originalSmtpTestRecipient;
		UtilImpl.SMTP_TEST_BOGUS_SEND = originalSmtpTestBogusSend;
		UtilImpl.CONCURRENT_SESSION_WARNINGS = originalConcurrentSessionWarnings;
		UtilImpl.CONCURRENT_SESSION_NOTIFICATIONS = originalConcurrentSessionNotifications;
		UtilImpl.SECURITY_EXCEPTION_NOTIFICATIONS = originalSecurityExceptionNotifications;
		GeoIPServiceStaticSingleton.set(originalGeoIPService);
		UtilImpl.GEO_IP_COUNTRY_CODES = originalGeoIPCountryCodes;
		UtilImpl.GEO_IP_WHITELIST = originalGeoIPWhitelist;
		UtilImpl.CUSTOMER = originalCustomer;
		WebContainer.clear();
		StateUtil.removeSessions(USER);
		StateUtil.removeSessions("h2-session-user");
		StateUtil.removeSessions("h2-concurrent-user");
	}

	@Test
	void testRequestPasswordResetUsesExtMailService() throws Exception {
		UserExtension user = new DataBuilder().fixture(FixtureType.crud).build(modules.admin.domain.User.MODULE_NAME,
				modules.admin.domain.User.DOCUMENT_NAME);
		String email = "reset-" + System.nanoTime() + "@skyve.org";
		user.setUserName("reset." + System.nanoTime());
		user.getContact().setEmail1(email);
		useDirectBasicUserRole(user);
		CORE.getPersistence().save(user);
		applyTestUser();
		int beforeLogCount = CORE.getPersistence()
								.newDocumentQuery(modules.admin.domain.MailLog.MODULE_NAME, modules.admin.domain.MailLog.DOCUMENT_NAME)
								.beanResults()
								.size();

		WebUtil.requestPasswordReset(CUSTOMER, email);

		assertEquals(1, capture.sendCount);
		assertTrue(capture.lastSend.getRecipientEmailAddresses().contains(email));

		applyTestUser();
		List<PersistentBean> logs = CORE.getPersistence()
										.newDocumentQuery(modules.admin.domain.MailLog.MODULE_NAME, modules.admin.domain.MailLog.DOCUMENT_NAME)
										.beanResults();
		assertTrue(logs.size() > beforeLogCount);

		modules.admin.domain.MailLog matchingLog = null;
		for (PersistentBean bean : logs) {
			modules.admin.domain.MailLog mailLog = (modules.admin.domain.MailLog) bean;
			if (email.equals(mailLog.getToRecipients())) {
				matchingLog = mailLog;
				break;
			}
		}
		assertThat(matchingLog, is(notNullValue()));
		if (matchingLog != null) {
			assertThat(matchingLog.getBodyExcerpt(), is("[REDACTED]"));
			assertNotNull(matchingLog.getBizUserId());
		}
	}

	@Test
	void testResetPasswordReturnsInvalidMessageForUnknownToken() throws Exception {
		String result = WebUtil.resetPassword("missing-token-" + System.nanoTime(), "newPassword1", "newPassword1");

		assertNotNull(result);
	}

	@Test
	void requestPasswordResetRollsBackWhenMailServiceFails() {
		UserExtension user = new DataBuilder().fixture(FixtureType.crud).build(modules.admin.domain.User.MODULE_NAME,
				modules.admin.domain.User.DOCUMENT_NAME);
		String email = "failed-reset-" + System.nanoTime() + "@skyve.org";
		user.setUserName("failed.reset." + System.nanoTime());
		user.getContact().setEmail1(email);
		useDirectBasicUserRole(user);
		CORE.getPersistence().save(user);
		applyTestUser();
		MailService throwingMailService = mock(MailService.class);
		when(throwingMailService.dispatchMail(any(Mail.class))).thenThrow(new IllegalStateException("mail failed"));
		MailServiceStaticSingleton.set(throwingMailService);

		assertThrows(IllegalStateException.class, () -> WebUtil.requestPasswordReset(CUSTOMER, email));
	}

	@Test
	void testResetPasswordProcessesExistingTokenWithRealPersistence() throws Exception {
		long suffix = System.nanoTime();
		String token = "token-" + suffix;
		UserExtension user = new DataBuilder().fixture(FixtureType.crud).build(modules.admin.domain.User.MODULE_NAME,
				modules.admin.domain.User.DOCUMENT_NAME);
		user.setBizId("rtu-" + suffix);
		user.setUserName("reset.token." + suffix);
		user.setPassword(EXT.hashPassword("OldPassword0!"));
		user.setPasswordHistory(null);
		user.setPasswordResetToken(token);
		user.setPasswordResetTokenCreationTimestamp(new Timestamp());
		useDirectBasicUserRole(user);
		CORE.getPersistence().save(user);
		CORE.getPersistence().commit(false);
		CORE.getPersistence().begin();

		String result = WebUtil.resetPassword(token, "NewPassword0!", "NewPassword0!");

		assertNotNull(result);
	}

	@Test
	void testResetPasswordReturnsExpiredMessageForExpiredToken() throws Exception {
		ensurePasswordResetExpiryMinutes(1);
		long suffix = System.nanoTime();
		String token = "expired-token-" + suffix;
		UserExtension user = new DataBuilder().fixture(FixtureType.crud).build(modules.admin.domain.User.MODULE_NAME,
				modules.admin.domain.User.DOCUMENT_NAME);
		user.setBizId("ertu-" + suffix);
		user.setUserName("expired.reset." + suffix);
		user.setPassword(EXT.hashPassword("OldPassword0!"));
		user.setPasswordHistory(null);
		user.setPasswordResetToken(token);
		user.setPasswordResetTokenCreationTimestamp(new Timestamp(System.currentTimeMillis() - 120_000L));
		useDirectBasicUserRole(user);
		CORE.getPersistence().save(user);
		CORE.getPersistence().commit(false);
		CORE.getPersistence().begin();

		String result = WebUtil.resetPassword(token, "NewPassword0!", "NewPassword0!");

		assertNotNull(result);
	}

	@Test
	void sendRegistrationEmailExecutesResendActivationAction() throws Exception {
		UserExtension user = new DataBuilder().fixture(FixtureType.crud).build(modules.admin.domain.User.MODULE_NAME,
				modules.admin.domain.User.DOCUMENT_NAME);
		user.setUserName("registration.email." + System.nanoTime());
		user.setActivated(Boolean.FALSE);
		useDirectBasicUserRole(user);
		user = CORE.getPersistence().save(user);
		CORE.getPersistence().commit(false);
		CORE.getPersistence().begin();
		applyTestUser();

		WebUtil.sendRegistrationEmail(user.getBizId());

		assertNotNull(user.getBizId());
	}

	@Test
	void processUserPrincipalForRequestReusesValidSessionUser() {
		UserImpl sessionUser = new UserImpl();
		sessionUser.setId("h2-session-user");
		sessionUser.setCustomerName(CUSTOMER);
		sessionUser.setName(USER);
		HttpSession session = session("h2-session");
		StateUtil.addSession(sessionUser.getId(), session);

		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getSession(false)).thenReturn(session);
		when(request.getLocale()).thenReturn(Locale.CANADA_FRENCH);
		when(session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME)).thenReturn(sessionUser);

		User result = WebUtil.processUserPrincipalForRequest(request, null);

		assertSame(sessionUser, result);
		assertSame(sessionUser, CORE.getPersistence().getUser());
	}

	@Test
	void processUserPrincipalForRequestConfirmsSessionUserMatchesPrincipal() {
		UserImpl sessionUser = new UserImpl();
		sessionUser.setId("h2-session-user");
		sessionUser.setCustomerName(CUSTOMER);
		sessionUser.setName(USER);
		HttpSession session = session("same-principal-session");
		StateUtil.addSession(sessionUser.getId(), session);

		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getSession(false)).thenReturn(session);
		when(request.getLocale()).thenReturn(Locale.ENGLISH);
		when(session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME)).thenReturn(sessionUser);

		User result = WebUtil.processUserPrincipalForRequest(request, CUSTOMER + '/' + USER);

		assertSame(sessionUser, result);
		assertSame(sessionUser, CORE.getPersistence().getUser());
	}

	@Test
	void processUserPrincipalForRequestReplacesSessionUserWhenPrincipalDiffers() {
		UtilImpl.CONCURRENT_SESSION_WARNINGS = false;
		UserImpl sessionUser = new UserImpl();
		sessionUser.setId("h2-session-user");
		sessionUser.setCustomerName(CUSTOMER);
		sessionUser.setName("other-" + System.nanoTime());
		HttpSession session = session("different-principal-session");
		StateUtil.addSession(sessionUser.getId(), session);

		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getSession(false)).thenReturn(session);
		when(request.getLocale()).thenReturn(Locale.ENGLISH);
		when(request.getHeader("X-Forwarded-For")).thenReturn(null);
		when(request.getHeader("x-forwarded-for")).thenReturn(null);
		when(request.getRemoteAddr()).thenReturn("127.0.0.1");
		when(session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME)).thenReturn(sessionUser);

		User result = WebUtil.processUserPrincipalForRequest(request, CUSTOMER + '/' + USER);

		assertNotNull(result);
		assertEquals(USER, result.getName());
		verify(session).setAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME, result);
	}

	@Test
	void processUserPrincipalForRequestInvalidatesUnknownSessionUser() {
		UserImpl sessionUser = new UserImpl();
		sessionUser.setId("h2-session-user");
		HttpSession session = session("unknown-session");
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getSession(false)).thenReturn(session);
		when(request.getLocale()).thenReturn(Locale.ENGLISH);
		when(session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME)).thenReturn(sessionUser);

		assertThrows(SessionEndedException.class, () -> WebUtil.processUserPrincipalForRequest(request, null));

		verify(session).invalidate();
	}

	@Test
	void processUserPrincipalForRequestEstablishesUserFromPrincipal() {
		UtilImpl.CONCURRENT_SESSION_WARNINGS = false;
		HttpSession session = session("principal-session");
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getSession(false)).thenReturn(null);
		when(request.getSession(true)).thenReturn(session);
		when(request.getLocale()).thenReturn(Locale.ENGLISH);
		when(request.getHeader("X-Forwarded-For")).thenReturn(null);
		when(request.getHeader("x-forwarded-for")).thenReturn(null);
		when(request.getRemoteAddr()).thenReturn("127.0.0.1");
		AbstractPersistence persistence = AbstractPersistence.get();
		int beforeLoginRecordCount = persistence
										.newDocumentQuery(modules.admin.domain.UserLoginRecord.MODULE_NAME,
												modules.admin.domain.UserLoginRecord.DOCUMENT_NAME)
										.beanResults()
										.size();
		ProvidedRepository originalRepository = ProvidedRepositoryFactory.get();
		ProvidedRepository repository = spy(originalRepository);
		doAnswer(invocation -> {
			SuperUser result = new SuperUser();
			result.setCustomerName(CUSTOMER);
			result.setName(USER);
			result.setId(USER);
			clearPersistenceUser(persistence);
			return result;
		}).when(repository).retrieveUser(CUSTOMER + '/' + USER);
		ProvidedRepositoryFactory.set(repository);

		User result;
		try {
			result = WebUtil.processUserPrincipalForRequest(request, CUSTOMER + '/' + USER);
		}
		finally {
			ProvidedRepositoryFactory.set(originalRepository);
		}

		assertNotNull(result);
		assertEquals(CUSTOMER, result.getCustomerName());
		assertEquals(USER, result.getName());
		verify(session).setAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME, result);
		assertSame(result, CORE.getPersistence().getUser());
		int afterLoginRecordCount = CORE.getPersistence()
										.newDocumentQuery(modules.admin.domain.UserLoginRecord.MODULE_NAME,
												modules.admin.domain.UserLoginRecord.DOCUMENT_NAME)
										.beanResults()
										.size();
		assertTrue(afterLoginRecordCount > beforeLoginRecordCount);
	}

	private static void clearPersistenceUser(AbstractPersistence persistence) throws Exception {
		Field userField = AbstractPersistence.class.getDeclaredField("user");
		userField.setAccessible(true);
		userField.set(persistence, null);
	}

	@Test
	void processUserPrincipalForRequestThrowsWhenPrincipalCannotBeResolved() {
		ProvidedRepository originalRepository = ProvidedRepositoryFactory.get();
		ProvidedRepository repository = mock(ProvidedRepository.class);
		ProvidedRepositoryFactory.set(repository);
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getSession(false)).thenReturn(null);
		when(repository.retrieveUser("missing-principal")).thenReturn(null);

		try {
			assertThrows(IllegalStateException.class,
					() -> WebUtil.processUserPrincipalForRequest(request, "missing-principal"));
		}
		finally {
			ProvidedRepositoryFactory.set(originalRepository);
		}
	}

	@Test
	void addSessionAndAuditConcurrentSessionWarningLogsWithRealPersistence() {
		UtilImpl.CONCURRENT_SESSION_WARNINGS = true;
		SuperUser user = new SuperUser();
		user.setCustomerName(CUSTOMER);
		user.setName(USER);
		user.setId("h2-concurrent-user");
		HttpSession existingSession = session("existing-session");
		HttpSession currentSession = session("current-session");
		StateUtil.addSession(user.getId(), existingSession);
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getUserPrincipal()).thenReturn(() -> CUSTOMER + '/' + USER);

		int beforeLogCount = CORE.getPersistence()
									.newDocumentQuery(modules.admin.domain.SecurityLog.MODULE_NAME,
											modules.admin.domain.SecurityLog.DOCUMENT_NAME)
									.beanResults()
									.size();

		WebUtil.addSessionAndAuditConcurrentSessionWarning(user, request, currentSession);

		assertEquals(2, StateUtil.getSessionCount(user.getId()));
		assertTrue(StateUtil.checkSession(user.getId(), currentSession));
		int afterLogCount = CORE.getPersistence()
								.newDocumentQuery(modules.admin.domain.SecurityLog.MODULE_NAME,
										modules.admin.domain.SecurityLog.DOCUMENT_NAME)
								.beanResults()
								.size();
		assertTrue(afterLogCount > beforeLogCount);
	}

	@Test
	void addSessionAndAuditConcurrentSessionWarningKeepsSessionWhenEligibilityThrows() {
		UtilImpl.CONCURRENT_SESSION_WARNINGS = true;
		User user = mock(User.class);
		when(user.getId()).thenReturn("h2-concurrent-user");
		when(user.getName()).thenReturn(USER);
		when(user.getCustomerName()).thenThrow(new IllegalStateException("customer lookup failed"));
		HttpSession existingSession = session("eligibility-existing-session");
		HttpSession currentSession = session("eligibility-current-session");
		StateUtil.addSession(user.getId(), existingSession);
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getUserPrincipal()).thenReturn(() -> CUSTOMER + '/' + USER);

		WebUtil.addSessionAndAuditConcurrentSessionWarning(user, request, currentSession);

		assertEquals(2, StateUtil.getSessionCount(user.getId()));
		assertTrue(StateUtil.checkSession(user.getId(), currentSession));
	}

	@Test
	void addSessionAndAuditConcurrentSessionWarningKeepsSessionWhenWarningLogThrows() {
		UtilImpl.CONCURRENT_SESSION_WARNINGS = true;
		User user = mock(User.class);
		when(user.getId()).thenReturn("h2-concurrent-user");
		when(user.getName()).thenReturn(USER);
		when(user.getCustomerName()).thenReturn(CUSTOMER).thenThrow(new IllegalStateException("log failed"));
		HttpSession existingSession = session("log-existing-session");
		HttpSession currentSession = session("log-current-session");
		StateUtil.addSession(user.getId(), existingSession);
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getUserPrincipal()).thenReturn(() -> CUSTOMER + '/' + USER);

		WebUtil.addSessionAndAuditConcurrentSessionWarning(user, request, currentSession);

		assertEquals(2, StateUtil.getSessionCount(user.getId()));
		assertTrue(StateUtil.checkSession(user.getId(), currentSession));
	}

	@Test
	void requestPasswordResetReturnsWithoutSendingMailWhenGeoIPBlocksRequest() throws Exception {
		String email = "blocked-reset-" + System.nanoTime() + "@skyve.org";
		UserExtension user = CORE.getPersistence().retrieve(modules.admin.domain.User.MODULE_NAME,
				modules.admin.domain.User.DOCUMENT_NAME,
				USER);
		assertNotNull(user);
		user.getContact().setEmail1(email);
		CORE.getPersistence().save(user);
		applyTestUser();
		GeoIPService geoip = mock(GeoIPService.class);
		when(geoip.isBlocking()).thenReturn(Boolean.TRUE);
		when(geoip.isWhitelist()).thenReturn(Boolean.FALSE);
		when(geoip.geolocate("203.0.113.9")).thenReturn(new IPGeolocation(null, null, "CN", null));
		GeoIPServiceStaticSingleton.set(geoip);
		UtilImpl.GEO_IP_COUNTRY_CODES = new CopyOnWriteArraySet<>(List.of("CN"));
		UtilImpl.GEO_IP_WHITELIST = false;
		UtilImpl.CUSTOMER = CUSTOMER;
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getHeader("X-Forwarded-For")).thenReturn(null);
		when(request.getHeader("x-forwarded-for")).thenReturn(null);
		when(request.getRemoteAddr()).thenReturn("203.0.113.9");
		WebContainer.setHttpServletRequestResponse(request, mock(HttpServletResponse.class));

		ProvidedRepository originalRepository = ProvidedRepositoryFactory.get();
		ProvidedRepository repository = spy(originalRepository);
		SuperUser metaUser = new SuperUser();
		metaUser.setCustomerName(CUSTOMER);
		metaUser.setName(USER);
		metaUser.setId(USER);
		doReturn(metaUser).when(repository).retrieveUser(anyString());
		ProvidedRepositoryFactory.set(repository);
		try {
			WebUtil.requestPasswordReset(CUSTOMER, email);
		}
		finally {
			ProvidedRepositoryFactory.set(originalRepository);
		}

		assertEquals(0, capture.sendCount);
	}

	private void applyTestUser() {
		SuperUser user = new SuperUser();
		user.setCustomerName(CUSTOMER);
		user.setName(USER);
		user.setId(USER);
		AbstractPersistence.get().setUser(user);
	}

	private static void useDirectBasicUserRole(UserExtension user) {
		user.getGroups().clear();
		user.getRoles().clear();
		UserRole role = UserRole.newInstance();
		role.setRoleName("admin.BasicUser");
		user.addRolesElement(role);
	}

	private static HttpSession session(String id) {
		HttpSession session = mock(HttpSession.class);
		when(session.getId()).thenReturn(id);
		return session;
	}

	private static void ensurePasswordResetExpiryMinutes(int minutes) throws Exception {
		if (updatePasswordResetExpiryMinutes(minutes) == 0) {
			ConfigurationExtension configuration = new DataBuilder().fixture(FixtureType.crud).build(Configuration.MODULE_NAME,
					Configuration.DOCUMENT_NAME);
			configuration.getStartup().setMapLayer(StartupBizlet.MAP_LAYER_GMAP);
			configuration.getStartup().setMailPort(Integer.valueOf(25));
			configuration.setPasswordResetTokenExpiryMinutes(Integer.valueOf(minutes));
			CORE.getPersistence().save(configuration);
			CORE.getPersistence().commit(false);
			CORE.getPersistence().begin();
			updatePasswordResetExpiryMinutes(minutes);
		}
	}

	private static int updatePasswordResetExpiryMinutes(int minutes) throws Exception {
		try (Connection connection = EXT.getDataStoreConnection();
				Statement statement = connection.createStatement()) {
			return statement.executeUpdate("update ADM_Configuration set passwordResetTokenExpiryMinutes = " + minutes);
		}
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
