package org.skyve;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.bizport.BizPortSheet;
import org.skyve.bizport.BizPortWorkbook;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.metadata.user.User;
import org.skyve.metadata.user.UserAccess;
import org.skyve.util.PushMessage;
import org.skyve.util.PushMessage.PushMessageReceiver;

class EXTTest {

	private static final String JOKER_USERID = "joker";
	private static final String BATMAN_USERID = "batman";

	@BeforeEach
	@SuppressWarnings("static-method")
	void clearReceivers() {
		PushMessage.RECEIVERS.clear();
	}

	/**
	 * Empty receiver queue, just looking for 0 exceptions
	 */
	@Test
	@SuppressWarnings("static-method")
	void testPushWithNoReceivers() {
		EXT.push(growlBroadcastMessage());
		assertTrue(PushMessage.RECEIVERS.isEmpty());
	}

	/**
	 * Send some broadcast messages
	 */
	@Test
	@SuppressWarnings("static-method")
	void testPushBroadcastReceived() {
		TestReceiver recvA = new TestReceiver(BATMAN_USERID);
		TestReceiver recvB = new TestReceiver(JOKER_USERID);

		PushMessage.RECEIVERS.add(recvA);
		PushMessage.RECEIVERS.add(recvB);

		EXT.push(growlBroadcastMessage());

		assertEquals(1, recvA.getMessages().size());
		assertEquals(1, recvB.getMessages().size());

		EXT.push(growlBroadcastMessage());

		assertEquals(2, recvA.getMessages().size());
		assertEquals(2, recvB.getMessages().size());
	}

	/**
	 * Send some messages addressed to users
	 */
	@Test
	@SuppressWarnings("static-method")
	void testPushAddressedMessagesReceived() {
		TestReceiver recvBatman = new TestReceiver(BATMAN_USERID);
		TestReceiver recvJoker = new TestReceiver(JOKER_USERID);

		PushMessage.RECEIVERS.add(recvBatman);
		PushMessage.RECEIVERS.add(recvJoker);

		EXT.push(growlBroadcastMessage().user(BATMAN_USERID));
		EXT.push(growlBroadcastMessage().user(BATMAN_USERID));
		EXT.push(growlBroadcastMessage().user(JOKER_USERID));

		assertEquals(2, recvBatman.getMessages().size());
		assertEquals(1, recvJoker.getMessages().size());

		EXT.push(growlBroadcastMessage().user(JOKER_USERID));
		EXT.push(growlBroadcastMessage().user(JOKER_USERID));
		EXT.push(growlBroadcastMessage().user(JOKER_USERID).user(BATMAN_USERID));
		EXT.push(growlBroadcastMessage().user("catwoman"));

		assertEquals(3, recvBatman.getMessages().size());
		assertEquals(4, recvJoker.getMessages().size());
	}

	@Test
	@SuppressWarnings("static-method")
	void testReaperRemovesStaleReceiverAndClosesIt() {
		AtomicBoolean closed = new AtomicBoolean(false);
		PushMessageReceiver stale = new PushMessageReceiver() {
			@Override
			public String forUserId() {
				return BATMAN_USERID;
			}

			@Override
			public void sendMessage(PushMessage message) {
				// no-op
			}

			@Override
			public boolean isStale() {
				return true;
			}

			@Override
			public void close() {
				closed.set(true);
			}
		};

		try {
			PushMessage.RECEIVERS.add(stale);
			PushMessage.startReaper(1);

			long deadline = System.currentTimeMillis() + 4_000L;
			while (PushMessage.RECEIVERS.contains(stale) && (System.currentTimeMillis() < deadline)) {
				Thread.onSpinWait();
			}

			assertFalse(PushMessage.RECEIVERS.contains(stale));
			assertTrue(closed.get());
		}
		finally {
			PushMessage.stopReaper();
			PushMessage.RECEIVERS.clear();
		}
	}

	private static PushMessage growlBroadcastMessage() {
		return new PushMessage().growl(MessageSeverity.info, "hello");
	}

	// ---- EXT utility method tests ----

	@Test
	@SuppressWarnings("static-method")
	void testHashPasswordProducesNonNullNonEmptyResult() {
		String hash = EXT.hashPassword("mySecret");
		assertNotNull(hash);
		assertFalse(hash.isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void testHashPasswordProducesDifferentHashEachCall() {
		String hash1 = EXT.hashPassword("password");
		String hash2 = EXT.hashPassword("password");
		// BCrypt produces a different salt each time
		org.junit.jupiter.api.Assertions.assertNotEquals(hash1, hash2, "BCrypt hashes should differ even for the same input");
	}

	@Test
	@SuppressWarnings("static-method")
	void testCheckPasswordReturnsTrueForMatchingClearText() {
		String clearText = "mySuperSecret";
		String hash = EXT.hashPassword(clearText);
		assertTrue(EXT.checkPassword(clearText, hash));
	}

	@Test
	@SuppressWarnings("static-method")
	void testCheckPasswordReturnsFalseForWrongClearText() {
		String hash = EXT.hashPassword("correctPassword");
		assertFalse(EXT.checkPassword("wrongPassword", hash));
	}

	@Test
	@SuppressWarnings("static-method")
	void testNewBizPortWorkbookOoxmlReturnsNonNull() {
		BizPortWorkbook wb = EXT.newBizPortWorkbook(true);
		assertNotNull(wb);
	}

	@Test
	@SuppressWarnings("static-method")
	void testNewBizPortSheetReturnsNonNull() {
		BizPortSheet sheet = EXT.newBizPortSheet("TestSheet");
		assertNotNull(sheet);
		assertEquals("TestSheet", sheet.getTitle());
	}

	@Test
	@SuppressWarnings("static-method")
	void testNewBizPortWorkbookXlsReturnsNonNull() {
		BizPortWorkbook wb = EXT.newBizPortWorkbook(false);
		assertNotNull(wb);
	}

	@Test
	@SuppressWarnings("static-method")
	void testNewSQLDataAccessWithDataStoreReturnsNonNull() throws Exception {
		org.skyve.persistence.DataStore ds = new org.skyve.persistence.DataStore(
				"org.h2.Driver",
				"jdbc:h2:mem:ext_test;DB_CLOSE_DELAY=-1",
				"sa",
				"",
				org.skyve.impl.persistence.hibernate.dialect.H2SpatialDialect.class.getName());
		try (org.skyve.dataaccess.sql.SQLDataAccess da = EXT.newSQLDataAccess(ds)) {
			assertNotNull(da);
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void testNewSQLDataAccessDefaultReturnsNonNull() throws Exception {
		org.skyve.impl.util.UtilImpl.DATA_STORE = new org.skyve.persistence.DataStore(
				"org.h2.Driver",
				"jdbc:h2:mem:ext_test2;DB_CLOSE_DELAY=-1",
				"sa",
				"",
				org.skyve.impl.persistence.hibernate.dialect.H2SpatialDialect.class.getName());
		try {
			try (org.skyve.dataaccess.sql.SQLDataAccess da = EXT.newSQLDataAccess()) {
				assertNotNull(da);
			}
		}
		finally {
			org.skyve.impl.util.UtilImpl.DATA_STORE = null;
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetDataStoreConnectionWithH2() throws Exception {
		org.skyve.persistence.DataStore ds = new org.skyve.persistence.DataStore(
				"org.h2.Driver",
				"jdbc:h2:mem:ext_test3;DB_CLOSE_DELAY=-1",
				"sa",
				"",
				org.skyve.impl.persistence.hibernate.dialect.H2SpatialDialect.class.getName());
		try (java.sql.Connection conn = EXT.getDataStoreConnection(ds, false)) {
			assertNotNull(conn);
			assertFalse(conn.isClosed());
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void testIsWebRequestReturnsFalseWhenNoRequestSet() {
		// No WebContainer has been set, so isWebRequest should return false
		assertFalse(EXT.isWebRequest());
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetHttpServletRequestThrowsWhenNoRequestAvailable() {
		assertThrows(IllegalStateException.class, EXT::getHttpServletRequest);
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetHttpServletResponseThrowsWhenNoResponseAvailable() {
		assertThrows(IllegalStateException.class, EXT::getHttpServletRespsone);
	}

	@Test
	@SuppressWarnings("static-method")
	void testCoreServiceGettersDoNotThrow() {
		org.junit.jupiter.api.Assertions.assertDoesNotThrow(EXT::getJobScheduler);
		org.junit.jupiter.api.Assertions.assertDoesNotThrow(EXT::getTagManager);
		org.junit.jupiter.api.Assertions.assertDoesNotThrow(EXT::getReporting);
		org.junit.jupiter.api.Assertions.assertDoesNotThrow(EXT::getCaching);
		org.junit.jupiter.api.Assertions.assertDoesNotThrow(EXT::getAddInManager);
		org.junit.jupiter.api.Assertions.assertDoesNotThrow(EXT::getGeoIPService);
		org.junit.jupiter.api.Assertions.assertDoesNotThrow(EXT::getSMSService);
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetMailServiceHandlesInitialisationState() {
		try {
			EXT.getMailService();
		}
		catch (IllegalStateException e) {
			assertTrue(e.getMessage().contains("not been initialised"));
		}
	}

	@Test
	@SuppressWarnings({ "static-method", "boxing" })
	void testCheckAccessDoesNotThrowWhenUserCanAccess() {
		User user = mock(User.class);
		UserAccess access = UserAccess.singular("admin", "User");

		when(user.canAccess(access, "desktop")).thenReturn(true);

		org.junit.jupiter.api.Assertions.assertDoesNotThrow(() -> EXT.checkAccess(user, access, "desktop"));
	}

	@Test
	@SuppressWarnings({ "static-method", "boxing" })
	void testCheckAccessThrowsForSingularAccess() {
		User user = mock(User.class);
		UserAccess access = UserAccess.singular("admin", "User");

		when(user.canAccess(access, "desktop")).thenReturn(false);
		when(user.getName()).thenReturn("tester");

		assertThrows(IllegalArgumentException.class, () -> EXT.checkAccess(user, access, "desktop"));
	}

	@Test
	@SuppressWarnings({ "static-method", "boxing" })
	void testCheckAccessThrowsForModelAggregateAccess() {
		User user = mock(User.class);
		UserAccess access = UserAccess.modelAggregate("admin", "User", "UserModel");

		when(user.canAccess(access, "desktop")).thenReturn(false);
		when(user.getName()).thenReturn("tester");

		assertThrows(IllegalArgumentException.class, () -> EXT.checkAccess(user, access, "desktop"));
	}

	@Test
	@SuppressWarnings({ "static-method", "boxing" })
	void testCheckAccessThrowsForDocumentAggregateAccess() {
		User user = mock(User.class);
		UserAccess access = UserAccess.documentAggregate("admin", "User");

		when(user.canAccess(access, "desktop")).thenReturn(false);
		when(user.getName()).thenReturn("tester");

		assertThrows(IllegalArgumentException.class, () -> EXT.checkAccess(user, access, "desktop"));
	}

	@Test
	@SuppressWarnings({ "static-method", "boxing" })
	void testCheckAccessThrowsForPreviousCompleteAccess() {
		User user = mock(User.class);
		UserAccess access = UserAccess.previousComplete("admin", "User", "contact.email1");

		when(user.canAccess(access, "desktop")).thenReturn(false);
		when(user.getName()).thenReturn("tester");

		assertThrows(IllegalArgumentException.class, () -> EXT.checkAccess(user, access, "desktop"));
	}

	@Test
	@SuppressWarnings({ "static-method", "boxing" })
	void testCheckAccessThrowsForQueryAggregateAccess() {
		User user = mock(User.class);
		UserAccess access = UserAccess.queryAggregate("admin", "qUsers");

		when(user.canAccess(access, "desktop")).thenReturn(false);
		when(user.getName()).thenReturn("tester");

		assertThrows(IllegalArgumentException.class, () -> EXT.checkAccess(user, access, "desktop"));
	}

	@Test
	@SuppressWarnings({ "static-method", "boxing" })
	void testCheckAccessThrowsForReportAccess() {
		User user = mock(User.class);
		UserAccess access = UserAccess.report("admin", "User", "UserSummary");

		when(user.canAccess(access, "desktop")).thenReturn(false);
		when(user.getName()).thenReturn("tester");

		assertThrows(IllegalArgumentException.class, () -> EXT.checkAccess(user, access, "desktop"));
	}

	@Test
	@SuppressWarnings({ "static-method", "boxing" })
	void testCheckAccessThrowsForContentAccess() {
		User user = mock(User.class);
		UserAccess access = UserAccess.content("admin", "User", "attachment");

		when(user.canAccess(access, "desktop")).thenReturn(false);
		when(user.getName()).thenReturn("tester");

		assertThrows(IllegalArgumentException.class, () -> EXT.checkAccess(user, access, "desktop"));
	}

	@Test
	@SuppressWarnings({ "static-method", "boxing" })
	void testCheckAccessThrowsForDynamicImageAccess() {
		User user = mock(User.class);
		UserAccess access = UserAccess.dynamicImage("admin", "User", "avatar");

		when(user.canAccess(access, "desktop")).thenReturn(false);
		when(user.getName()).thenReturn("tester");

		assertThrows(IllegalArgumentException.class, () -> EXT.checkAccess(user, access, "desktop"));
	}

	private static class TestReceiver implements PushMessageReceiver {
		List<PushMessage> messages = new ArrayList<>();
		private String userId;

		public TestReceiver(String userId) {
			this.userId = userId;
		}

		@Override
		public String forUserId() {
			return userId;
		}

		@Override
		public void sendMessage(PushMessage message) {
			messages.add(message);
		}

		public List<PushMessage> getMessages() {
			return messages;
		}
	}
}
