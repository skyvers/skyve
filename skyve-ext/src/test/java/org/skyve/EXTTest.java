package org.skyve;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.MessageSeverity;
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
	}

	/**
	 * Send some broadcast messages
	 */
	@Test
	@SuppressWarnings({"boxing", "static-method"})
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
	@SuppressWarnings({"boxing", "static-method"})
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
	@SuppressWarnings({"boxing", "static-method"})
	void testReaperRemovesStaleReceiverAndClosesIt() throws Exception {
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

			for (int waitIterations = 4; PushMessage.RECEIVERS.contains(stale) && (waitIterations > 0); --waitIterations) {
				TimeUnit.SECONDS.sleep(1);
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
