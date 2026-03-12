package org.skyve;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.util.PushMessage;
import org.skyve.util.PushMessage.PushMessageReceiver;

public class EXTTest {

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

		assertThat(recvA.getMessages().size(), is(1));
		assertThat(recvB.getMessages().size(), is(1));

		EXT.push(growlBroadcastMessage());

		assertThat(recvA.getMessages().size(), is(2));
		assertThat(recvB.getMessages().size(), is(2));
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

		assertThat(recvBatman.getMessages().size(), is(2));
		assertThat(recvJoker.getMessages().size(), is(1));

		EXT.push(growlBroadcastMessage().user(JOKER_USERID));
		EXT.push(growlBroadcastMessage().user(JOKER_USERID));
		EXT.push(growlBroadcastMessage().user(JOKER_USERID).user(BATMAN_USERID));
		EXT.push(growlBroadcastMessage().user("catwoman"));

		assertThat(recvBatman.getMessages().size(), is(3));
		assertThat(recvJoker.getMessages().size(), is(4));
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

			assertThat(PushMessage.RECEIVERS.contains(stale), is(false));
			assertThat(closed.get(), is(true));
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
