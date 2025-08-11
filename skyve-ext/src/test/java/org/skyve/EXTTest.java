package org.skyve;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.util.PushMessage;
import org.skyve.util.PushMessage.PushMessageReceiver;

public class EXTTest {

	private static final String JOKER_USERID = "joker";
	private static final String BATMAN_USERID = "batman";

	@BeforeEach
	public void clearReceivers() {
		PushMessage.RECEIVERS.clear();
	}

	/**
	 * Empty receiver queue, just looking for 0 exceptions
	 */
	@Test
	public void testPushWithNoReceivers() {

		EXT.push(growlBroadcastMessage());
	}

	/**
	 * Send some broadcast messages
	 */
	@Test
	public void testPushBroadcastReceived() {

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
	public void testPushAddressedMessagesReceived() {

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

	private PushMessage growlBroadcastMessage() {
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
