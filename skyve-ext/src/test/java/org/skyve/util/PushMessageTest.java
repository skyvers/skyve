package org.skyve.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.metadata.user.User;

@SuppressWarnings({"static-method", "boxing"})
public class PushMessageTest {

	@Before
	public void setUp() {
		// Ensure reaper is stopped before each test
		PushMessage.stopReaper();
		PushMessage.RECEIVERS.clear();
	}

	@After
	public void tearDown() {
		// Ensure reaper is always stopped after each test
		PushMessage.stopReaper();
		PushMessage.RECEIVERS.clear();
	}

	// ======== user(String) ========

	@Test
	public void testUserStringAddsToUserIds() {
		PushMessage msg = new PushMessage().user("user-1");
		Set<String> ids = msg.getUserIds();
		assertTrue("user-1 should be in userIds", ids.contains("user-1"));
	}

	@Test
	public void testUserStringMultiple() {
		PushMessage msg = new PushMessage().user("a").user("b").user("c");
		assertEquals(3, msg.getUserIds().size());
	}

	@Test
	public void testUserStringIsSortedSet() {
		PushMessage msg = new PushMessage().user("c").user("a").user("b");
		List<String> ordered = new java.util.ArrayList<>(msg.getUserIds());
		assertEquals("a", ordered.get(0));
		assertEquals("b", ordered.get(1));
		assertEquals("c", ordered.get(2));
	}

	// ======== user(User) ========

	@Test
	public void testUserObjectAddsToUserIds() {
		User user = mock(User.class);
		when(user.getId()).thenReturn("mocked-user-id");
		PushMessage msg = new PushMessage().user(user);
		assertTrue(msg.getUserIds().contains("mocked-user-id"));
	}

	// ======== getItems() ========

	@Test
	public void testGetItemsEmptyOnNewMessage() {
		PushMessage msg = new PushMessage();
		assertNotNull(msg.getItems());
		assertTrue(msg.getItems().isEmpty());
	}

	// ======== growl() ========

	@Test
	public void testGrowlAddsItemWithCorrectType() {
		PushMessage msg = new PushMessage().growl(MessageSeverity.info, "Hello");
		List<Map<String, Object>> items = msg.getItems();
		assertEquals(1, items.size());
		assertEquals("g", items.get(0).get("type"));
	}

	@Test
	public void testGrowlAddsItemWithSeverity() {
		PushMessage msg = new PushMessage().growl(MessageSeverity.warn, "Watch out");
		assertEquals("warn", msg.getItems().get(0).get("severity"));
	}

	@Test
	public void testGrowlAddsItemWithMessage() {
		PushMessage msg = new PushMessage().growl(MessageSeverity.error, "Bad thing");
		assertNotNull(msg.getItems().get(0).get("message"));
	}

	@Test
	public void testGrowlReturnsSelf() {
		PushMessage msg = new PushMessage();
		assertSame(msg, msg.growl(MessageSeverity.info, "Test"));
	}

	// ======== message() ========

	@Test
	public void testMessageAddsItemWithCorrectType() {
		PushMessage msg = new PushMessage().message(MessageSeverity.info, "Hello");
		List<Map<String, Object>> items = msg.getItems();
		assertEquals(1, items.size());
		assertEquals("m", items.get(0).get("type"));
	}

	@Test
	public void testMessageAddsItemWithSeverity() {
		PushMessage msg = new PushMessage().message(MessageSeverity.error, "Error");
		assertEquals("error", msg.getItems().get(0).get("severity"));
	}

	@Test
	public void testMessageReturnsSelf() {
		PushMessage msg = new PushMessage();
		assertSame(msg, msg.message(MessageSeverity.info, "Test"));
	}

	// ======== rerender() ========

	@Test
	public void testRerenderAddsItemWithCorrectType() {
		PushMessage msg = new PushMessage().rerender();
		List<Map<String, Object>> items = msg.getItems();
		assertEquals(1, items.size());
		assertEquals("r", items.get(0).get("type"));
	}

	@Test
	public void testRerenderReturnsSelf() {
		PushMessage msg = new PushMessage();
		assertSame(msg, msg.rerender());
	}

	// ======== execute() ========

	@Test
	public void testExecuteAddsItemWithCorrectType() {
		Map<String, Object> args = new HashMap<>();
		args.put("key", "value");
		PushMessage msg = new PushMessage().execute("myFunction", args);
		List<Map<String, Object>> items = msg.getItems();
		assertEquals(1, items.size());
		assertEquals("j", items.get(0).get("type"));
	}

	@Test
	public void testExecuteAddsMethodName() {
		Map<String, Object> args = new HashMap<>();
		PushMessage msg = new PushMessage().execute("doSomething", args);
		assertEquals("doSomething", msg.getItems().get(0).get("method"));
	}

	@Test
	public void testExecuteAddsArgument() {
		Map<String, Object> args = new HashMap<>();
		args.put("x", Integer.valueOf(42));
		PushMessage msg = new PushMessage().execute("fn", args);
		@SuppressWarnings("unchecked")
		Map<String, Object> argMap = (Map<String, Object>) msg.getItems().get(0).get("argument");
		assertEquals(Integer.valueOf(42), argMap.get("x"));
	}

	@Test
	public void testExecuteReturnsSelf() {
		PushMessage msg = new PushMessage();
		assertSame(msg, msg.execute("fn", new HashMap<>()));
	}

	// ======== chaining ========

	@Test
	public void testChainingMultipleItems() {
		PushMessage msg = new PushMessage()
				.user("u1")
				.growl(MessageSeverity.info, "Hi")
				.message(MessageSeverity.warn, "Warning")
				.rerender();
		assertEquals(1, msg.getUserIds().size());
		assertEquals(3, msg.getItems().size());
	}

	// ======== toString() ========

	@Test
	public void testToStringContainsUserIds() {
		PushMessage msg = new PushMessage().user("user-xyz");
		String s = msg.toString();
		assertTrue("toString should contain userIds", s.contains("userIds"));
	}

	@Test
	public void testToStringContainsItems() {
		PushMessage msg = new PushMessage().rerender();
		String s = msg.toString();
		assertTrue("toString should contain items", s.contains("items"));
	}

	// ======== startReaper / stopReaper ========

	@Test
	public void testStartAndStopReaperDoesNotThrow() {
		PushMessage.startReaper(10);
		PushMessage.stopReaper();
		// No exception = pass
	}

	@Test
	public void testStartReaperTwiceIsIgnored() {
		PushMessage.startReaper(10);
		// Starting again should log a warning but not throw
		PushMessage.startReaper(10);
		PushMessage.stopReaper();
	}

	@Test
	public void testStopReaperWhenNotStartedDoesNotThrow() {
		// Already stopped in setUp(); calling again should be safe
		PushMessage.stopReaper();
	}

	// ======== RECEIVERS queue ========

	@Test
	public void testReceiversQueueIsInitiallyEmpty() {
		assertTrue(PushMessage.RECEIVERS.isEmpty());
	}

	@Test
	public void testReceiversQueueCanAddAndRemove() {
		PushMessage.PushMessageReceiver receiver = mock(PushMessage.PushMessageReceiver.class);
		when(receiver.isStale()).thenReturn(Boolean.FALSE);
		PushMessage.RECEIVERS.add(receiver);
		assertFalse(PushMessage.RECEIVERS.isEmpty());
		PushMessage.RECEIVERS.remove(receiver);
		assertTrue(PushMessage.RECEIVERS.isEmpty());
	}

        // ======== PushMessageReceiver default methods ========

        @Test
        public void testPushMessageReceiverDefaultIsStaleReturnsFalse() {
                // covers the isStale() default method body
                PushMessage.PushMessageReceiver receiver = new PushMessage.PushMessageReceiver() {
                        @Override public String forUserId() { return "u"; }
                        @Override public void sendMessage(PushMessage message) { /* no-op */ }
                };
                assertFalse(receiver.isStale());
        }

        @Test
        public void testPushMessageReceiverDefaultCloseIsNoOp() {
                // covers L268: default close() method closing brace
                PushMessage.PushMessageReceiver receiver = new PushMessage.PushMessageReceiver() {
                        @Override public String forUserId() { return "u"; }
                        @Override public void sendMessage(PushMessage message) { /* no-op */ }
                };
                receiver.close(); // should not throw
                assertTrue(true); // verifies no exception was thrown
        }

        // ======== reaper stale / non-stale behaviour ========

        @Test
        public void testReaperRemovesStaleReceiverAndCallsClose() throws InterruptedException {
                // covers L83-88 (remove + close call) and L73 (isStale() call)
                CountDownLatch closeLatch = new CountDownLatch(1);
                PushMessage.PushMessageReceiver staleReceiver = new PushMessage.PushMessageReceiver() {
                        @Override public String forUserId() { return "stale"; }
                        @Override public void sendMessage(PushMessage message) { /* no-op */ }
                        @Override public boolean isStale() { return true; }
                        @Override public void close() { closeLatch.countDown(); }
                };
                PushMessage.RECEIVERS.add(staleReceiver);
                PushMessage.startReaper(1);
                assertTrue("close() should be called on stale receiver within 3s", closeLatch.await(3, TimeUnit.SECONDS));
                assertTrue("Stale receiver should be removed from RECEIVERS", PushMessage.RECEIVERS.isEmpty());
        }

        @Test
        public void testReaperKeepsNonStaleReceiver() throws InterruptedException {
                // covers L80 (continue when !stale): reaper runs but non-stale receiver is kept
                CountDownLatch reaperRanLatch = new CountDownLatch(1);
                PushMessage.PushMessageReceiver triggerReceiver = new PushMessage.PushMessageReceiver() {
                        @Override public String forUserId() { return "trigger"; }
                        @Override public void sendMessage(PushMessage message) { /* no-op */ }
                        @Override public boolean isStale() { return true; }
                        @Override public void close() { reaperRanLatch.countDown(); }
                };
                PushMessage.PushMessageReceiver activeReceiver = new PushMessage.PushMessageReceiver() {
                        @Override public String forUserId() { return "active"; }
                        @Override public void sendMessage(PushMessage message) { /* no-op */ }
                        @Override public boolean isStale() { return false; }
                };
                PushMessage.RECEIVERS.add(triggerReceiver);
                PushMessage.RECEIVERS.add(activeReceiver);
                PushMessage.startReaper(1);
                assertTrue("Reaper should have run", reaperRanLatch.await(3, TimeUnit.SECONDS));
                assertTrue("Non-stale receiver should remain", PushMessage.RECEIVERS.contains(activeReceiver));
        }

        @Test
        public void testReaperHandlesIsStaleThrowingRuntimeException() throws InterruptedException {
                // covers L75-76: RuntimeException from isStale() → defensive stale=true → close() called
                CountDownLatch closeLatch = new CountDownLatch(1);
                PushMessage.PushMessageReceiver throwingReceiver = new PushMessage.PushMessageReceiver() {
                        @Override public String forUserId() { return "thrower"; }
                        @Override public void sendMessage(PushMessage message) { /* no-op */ }
                        @Override public boolean isStale() { throw new RuntimeException("isStale boom"); }
                        @Override public void close() { closeLatch.countDown(); }
                };
                PushMessage.RECEIVERS.add(throwingReceiver);
                PushMessage.startReaper(1);
                assertTrue("close() should be called when isStale() throws", closeLatch.await(3, TimeUnit.SECONDS));
        }

	@Test
	public void testReaperCatchesExceptionFromClose() throws InterruptedException {
		CountDownLatch closeLatch = new CountDownLatch(1);
		PushMessage.PushMessageReceiver throwingCloseReceiver = new PushMessage.PushMessageReceiver() {
			@Override public String forUserId() { return "closeThrow"; }
			@Override public void sendMessage(PushMessage message) { /* no-op */ }
			@Override public boolean isStale() { return true; }
			@Override public void close() {
				closeLatch.countDown();
				throw new RuntimeException("close() boom");
			}
		};
		PushMessage.RECEIVERS.add(throwingCloseReceiver);
		PushMessage.startReaper(1);
		assertTrue("reaper should call close() even when it throws", closeLatch.await(3, TimeUnit.SECONDS));
	}
}
