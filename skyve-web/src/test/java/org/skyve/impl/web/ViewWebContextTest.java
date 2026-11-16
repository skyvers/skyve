package org.skyve.impl.web;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.impl.job.JobSchedulerStaticSingleton;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.job.JobScheduler;
import org.skyve.metadata.user.User;
import org.skyve.web.BackgroundTask;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;

/**
 * Tests the background task scheduling of {@link ViewWebContext}, which runs a task as the conversation's user.
 */
class ViewWebContextTest {
	private static final String KEY = "view-web-context-key";

	private JobScheduler originalScheduler;
	private final JobScheduler scheduler = mock(JobScheduler.class);

	@BeforeEach
	void installScheduler() {
		originalScheduler = JobSchedulerStaticSingleton.get();
		JobSchedulerStaticSingleton.set(scheduler);
	}

	@AfterEach
	void restoreScheduler() {
		JobSchedulerStaticSingleton.set(originalScheduler);
	}

	@Test
	void backgroundCachesTheConversationThenRunsTheTaskAsTheConversationUser() {
		User user = mock(User.class);
		CachingCountingWebContext context = new CachingCountingWebContext(conversationAs(user));

		context.background(Task.class);

		assertEquals(1, context.cacheCount);
		verify(scheduler).runBackgroundTask(Task.class, user, KEY);
	}

	@Test
	void backgroundWithoutCachingConversationRunsTheTaskAsTheConversationUser() {
		User user = mock(User.class);
		CachingCountingWebContext context = new CachingCountingWebContext(conversationAs(user));

		context.backgroundWithoutCachingConversation(Task.class);

		assertEquals(0, context.cacheCount);
		verify(scheduler).runBackgroundTask(Task.class, user, KEY);
	}

	@Test
	void backgroundWithoutAConversationIsAnInvariantFailure() {
		CachingCountingWebContext context = new CachingCountingWebContext(null);

		IllegalStateException e = assertThrows(IllegalStateException.class,
												() -> context.backgroundWithoutCachingConversation(Task.class));

		assertTrue(e.getMessage().contains("has no conversation to run a background task as"), e.getMessage());
		assertTrue(e.getMessage().contains(KEY), e.getMessage());
		verify(scheduler, never()).runBackgroundTask(any(), any(), anyString());
	}

	private static AbstractPersistence conversationAs(User user) {
		AbstractPersistence result = mock(AbstractPersistence.class);
		when(result.getUser()).thenReturn(user);
		return result;
	}

	/**
	 * Counts conversation caching instead of putting the conversation in the conversation cache.
	 */
	private static final class CachingCountingWebContext extends ViewWebContext {
		private static final long serialVersionUID = 1L;

		private int cacheCount = 0;

		private CachingCountingWebContext(AbstractPersistence conversation) {
			super(KEY, request());
			setConversation(conversation);
		}

		@Override
		public void cacheConversationAndCycleTransaction() {
			cacheCount++;
		}

		@Override
		public List<Map<String, String>> getGrowls() {
			return null;
		}

		@Override
		public List<Map<String, String>> getMessages() {
			return null;
		}

		@Override
		public void growl(MessageSeverity severity, String message) {
			// not shown
		}

		@Override
		public void message(MessageSeverity severity, String message) {
			// not shown
		}

		private static HttpServletRequest request() {
			HttpServletRequest result = mock(HttpServletRequest.class);
			HttpSession session = mock(HttpSession.class);
			when(session.getId()).thenReturn("view-web-context-session");
			when(result.getSession(false)).thenReturn(session);
			return result;
		}
	}

	/**
	 * A background task that is only scheduled, never run.
	 */
	private static final class Task implements BackgroundTask<Bean> {
		@Override
		public Bean getBean() {
			return null;
		}

		@Override
		public void cacheConversationAndCycleTransaction() {
			// never run
		}

		@Override
		public void execute(Bean bean) {
			// never run
		}
	}
}
