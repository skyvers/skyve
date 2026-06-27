package org.skyve.impl.sail.mock;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.skyve.domain.Bean;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.web.BackgroundTask;

/**
 * Provides a minimal {@link org.skyve.web.WebContext} implementation for SAIL test execution.
 *
 * <p>Threading: instances are test-scoped and not thread-safe.
 */
public final class MockWebContext extends AbstractWebContext {
	private static final long serialVersionUID = 2494440501288342409L;

	/**
	 * Creates a mock web context with generated conversation and session identifiers.
	 */
	public MockWebContext() {
		super(UUID.randomUUID().toString());
		sessionId = UUID.randomUUID().toString();
	}

	/**
	 * Ignores severity messages in test mode.
	 */
	@Override
	public void message(MessageSeverity severity, String message) {
		// do nothing
	}

	/**
	 * Ignores growl notifications in test mode.
	 */
	@Override
	public void growl(MessageSeverity severity, String message) {
		// do nothing
	}

	/**
	 * Skips conversation caching for mock execution.
	 */
	@Override
	public void cacheConversation() throws Exception {
		// do nothing
	}

	/**
	 * Skips background scheduling for mock execution.
	 */
	@Override
	public <T extends Bean> void background(Class<? extends BackgroundTask<T>> taskClass) throws Exception {
		// do nothing
	}

	/**
	 * Skips uncached background scheduling for mock execution.
	 */
	@Override
	public <T extends Bean> void backgroundWithoutCachingConversation(Class<? extends BackgroundTask<T>> taskClass)
	throws Exception {
		// do nothing
	}

	/**
	 * Returns an empty growl collection for tests.
	 */
	@Override
	public List<Map<String, String>> getGrowls() {
		return Collections.emptyList();
	}

	/**
	 * Returns an empty message collection for tests.
	 */
	@Override
	public List<Map<String, String>> getMessages() {
		return Collections.emptyList();
	}
}
