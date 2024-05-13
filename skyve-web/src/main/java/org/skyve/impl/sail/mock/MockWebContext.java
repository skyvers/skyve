package org.skyve.impl.sail.mock;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.skyve.domain.Bean;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.web.BackgroundTask;

public final class MockWebContext extends AbstractWebContext {
	private static final long serialVersionUID = 2494440501288342409L;

	public MockWebContext() {
		super(UUID.randomUUID().toString(), null, null);
		sessionId = UUID.randomUUID().toString();
	}

	@Override
	public void message(MessageSeverity severity, String message) {
		// do nothing
	}

	@Override
	public void growl(MessageSeverity severity, String message) {
		// do nothing
	}

	@Override
	public void cacheConversation() throws Exception {
		// do nothing
	}

	@Override
	public <T extends Bean> void background(Class<? extends BackgroundTask<T>> taskClass) throws Exception {
		// do nothing
	}

	@Override
	public <T extends Bean> void backgroundWithoutCachingConversation(Class<? extends BackgroundTask<T>> taskClass)
	throws Exception {
		// do nothing
	}

	@Override
	public List<Map<String, String>> getGrowls() {
		return Collections.emptyList();
	}

	@Override
	public List<Map<String, String>> getMessages() {
		return Collections.emptyList();
	}
}
