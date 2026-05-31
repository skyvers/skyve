package org.skyve.impl.sail.mock;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.MessageSeverity;

@SuppressWarnings("static-method")
class MockSupportClassesTest {
	@Test
	void mockElContextReturnsNullMappersAndResolver() {
		MockELContext context = new MockELContext();

		assertNull(context.getELResolver());
		assertNull(context.getFunctionMapper());
		assertNull(context.getVariableMapper());
	}

	@Test
	void mockExpressionFactoryReturnsNullsForAllCreationPaths() {
		MockExpressionFactory factory = new MockExpressionFactory();
		MockELContext context = new MockELContext();

		assertNull(factory.createValueExpression(context, "#{x}", String.class));
		assertNull(factory.createValueExpression("x", String.class));
		assertNull(factory.createMethodExpression(context, "#{x}", Object.class, new Class<?>[0]));
		assertNull(factory.coerceToType("x", String.class));
	}

	@Test
	void mockWebContextNoOpsAndCollectionsAreStable() throws Exception {
		MockWebContext context = new MockWebContext();

		assertNotNull(context.getSessionId());
		assertNotNull(context.getKey());
		context.message(MessageSeverity.info, "m");
		context.growl(MessageSeverity.warn, "g");
		context.cacheConversation();
		context.background(null);
		context.backgroundWithoutCachingConversation(null);
		assertTrue(context.getGrowls().isEmpty());
		assertTrue(context.getMessages().isEmpty());
	}
}
