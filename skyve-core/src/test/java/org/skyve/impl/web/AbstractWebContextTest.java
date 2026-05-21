package org.skyve.impl.web;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.web.BackgroundTask;

class AbstractWebContextTest {

	/** Minimal concrete subclass for testing. */
	private static class TestWebContext extends AbstractWebContext {
		private static final long serialVersionUID = 1L;

		TestWebContext(String key) {
			super(key);
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
		public void message(MessageSeverity severity, String message) {
			// no-op
		}

		@Override
		public void growl(MessageSeverity severity, String message) {
			// no-op
		}

		@Override
		public void cacheConversation() throws Exception {
			// no-op
		}

		@Override
		public <T extends Bean> void background(Class<? extends BackgroundTask<T>> taskClass) throws Exception {
			// no-op
		}

		@Override
		public <T extends Bean> void backgroundWithoutCachingConversation(Class<? extends BackgroundTask<T>> taskClass) throws Exception {
			// no-op
		}
	}

	private TestWebContext ctx;

	@BeforeEach
	void setUp() {
		ctx = new TestWebContext("testKey");
	}

	private static DynamicBean beanWithId(String id) {
		Map<String, Object> props = new HashMap<>();
		props.put(Bean.DOCUMENT_ID, id);
		return new DynamicBean("admin", "User", props);
	}

	// ---- constants ----

	@Test
	@SuppressWarnings("static-method")
	void contextNameConstant() {
		assertThat(AbstractWebContext.CONTEXT_NAME, is("_c"));
	}

	@Test
	@SuppressWarnings("static-method")
	void csrfTokenNameConstant() {
		assertNotNull(AbstractWebContext.CSRF_TOKEN_NAME);
	}

	// ---- key ----

	@Test
	void getKeyReturnsConstructorValue() {
		assertThat(ctx.getKey(), is("testKey"));
	}

	@Test
	void setKeyUpdatesKey() {
		ctx.setKey("newKey");
		assertThat(ctx.getKey(), is("newKey"));
	}

	// ---- sessionId ----

	@Test
	void sessionIdIsNullByDefault() {
		assertNull(ctx.getSessionId());
	}

	// ---- action ----

	@Test
	void getActionIsNullByDefault() {
		assertNull(ctx.getAction());
	}

	@Test
	void setActionRoundtrip() {
		ctx.setAction("SaveAction");
		assertThat(ctx.getAction(), is("SaveAction"));
	}

	// ---- conversation ----

	@Test
	void conversationIsNullByDefault() {
		assertNull(ctx.getConversation());
	}

	@Test
	void setConversationRoundtrip() {
		ctx.setConversation(null);
		assertNull(ctx.getConversation());
	}

	// ---- currentBean / getBean ----

	@Test
	void getCurrentBeanThrowsWhenNotSet() {
		assertThrows(IllegalStateException.class, ctx::getCurrentBean);
	}

	@Test
	void getNullableCurrentBeanReturnsNullWhenNotSet() {
		assertNull(ctx.getNullableCurrentBean());
	}

	@Test
	void setCurrentBeanMakesBeanRetrievable() {
		DynamicBean bean = beanWithId("bean-001");
		ctx.setCurrentBean(bean);
		assertSame(bean, ctx.getCurrentBean());
		assertSame(bean, ctx.getNullableCurrentBean());
	}

	@Test
	void setCurrentBeanRegistersInContextBeans() {
		DynamicBean bean = beanWithId("bean-002");
		ctx.setCurrentBean(bean);
		Bean retrieved = ctx.getBean("bean-002");
		assertSame(bean, retrieved);
	}

	@Test
	void getBeanReturnsNullForUnknownId() {
		assertNull(ctx.getBean("no-such-id"));
	}

	@Test
	void setCurrentBeanToNullRemovesFromContextBeans() {
		DynamicBean bean = beanWithId("bean-003");
		ctx.setCurrentBean(bean);
		assertNotNull(ctx.getBean("bean-003"));
		ctx.setCurrentBean(null);
		assertThat(ctx.getBean("bean-003"), is(nullValue()));
	}

	// ---- webId ----

	@Test
	void getWebIdWithNoBeanReturnsKey() {
		assertThat(ctx.getWebId(), is("testKey"));
	}

	@Test
	void getWebIdWithBeanCombinesKeyAndBizId() {
		DynamicBean bean = beanWithId("myBizId");
		ctx.setCurrentBean(bean);
		assertThat(ctx.getWebId(), is("testKeymyBizId"));
	}

	// ---- toString ----

	@Test
	void toStringContainsKey() {
		assertThat(ctx.toString(), containsString("testKey"));
	}
}
