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

public class AbstractWebContextTest {

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
	public void setUp() {
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
	public void contextNameConstant() {
		assertThat(AbstractWebContext.CONTEXT_NAME, is("_c"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void csrfTokenNameConstant() {
		assertNotNull(AbstractWebContext.CSRF_TOKEN_NAME);
	}

	// ---- key ----

	@Test
	public void getKeyReturnsConstructorValue() {
		assertThat(ctx.getKey(), is("testKey"));
	}

	@Test
	public void setKeyUpdatesKey() {
		ctx.setKey("newKey");
		assertThat(ctx.getKey(), is("newKey"));
	}

	// ---- sessionId ----

	@Test
	public void sessionIdIsNullByDefault() {
		assertNull(ctx.getSessionId());
	}

	// ---- action ----

	@Test
	public void getActionIsNullByDefault() {
		assertNull(ctx.getAction());
	}

	@Test
	public void setActionRoundtrip() {
		ctx.setAction("SaveAction");
		assertThat(ctx.getAction(), is("SaveAction"));
	}

	// ---- conversation ----

	@Test
	public void conversationIsNullByDefault() {
		assertNull(ctx.getConversation());
	}

	// ---- currentBean / getBean ----

	@Test
	public void getCurrentBeanThrowsWhenNotSet() {
		assertThrows(IllegalStateException.class, ctx::getCurrentBean);
	}

	@Test
	public void getNullableCurrentBeanReturnsNullWhenNotSet() {
		assertNull(ctx.getNullableCurrentBean());
	}

	@Test
	public void setCurrentBeanMakesBeanRetrievable() {
		DynamicBean bean = beanWithId("bean-001");
		ctx.setCurrentBean(bean);
		assertSame(bean, ctx.getCurrentBean());
		assertSame(bean, ctx.getNullableCurrentBean());
	}

	@Test
	public void setCurrentBeanRegistersInContextBeans() {
		DynamicBean bean = beanWithId("bean-002");
		ctx.setCurrentBean(bean);
		Bean retrieved = ctx.getBean("bean-002");
		assertSame(bean, retrieved);
	}

	@Test
	public void getBeanReturnsNullForUnknownId() {
		assertNull(ctx.getBean("no-such-id"));
	}

	@Test
	public void setCurrentBeanToNullRemovesFromContextBeans() {
		DynamicBean bean = beanWithId("bean-003");
		ctx.setCurrentBean(bean);
		assertNotNull(ctx.getBean("bean-003"));
		ctx.setCurrentBean(null);
		assertThat(ctx.getBean("bean-003"), is(nullValue()));
	}

	// ---- webId ----

	@Test
	public void getWebIdWithNoBeanReturnsKey() {
		assertThat(ctx.getWebId(), is("testKey"));
	}

	@Test
	public void getWebIdWithBeanCombinesKeyAndBizId() {
		DynamicBean bean = beanWithId("myBizId");
		ctx.setCurrentBean(bean);
		assertThat(ctx.getWebId(), is("testKeymyBizId"));
	}

	// ---- toString ----

	@Test
	public void toStringContainsKey() {
		assertThat(ctx.toString(), containsString("testKey"));
	}
}
