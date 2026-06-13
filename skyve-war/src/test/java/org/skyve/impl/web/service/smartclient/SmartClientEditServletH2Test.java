package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.SortedMap;
import java.util.TreeMap;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.ServletConstants;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.test.AbstractSkyveTest;
import modules.test.domain.AllAttributesPersistent;

class SmartClientEditServletH2Test extends AbstractSkyveTest {
	private static final String UXUI = "external";

	@Test
	void fetchCreatesTopLevelBeanAndRendersViewJson() throws Exception {
		String body = invokeFetch(null, null, null, new TreeMap<>());

		assertSuccessfulEditFetch(body);
	}

	@Test
	void fetchRetrievesPersistedTopLevelBeanAndRendersViewJson() throws Exception {
		AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
		bean.setText("edit-fetch-" + System.nanoTime());
		bean = p.save(bean);

		String body = invokeFetch(bean.getBizId(), null, null, new TreeMap<>());

		assertSuccessfulEditFetch(body);
		assertTrue(body.contains(bean.getBizId()));
	}

	@Test
	void fetchCreatesTopLevelBeanForRerenderSource() throws Exception {
		String body = invokeFetch(null, "text", ImplicitActionName.OK, new TreeMap<>());

		assertSuccessfulEditFetch(body);
	}

	@Test
	void fetchRetrievesPersistedTopLevelBeanForRerenderSource() throws Exception {
		AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
		bean.setText("edit-rerender-" + System.nanoTime());
		bean = p.save(bean);

		String body = invokeFetch(bean.getBizId(), "text", ImplicitActionName.OK, new TreeMap<>());

		assertSuccessfulEditFetch(body);
		assertTrue(body.contains(bean.getBizId()));
	}

	@Test
	void removeDeletesPersistedTopLevelBeanAndWritesSuccessPayload() throws Exception {
		AllAttributesPersistent bean = new DataBuilder().fixture(FixtureType.crud)
														.build(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);
		bean.setText("edit-delete-" + System.nanoTime());
		bean = p.save(bean);

		String body = invokeRemove(bean);

		assertTrue(body.contains("\"status\":0"), body);
		assertNull(p.retrieve(aapd, bean.getBizId()));
	}

	@Test
	void pumpOutResponseRendersCurrentBeanWithRedirectUrl() throws Exception {
		AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
		bean.setText("edit-response-" + System.nanoTime());
		AbstractWebContext webContext = mockWebContext();
		webContext.setConversation((AbstractPersistence) p);
		webContext.setCurrentBean(bean);
		View view = aapd.getView(UXUI, c, ViewType.create.toString());
		StringWriter sink = new StringWriter();

		try (PrintWriter writer = new PrintWriter(sink, true)) {
			Method pumpOutResponse = SmartClientEditServlet.class.getDeclaredMethod("pumpOutResponse",
					AbstractWebContext.class,
					org.skyve.metadata.user.User.class,
					CustomerImpl.class,
					org.skyve.metadata.module.Module.class,
					org.skyve.metadata.model.document.Document.class,
					View.class,
					String.class,
					Bean.class,
					Bizlet.class,
					int.class,
					int.class,
					String.class,
					PrintWriter.class);
			pumpOutResponse.setAccessible(true);
			try {
				pumpOutResponse.invoke(null,
										webContext,
										u,
										(CustomerImpl) c,
										m,
										aapd,
										view,
										UXUI,
										bean,
										null,
										Integer.valueOf(0),
										Integer.valueOf(0),
										"/download/test",
										writer);
			}
			catch (InvocationTargetException e) {
				Throwable cause = e.getCause();
				if (cause instanceof Exception exception) {
					throw exception;
				}
				throw e;
			}
		}

		String body = sink.toString();
		assertTrue(body.contains("\"status\":0"), body);
		assertTrue(body.contains("\"data\":"), body);
	}

	@Test
	void applyPushActionSkipsJsonApplyAndRendersResponse() throws Exception {
		AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
		bean.setText("edit-push-" + System.nanoTime());
		AbstractWebContext webContext = mockWebContext();
		webContext.setConversation((AbstractPersistence) p);
		webContext.setCurrentBean(bean);
		StringWriter sink = new StringWriter();

		try (PrintWriter writer = new PrintWriter(sink, true)) {
			Method apply = SmartClientEditServlet.class.getDeclaredMethod("apply",
					AbstractWebContext.class,
					org.skyve.metadata.user.User.class,
					org.skyve.metadata.customer.Customer.class,
					org.skyve.metadata.module.Module.class,
					org.skyve.metadata.model.document.Document.class,
					Bean.class,
					org.skyve.metadata.model.document.Document.class,
					Bean.class,
					String.class,
					String.class,
					String.class,
					ImplicitActionName.class,
					String.class,
					int.class,
					int.class,
					SortedMap.class,
					AbstractPersistence.class,
					String.class,
					PrintWriter.class);
			apply.setAccessible(true);
			try {
				apply.invoke(null,
								webContext,
								u,
								c,
								m,
								aapd,
								bean,
								aapd,
								bean,
								null,
								null,
								null,
								null,
								ServletConstants.PUSH_ACTION_NAME,
								Integer.valueOf(0),
								Integer.valueOf(0),
								new TreeMap<>(),
								p,
								UXUI,
								writer);
			}
			catch (InvocationTargetException e) {
				Throwable cause = e.getCause();
				if (cause instanceof Exception exception) {
					throw exception;
				}
				throw e;
			}
		}

		String body = sink.toString();
		assertTrue(body.contains("\"status\":0"), body);
		assertTrue(body.contains("\"data\":"), body);
	}

	private String invokeFetch(String bizId,
								String source,
								ImplicitActionName action,
								SortedMap<String, Object> parameters)
	throws Exception {
		AbstractWebContext webContext = mockWebContext();
		webContext.setConversation((AbstractPersistence) p);
		StringWriter sink = new StringWriter();
		try (PrintWriter writer = new PrintWriter(sink, true)) {
			Method fetch = SmartClientEditServlet.class.getDeclaredMethod("fetch",
					AbstractWebContext.class,
					org.skyve.metadata.user.User.class,
					org.skyve.metadata.customer.Customer.class,
					Bean.class,
					org.skyve.metadata.module.Module.class,
					org.skyve.metadata.model.document.Document.class,
					String.class,
					String.class,
					String.class,
					int.class,
					int.class,
					ImplicitActionName.class,
					SortedMap.class,
					AbstractPersistence.class,
					String.class,
					PrintWriter.class);
			fetch.setAccessible(true);
			try {
				fetch.invoke(null,
								webContext,
								u,
								c,
								null,
								m,
								aapd,
								null,
								source,
								bizId,
								Integer.valueOf(0),
								Integer.valueOf(0),
								action,
								parameters,
								p,
								UXUI,
								writer);
			}
			catch (InvocationTargetException e) {
				Throwable cause = e.getCause();
				if (cause instanceof Exception exception) {
					throw exception;
				}
				throw e;
			}
		}
		return sink.toString();
	}

	private String invokeRemove(PersistentBean beanToDelete) throws Exception {
		AbstractWebContext webContext = mockWebContext();
		webContext.setConversation((AbstractPersistence) p);
		Bizlet<?> bizlet = ((DocumentImpl) aapd).getBizlet(c);
		StringWriter sink = new StringWriter();
		try (PrintWriter writer = new PrintWriter(sink, true)) {
			Method remove = SmartClientEditServlet.class.getDeclaredMethod("remove",
					AbstractWebContext.class,
					org.skyve.metadata.user.User.class,
					org.skyve.metadata.customer.Customer.class,
					org.skyve.metadata.model.document.Document.class,
					PersistentBean.class,
					Bizlet.class,
					AbstractPersistence.class,
					PrintWriter.class);
			remove.setAccessible(true);
			try {
				remove.invoke(null,
								webContext,
								u,
								(CustomerImpl) c,
								aapd,
								beanToDelete,
								bizlet,
								p,
								writer);
			}
			catch (InvocationTargetException e) {
				Throwable cause = e.getCause();
				if (cause instanceof Exception exception) {
					throw exception;
				}
				throw e;
			}
		}
		return sink.toString();
	}

	private static void assertSuccessfulEditFetch(String body) {
		assertTrue(body.contains("\"status\":0"), body);
		assertTrue(body.contains("\"totalRows\":1"), body);
		assertTrue(body.contains("\"data\":["), body);
	}
}
