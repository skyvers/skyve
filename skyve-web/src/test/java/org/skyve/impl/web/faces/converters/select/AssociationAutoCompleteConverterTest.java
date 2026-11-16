package org.skyve.impl.web.faces.converters.select;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.faces.FacesUtil;
import org.skyve.impl.web.faces.models.BeanMapAdapter;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.web.WebContext;

import jakarta.el.ELContext;
import jakarta.el.ELResolver;
import jakarta.faces.application.FacesMessage;
import jakarta.faces.component.UIViewRoot;
import jakarta.faces.context.FacesContext;
import jakarta.faces.context.PartialViewContext;

class AssociationAutoCompleteConverterTest {
	private abstract static class FacesContextBridge extends FacesContext {
		static void setCurrent(FacesContext context) {
			setCurrentInstance(context);
		}
	}

	private AssociationAutoCompleteConverter converter;

	@BeforeEach
	void before() {
		converter = new AssociationAutoCompleteConverter();
	}

	@AfterEach
	@SuppressWarnings("static-method")
	void after() throws ReflectiveOperationException {
		FacesContextBridge.setCurrent(null);
		clearThreadPersistence();
	}

	@Test
	void getAsStringReturnsEmptyStringForNull() {
		assertEquals("", converter.getAsString(null, null, null));
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void getAsObjectReturnsNullForNull() {
		assertNull(converter.getAsObject(null, null, null));
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void getAsObjectReturnsNullForBlank() {
		assertNull(converter.getAsObject(null, null, "  "));
	}

	@Test
	void getAsStringReturnsEmptyStringForOtherObject() {
		assertEquals("", converter.getAsString(null, null, "notABean"));
	}

	@Test
	void getAsStringFormatsBean() {
		Bean bean = mock(Bean.class);
		when(bean.getBizModule()).thenReturn("modName");
		when(bean.getBizDocument()).thenReturn("docName");
		when(bean.getBizId()).thenReturn("bizId");
		assertEquals("modName.docName.bizId", converter.getAsString(null, null, bean));
	}

	@Test
	void getAsStringFormatsBeanMapAdapter() {
		Bean bean = mock(Bean.class);
		when(bean.getBizModule()).thenReturn("mod");
		when(bean.getBizDocument()).thenReturn("doc");
		when(bean.getBizId()).thenReturn("id");
		WebContext webContext = mock(WebContext.class);
		BeanMapAdapter adapter = new BeanMapAdapter(bean, webContext);
		assertEquals("mod.doc.id", converter.getAsString(null, null, adapter));
	}

	@Test
	void getAsObjectReportsAndReturnsNullWhenTheViewHasNoWebContext() {
		User user = mock(User.class);
		AbstractPersistence persistence = bindPersistenceForUser(user);
		FacesView view = mock(FacesView.class);
		when(view.getWebContext()).thenReturn(null);
		FacesContext context = installViewContext(view);

		assertNull(converter.getAsObject(context, null, "admin.Contact.b1"));

		// FacesAction.execute() turns the IllegalStateException into a global message and rolls back
		verify(persistence).setRollbackOnly();
		verify(context).addMessage(isNull(), any(FacesMessage.class));
		verify(user, never()).getCustomer();
	}

	@Test
	@SuppressWarnings("boxing")
	void getAsObjectFindsTheReferencedBeanThroughTheViewWebContext() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Bean referenced = mock(Bean.class);
		Bean conversationBean = mock(Bean.class);
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);
		when(document.isPersistable()).thenReturn(Boolean.TRUE);
		when(user.canReadBean(any(), any(), any(), any(), any(), any())).thenReturn(Boolean.TRUE);
		AbstractPersistence persistence = bindPersistenceForUser(user);
		doReturn(referenced).when(persistence).retrieve(document, "b1");
		FacesView view = mock(FacesView.class);
		AbstractWebContext webContext = mock(AbstractWebContext.class);
		when(view.getWebContext()).thenReturn(webContext);
		when(view.getCurrentBean()).thenReturn(new BeanMapAdapter(conversationBean, webContext));
		FacesContext context = installViewContext(view);

		assertSame(referenced, converter.getAsObject(context, null, "admin.Contact.b1"));
	}

	/**
	 * Installs a current Faces context whose view root names a managed bean that resolves to the given view.
	 */
	private static FacesContext installViewContext(FacesView view) {
		FacesContext context = mock(FacesContext.class);
		UIViewRoot root = mock(UIViewRoot.class);
		Map<String, Object> rootAttributes = new HashMap<>();
		rootAttributes.put(FacesUtil.MANAGED_BEAN_NAME_KEY, "skyve");
		when(root.getAttributes()).thenReturn(rootAttributes);
		when(context.getViewRoot()).thenReturn(root);
		ELContext elContext = mock(ELContext.class);
		ELResolver resolver = mock(ELResolver.class);
		when(context.getELContext()).thenReturn(elContext);
		when(elContext.getELResolver()).thenReturn(resolver);
		when(resolver.getValue(elContext, null, "skyve")).thenReturn(view);
		PartialViewContext partial = mock(PartialViewContext.class);
		when(partial.getRenderIds()).thenReturn(new HashSet<>());
		when(context.getPartialViewContext()).thenReturn(partial);
		FacesContextBridge.setCurrent(context);
		return context;
	}

	private static AbstractPersistence bindPersistenceForUser(User user) {
		AbstractPersistence persistence = mock(AbstractPersistence.class, CALLS_REAL_METHODS);
		persistence.setUser(user);
		persistence.setForThread();
		return persistence;
	}

	@SuppressWarnings({"unchecked", "java:S3011"}) // Test cleanup must access the framework-owned thread-local
	private static void clearThreadPersistence() throws ReflectiveOperationException {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		((ThreadLocal<AbstractPersistence>) field.get(null)).remove();
	}
}
