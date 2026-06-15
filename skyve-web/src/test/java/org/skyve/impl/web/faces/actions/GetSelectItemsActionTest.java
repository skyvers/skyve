package org.skyve.impl.web.faces.actions;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.user.User;
import org.skyve.web.WebContext;

import jakarta.faces.model.SelectItem;

@SuppressWarnings("static-method")
class GetSelectItemsActionTest {
	private ProvidedRepository originalRepository;

	@BeforeEach
	void setUp() {
		originalRepository = ProvidedRepositoryFactory.get();
	}

	@AfterEach
	void tearDown() throws Exception {
		ProvidedRepositoryFactory.set(originalRepository);
		clearThreadPersistence();
	}

	@Test
	void callbackAddsSelectableEmptyItemForOptionalConstantDomain() throws Exception {
		ModuleImpl module = new ModuleImpl();
		module.setName("testModule");
		DocumentImpl document = new DocumentImpl();
		document.setName("TestDoc");
		document.setOwningModuleName("testModule");

		Text choice = new Text();
		choice.setName("choice");
		choice.setLength(20);
		choice.setDomainType(DomainType.constant);
		document.putAttribute(choice);

		ProvidedRepository repository = mock(ProvidedRepository.class);
		when(repository.getDocument(any(), eq(module), eq("TestDoc"))).thenReturn(document);
		ProvidedRepositoryFactory.set(repository);

		CustomerImpl customer = mock(CustomerImpl.class);
		when(customer.getModule("testModule")).thenReturn(module);
		when(customer.getConstantDomainValues(null, "testModule", "TestDoc", choice)).thenReturn(List.of(
				new DomainValue("A", "Alpha"),
				new DomainValue("B", "Beta")));

		bindPersistence(customer);

		Bean bean = mock(Bean.class);
		when(bean.getBizModule()).thenReturn("testModule");
		when(bean.getBizDocument()).thenReturn("TestDoc");
		WebContext webContext = mock(WebContext.class);

		List<SelectItem> result = new GetSelectItemsAction(bean, webContext, "choice", true).callback();

		assertEquals(3, result.size());
		assertEquals(null, result.get(0).getValue());
		assertEquals("", result.get(0).getLabel());
		assertEquals("A", result.get(1).getValue());
		assertEquals("Alpha", result.get(1).getLabel());
		assertEquals("B", result.get(2).getValue());
		assertEquals("Beta", result.get(2).getLabel());
	}

	@Test
	void callbackAddsUnselectableEmptyItemWhenRequiredConstantDomainUsesBeanConstructor() throws Exception {
		ModuleImpl module = new ModuleImpl();
		module.setName("testModule");
		DocumentImpl document = new DocumentImpl();
		document.setName("TestDoc");
		document.setOwningModuleName("testModule");

		Text choice = new Text();
		choice.setName("choice");
		choice.setLength(20);
		choice.setRequired(true);
		choice.setDomainType(DomainType.constant);
		document.putAttribute(choice);

		ProvidedRepository repository = mock(ProvidedRepository.class);
		when(repository.getDocument(any(), eq(module), eq("TestDoc"))).thenReturn(document);
		ProvidedRepositoryFactory.set(repository);

		CustomerImpl customer = mock(CustomerImpl.class);
		when(customer.getModule("testModule")).thenReturn(module);
		when(customer.getConstantDomainValues(null, "testModule", "TestDoc", choice)).thenReturn(List.of(
				new DomainValue("A", "Alpha")));

		bindPersistence(customer);

		Bean bean = mock(Bean.class);
		when(bean.getBizModule()).thenReturn("testModule");
		when(bean.getBizDocument()).thenReturn("TestDoc");
		WebContext webContext = mock(WebContext.class);

		List<SelectItem> result = new GetSelectItemsAction(bean, webContext, "choice", true).callback();

		assertEquals(2, result.size());
		assertEquals(null, result.get(0).getValue());
		assertEquals("", result.get(0).getLabel());
		assertEquals("A", result.get(1).getValue());
		assertEquals("Alpha", result.get(1).getLabel());
	}

	@Test
	void callbackReturnsEmptyListForDynamicDomainWithoutOwningBean() throws Exception {
		ModuleImpl module = new ModuleImpl();
		module.setName("testModule");
		DocumentImpl document = new DocumentImpl();
		document.setName("TestDoc");
		document.setOwningModuleName("testModule");

		Text choice = new Text();
		choice.setName("choice");
		choice.setLength(20);
		choice.setDomainType(DomainType.dynamic);
		document.putAttribute(choice);

		ProvidedRepository repository = mock(ProvidedRepository.class);
		when(repository.getDocument(any(), eq(module), eq("TestDoc"))).thenReturn(document);
		ProvidedRepositoryFactory.set(repository);

		CustomerImpl customer = mock(CustomerImpl.class);
		when(customer.getModule("testModule")).thenReturn(module);
		bindPersistence(customer);

		List<SelectItem> result = new GetSelectItemsAction("testModule", "TestDoc", "choice", false).callback();

		assertNotNull(result);
		assertTrue(result.isEmpty());
	}

	@Test
	void callbackReturnsEmptyListWhenBindingMetadataCannotBeResolved() {
		ModuleImpl module = new ModuleImpl();
		module.setName("testModule");
		DocumentImpl document = new DocumentImpl();
		document.setName("TestDoc");
		document.setOwningModuleName("testModule");

		ProvidedRepository repository = mock(ProvidedRepository.class);
		when(repository.getDocument(any(), eq(module), eq("TestDoc"))).thenReturn(document);
		ProvidedRepositoryFactory.set(repository);

		CustomerImpl customer = mock(CustomerImpl.class);
		when(customer.getModule("testModule")).thenReturn(module);
		bindPersistence(customer);

		GetSelectItemsAction action = new GetSelectItemsAction("testModule", "TestDoc", "missing", false);
		assertThrows(MetaDataException.class, action::callback);
	}

	private static void bindPersistence(CustomerImpl customer) {
		User user = mock(User.class);
		when(user.getCustomer()).thenReturn(customer);

		AbstractPersistence persistence = mock(AbstractPersistence.class, CALLS_REAL_METHODS);
		persistence.setUser(user);
		persistence.setForThread();
	}

	@SuppressWarnings("unchecked")
	private static void clearThreadPersistence() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		threadLocal.remove();
	}
}
