package org.skyve.impl.metadata.view.component;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.repository.ProvidedRepository;

@SuppressWarnings("static-method")
class ComponentTest {
	private ProvidedRepository originalRepository;

	@AfterEach
	void tearDown() {
		if (originalRepository != null) {
			setRepository(originalRepository);
			originalRepository = null;
		}
	}

	@Test
	void defaultConstructorCreatesInstance() {
		assertNotNull(new Component());
	}

	@Test
	void moduleNameNullByDefault() {
		assertNull(new Component().getModuleName());
	}

	@Test
	void moduleNameRoundTrip() {
		Component c = new Component();
		c.setModuleName("admin");
		assertEquals("admin", c.getModuleName());
	}

	@Test
	void documentNameNullByDefault() {
		assertNull(new Component().getDocumentName());
	}

	@Test
	void documentNameRoundTrip() {
		Component c = new Component();
		c.setDocumentName("UserDashboard");
		assertEquals("UserDashboard", c.getDocumentName());
	}

	@Test
	void nameNullByDefault() {
		assertNull(new Component().getName());
	}

	@Test
	void nameRoundTrip() {
		Component c = new Component();
		c.setName("editView");
		assertEquals("editView", c.getName());
	}

	@Test
	void widgetIdNullByDefault() {
		assertNull(new Component().getWidgetId());
	}

	@Test
	void widgetIdRoundTrip() {
		Component c = new Component();
		c.setWidgetId("widget123");
		assertEquals("widget123", c.getWidgetId());
	}

	@Test
	void invisibleConditionNameNullByDefault() {
		assertNull(new Component().getInvisibleConditionName());
	}

	@Test
	void invisibleConditionNameRoundTrip() {
		Component c = new Component();
		c.setInvisibleConditionName("hideIt");
		assertEquals("hideIt", c.getInvisibleConditionName());
	}

	@Test
	void invisibleConditionNameBlankBecomesNull() {
		Component c = new Component();
		c.setInvisibleConditionName("  ");
		assertNull(c.getInvisibleConditionName());
	}

	@Test
	void visibleConditionNameNegatesAndSetsInvisible() {
		Component c = new Component();
		c.setVisibleConditionName("showIt");
		assertEquals("notShowIt", c.getInvisibleConditionName());
	}

	@Test
	void visibleConditionNameBlankNegatesToNull() {
		Component c = new Component();
		c.setVisibleConditionName("  ");
		assertNull(c.getInvisibleConditionName());
	}

	@Test
	void propertiesMapIsNotNull() {
		assertNotNull(new Component().getProperties());
	}

	@Test
	void propertiesMapAcceptsEntries() {
		Component c = new Component();
		c.getProperties().put("key", "value");
		assertEquals("value", c.getProperties().get("key"));
	}

	@Test
	void namesListIsNotNull() {
		assertNotNull(new Component().getNames());
	}

	@Test
	void linkAndGetFragmentUseOwningModuleAndDocumentWhenNoOverrides() {
		ProvidedRepository repository = mock(ProvidedRepository.class);
		setRepositoryForTest(repository);

		CustomerImpl customer = mock(CustomerImpl.class);
		ModuleImpl owningModule = mock(ModuleImpl.class);
		when(owningModule.getName()).thenReturn("admin");

		DocumentImpl owningDocument = new DocumentImpl();
		owningDocument.setName("Startup");
		owningDocument.setOwningModuleName("admin");

		ViewImpl originalView = mock(ViewImpl.class);
		ViewImpl fragmentView = mock(ViewImpl.class);
		Component component = new Component();

		when(repository.getView("desktop", customer, owningDocument, "edit")).thenReturn(originalView);
		when(originalView.getFragment(customer, owningModule, owningDocument, "desktop", component)).thenReturn(fragmentView);
		when(customer.getModule("admin")).thenReturn(owningModule);
		when(owningModule.getDocument(customer, "Startup")).thenReturn(owningDocument);

		component.link("desktop", customer, owningModule, owningDocument, "edit");

		ViewImpl result = component.getFragment(customer, "desktop");

		assertEquals(fragmentView, result);
		verify(owningModule, atLeastOnce()).getDocument(customer, "Startup");
	}

	@Test
	void linkUsesExplicitModuleDocumentAndViewNameWhenProvided() {
		ProvidedRepository repository = mock(ProvidedRepository.class);
		setRepositoryForTest(repository);

		CustomerImpl customer = mock(CustomerImpl.class);
		ModuleImpl owningModule = mock(ModuleImpl.class);
		when(owningModule.getName()).thenReturn("admin");

		DocumentImpl owningDocument = new DocumentImpl();
		owningDocument.setName("OwningDoc");
		owningDocument.setOwningModuleName("admin");

		ModuleImpl targetModule = mock(ModuleImpl.class);
		when(targetModule.getName()).thenReturn("test");
		DocumentImpl targetDocument = new DocumentImpl();
		targetDocument.setName("TargetDoc");
		targetDocument.setOwningModuleName("test");
		when(customer.getModule("test")).thenReturn(targetModule);
		when(targetModule.getDocument(customer, "TargetDoc")).thenReturn(targetDocument);

		ViewImpl originalView = mock(ViewImpl.class);
		ViewImpl fragmentView = mock(ViewImpl.class);
		Component component = new Component();
		when(repository.getView("desktop", customer, targetDocument, "customView")).thenReturn(originalView);
		when(originalView.getFragment(customer, targetModule, targetDocument, "desktop", component)).thenReturn(fragmentView);

		component.setModuleName("test");
		component.setDocumentName("TargetDoc");
		component.setName("customView");
		component.link("desktop", customer, owningModule, owningDocument, "edit");

		ViewImpl result = component.getFragment(customer, "desktop");

		assertEquals(fragmentView, result);
		verify(targetModule, atLeastOnce()).getDocument(customer, "TargetDoc");
	}

	@Test
	void linkThrowsWhenTargetViewDoesNotExist() {
		ProvidedRepository repository = mock(ProvidedRepository.class);
		setRepositoryForTest(repository);

		CustomerImpl customer = mock(CustomerImpl.class);
		ModuleImpl owningModule = mock(ModuleImpl.class);
		when(owningModule.getName()).thenReturn("admin");

		DocumentImpl owningDocument = new DocumentImpl();
		owningDocument.setName("Startup");
		owningDocument.setOwningModuleName("admin");

		when(repository.getView("desktop", customer, owningDocument, "edit")).thenReturn(null);

		Component component = new Component();
		assertThrows(MetaDataException.class, () -> component.link("desktop", customer, owningModule, owningDocument, "edit"));
	}

	private void setRepositoryForTest(ProvidedRepository repository) {
		if (originalRepository == null) {
			originalRepository = getRepository();
		}
		setRepository(repository);
	}

	private static ProvidedRepository getRepository() {
		try {
			Field field = ProvidedRepositoryFactory.class.getDeclaredField("repository");
			field.setAccessible(true);
			return (ProvidedRepository) field.get(null);
		}
		catch (ReflectiveOperationException e) {
			throw new RuntimeException(e);
		}
	}

	private static void setRepository(ProvidedRepository repository) {
		try {
			Field field = ProvidedRepositoryFactory.class.getDeclaredField("repository");
			field.setAccessible(true);
			field.set(null, repository);
		}
		catch (ReflectiveOperationException e) {
			throw new RuntimeException(e);
		}
	}
}
