package org.skyve.impl.sail.execution.pf;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.web.faces.pipeline.component.ComponentBuilder;
import org.skyve.impl.web.faces.pipeline.layout.LayoutBuilder;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.sail.language.step.context.PushEditContext;
import org.skyve.metadata.sail.language.step.context.PushListContext;
import org.skyve.metadata.sail.language.step.interaction.DataEnter;
import org.skyve.metadata.sail.language.step.interaction.TabSelect;
import org.skyve.metadata.sail.language.step.interaction.TestDataEnter;
import org.skyve.metadata.sail.language.step.interaction.actions.Action;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridNew;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridSelect;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridZoom;
import org.skyve.metadata.sail.language.step.interaction.lookup.LookupDescriptionPick;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateEdit;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateLink;
import org.skyve.metadata.user.User;

import jakarta.faces.component.UIComponent;

@SuppressWarnings("static-method")
class PrimeFacesInlineWebDriverExecutorTest {
	@AfterEach
	void clearPersistence() {
		setPersistenceThreadLocal(null);
	}

	@Test
	void interactionsWithoutContextThrowMetaDataException() {
		TestPrimeFacesInlineWebDriverExecutor executor = new TestPrimeFacesInlineWebDriverExecutor();
		TabSelect tabSelect = new TabSelect();
		tabSelect.setTabPath("Details");
		DataEnter dataEnter = new DataEnter();
		dataEnter.setBinding("name");
		dataEnter.setValue("Acme");
		Action action = new Action();
		action.setActionName("Custom");
		LookupDescriptionPick pick = new LookupDescriptionPick();
		pick.setBinding("contact");
		pick.setRow(Integer.valueOf(0));
		DataGridNew dataGridNew = new DataGridNew();
		dataGridNew.setBinding("contacts");
		ListGridSelect listGridSelect = new ListGridSelect();
		listGridSelect.setRow(Integer.valueOf(0));

		assertThrows(MetaDataException.class, () -> executor.executeTabSelect(tabSelect));
		assertThrows(MetaDataException.class, () -> executor.executeDataEnter(dataEnter));
		assertThrows(MetaDataException.class, () -> executor.executeAction(action));
		assertThrows(MetaDataException.class, () -> executor.executeLookupDescriptionPick(pick));
		assertThrows(MetaDataException.class, () -> executor.executeDataGridNew(dataGridNew));
		assertThrows(MetaDataException.class, () -> executor.executeListGridSelect(listGridSelect));
		assertThrows(MetaDataException.class, () -> executor.executeTestDataEnter(new TestDataEnter()));
	}

	@Test
	void testDataEnterAfterNavigateLinkThrowsMetaDataException() {
		TestPrimeFacesInlineWebDriverExecutor executor = new TestPrimeFacesInlineWebDriverExecutor();
		NavigateEdit edit = new NavigateEdit();
		edit.setModuleName("sales");
		edit.setDocumentName("Customer");
		executor.executeNavigateEdit(edit);
		executor.executeNavigateLink(new NavigateLink());

		assertThrows(MetaDataException.class, () -> executor.executeTestDataEnter(new TestDataEnter()));
	}

	@Test
	void listGridZoomWithoutQueryDocumentOrModelThrowsMetaDataException() {
		TestPrimeFacesInlineWebDriverExecutor executor = newEditExecutorWithListGrid("sales.Customer");
		ListGridZoom zoom = new ListGridZoom();
		zoom.setRow(Integer.valueOf(0));

		assertThrows(MetaDataException.class, () -> executor.executeListGridZoom(zoom));
	}

	@Test
	void listGridZoomOnDocumentWithoutModuleRefPushesOwningModule() {
		Module module = mock(Module.class);
		DocumentRef ref = new DocumentRef();
		ref.setOwningModuleName("sales");
		when(module.getDocumentRefs()).thenReturn(Map.of("Contact", ref));
		bindModule(module);
		TestPrimeFacesInlineWebDriverExecutor executor = newEditExecutorWithListGrid("sales.Contact");
		ListGridZoom zoom = new ListGridZoom();
		zoom.setDocumentName("Contact");
		zoom.setRow(Integer.valueOf(0));

		executor.executeListGridZoom(zoom);

		PrimeFacesAutomationContext context = executor.current();
		assertEquals("sales", context.getModuleName());
		assertEquals("Contact", context.getDocumentName());
	}

	/**
	 * Creates an executor in an edit view of sales.Customer with a list grid and its zoom button collected.
	 */
	private static TestPrimeFacesInlineWebDriverExecutor newEditExecutorWithListGrid(String listGridIdentifier) {
		TestPrimeFacesInlineWebDriverExecutor executor = new TestPrimeFacesInlineWebDriverExecutor();
		NavigateEdit edit = new NavigateEdit();
		edit.setModuleName("sales");
		edit.setDocumentName("Customer");
		executor.executeNavigateEdit(edit);
		PrimeFacesAutomationContext context = executor.current();
		context.put(listGridIdentifier, mock(UIComponent.class), listGridIdentifier);
		context.put(listGridIdentifier + ".zoom", mock(UIComponent.class), listGridIdentifier);
		return executor;
	}

	/**
	 * Binds a mocked persistence to the thread whose user's customer has the given "sales" module.
	 */
	private static void bindModule(Module module) {
		Customer customer = mock(Customer.class);
		when(customer.getModule("sales")).thenReturn(module);
		User user = mock(User.class);
		when(user.getCustomer()).thenReturn(customer);
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		when(persistence.getUser()).thenReturn(user);
		setPersistenceThreadLocal(persistence);
	}

	@SuppressWarnings("unchecked")
	private static void setPersistenceThreadLocal(AbstractPersistence persistence) {
		try {
			Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
			field.setAccessible(true);
			ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
			if (persistence == null) {
				threadLocal.remove();
			}
			else {
				threadLocal.set(persistence);
			}
		}
		catch (ReflectiveOperationException e) {
			throw new IllegalStateException(e);
		}
	}

	private static final class TestPrimeFacesInlineWebDriverExecutor extends PrimeFacesInlineWebDriverExecutor {
		private TestPrimeFacesInlineWebDriverExecutor() {
			super(mock(ComponentBuilder.class), mock(LayoutBuilder.class));
		}

		private PrimeFacesAutomationContext current() {
			return peek();
		}

		@Override
		public void executePushListContext(PushListContext push) {
			PrimeFacesAutomationContext context = new PrimeFacesAutomationContext();
			context.setModuleName(push.getModuleName());
			context.setDocumentName(push.getDocumentName());
			push(context);
		}

		@Override
		public void executePushEditContext(PushEditContext push) {
			PrimeFacesAutomationContext context = new PrimeFacesAutomationContext();
			context.setModuleName(push.getModuleName());
			context.setDocumentName(push.getDocumentName());
			push(context);
		}
	}
}
