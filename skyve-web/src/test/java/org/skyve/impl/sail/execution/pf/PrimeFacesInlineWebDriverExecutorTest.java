package org.skyve.impl.sail.execution.pf;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.primefaces.component.autocomplete.AutoComplete;
import org.primefaces.component.selectonemenu.SelectOneMenu;
import org.primefaces.component.selectoneradio.SelectOneRadio;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.web.faces.pipeline.component.ComponentBuilder;
import org.skyve.impl.web.faces.pipeline.layout.LayoutBuilder;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.sail.language.step.context.PopContext;
import org.skyve.metadata.sail.language.step.context.PushEditContext;
import org.skyve.metadata.sail.language.step.context.PushListContext;
import org.skyve.metadata.sail.language.step.interaction.DataEnter;
import org.skyve.metadata.sail.language.step.interaction.DataEnter.Match;
import org.skyve.metadata.sail.language.step.interaction.TabSelect;
import org.skyve.metadata.sail.language.step.interaction.TestDataEnter;
import org.skyve.metadata.sail.language.step.interaction.actions.Action;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridNew;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridSelect;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridZoom;
import org.skyve.metadata.sail.language.step.interaction.lookup.LookupDescriptionAutoComplete;
import org.skyve.metadata.sail.language.step.interaction.lookup.LookupDescriptionPick;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateEdit;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateLink;
import org.skyve.metadata.user.User;

import jakarta.faces.component.UIComponent;
import jakarta.faces.component.html.HtmlForm;

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
	@Test
	void comboAndRadioEntriesByCodeOrLabelAreNotGenerated() {
		TestPrimeFacesInlineWebDriverExecutor executor = new TestPrimeFacesInlineWebDriverExecutor();
		executor.pushContextWith(inForm(new SelectOneMenu()));
		executor.executeDataEnter(dataEnter(Match.code));
		executor.executeDataEnter(dataEnter(Match.label));
		executor.executePopContext(new PopContext());
		executor.pushContextWith(inForm(new SelectOneRadio()));
		executor.executeDataEnter(dataEnter(Match.code));

		String script = executor.toString();
		assertAll(() -> assertTrue(script.contains("<dataEnter binding=\"status\" match=\"code\" /> is not supported by this generator"), script),
					() -> assertTrue(script.contains("<dataEnter binding=\"status\" match=\"label\" /> is not supported by this generator"), script),
					() -> assertFalse(script.contains("selectOne(\"form:status\", 2);"), script),
					() -> assertFalse(script.contains("radio(\"form:status\", 2);"), script));
	}

	@Test
	void comboAndRadioEntriesByIndexAreGenerated() {
		TestPrimeFacesInlineWebDriverExecutor executor = new TestPrimeFacesInlineWebDriverExecutor();
		executor.pushContextWith(inForm(new SelectOneMenu()));
		executor.executeDataEnter(dataEnter(null));
		executor.executePopContext(new PopContext());
		executor.pushContextWith(inForm(new SelectOneRadio()));
		executor.executeDataEnter(dataEnter(Match.index));

		String script = executor.toString();
		assertAll(() -> assertTrue(script.contains("selectOne(\"form:status\", 2);"), script),
					() -> assertTrue(script.contains("radio(\"form:status\", 2);"), script),
					() -> assertFalse(script.contains("is not supported"), script));
	}

	@Test
	void lookupPicksByTextAreNotGenerated() {
		TestPrimeFacesInlineWebDriverExecutor executor = new TestPrimeFacesInlineWebDriverExecutor();
		executor.pushContextWith(inForm(new AutoComplete()));
		LookupDescriptionPick pick = new LookupDescriptionPick();
		pick.setBinding("status");
		pick.setRowContaining("SAIL lookup");
		executor.executeLookupDescriptionPick(pick);
		LookupDescriptionAutoComplete complete = new LookupDescriptionAutoComplete();
		complete.setBinding("status");
		complete.setSearch("SAIL");
		complete.setPick("lookup 2");
		executor.executeLookupDescriptionAutoComplete(complete);

		String script = executor.toString();
		assertAll(() -> assertTrue(script.contains("<LookupDescriptionPick rowContaining=\"SAIL lookup\" /> is not supported by this generator"), script),
					() -> assertTrue(script.contains("<lookupDescriptionAutoComplete binding=\"status\" pick=\"lookup 2\" /> is not supported by this generator"), script),
					() -> assertFalse(script.contains("lookupDescription(\"form:status\""), script));
	}

	@Test
	void linksByTextAreNotGenerated() {
		TestPrimeFacesInlineWebDriverExecutor executor = new TestPrimeFacesInlineWebDriverExecutor();
		executor.pushContextWith(inForm(new AutoComplete()));
		NavigateLink link = new NavigateLink();
		link.setText("Open the request");
		executor.executeNavigateLink(link);

		String script = executor.toString();
		assertTrue(script.contains("<navigateLink text=\"Open the request\" /> is not supported by this generator"), script);
	}

	private static DataEnter dataEnter(Match match) {
		DataEnter result = new DataEnter();
		result.setBinding("status");
		result.setValue("2");
		result.setMatch(match);
		return result;
	}

	private static UIComponent inForm(UIComponent component) {
		HtmlForm form = new HtmlForm();
		form.setId("form");
		component.setId("status");
		form.getChildren().add(component);
		return component;
	}

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

		private void pushContextWith(UIComponent component) {
			PrimeFacesAutomationContext context = new PrimeFacesAutomationContext() {
				@Override
				public List<UIComponent> getFacesComponents(String identifier) {
					return List.of(component);
				}
			};
			context.setModuleName("m");
			context.setDocumentName("d");
			push(context);
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
