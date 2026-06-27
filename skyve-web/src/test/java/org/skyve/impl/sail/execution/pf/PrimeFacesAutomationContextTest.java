package org.skyve.impl.sail.execution.pf;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.primefaces.component.commandbutton.CommandButton;
import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.sail.mock.MockFacesContext;
import org.skyve.impl.web.faces.pipeline.component.ComponentBuilder.EventSourceComponent;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.sail.language.Step;
import org.skyve.metadata.sail.language.step.context.PushEditContext;

import jakarta.faces.component.NamingContainer;
import jakarta.faces.component.UIComponent;
import jakarta.faces.component.UIComponentBase;
import jakarta.faces.context.FacesContext;

@SuppressWarnings("static-method")
class PrimeFacesAutomationContextTest {
	private abstract static class FacesContextBridge extends FacesContext {
		static void setCurrent(FacesContext context) {
			setCurrentInstance(context);
		}
	}

	private static class TestComponent extends UIComponentBase {
		private final String family;

		private TestComponent(String family) {
			this.family = family;
		}

		@Override
		public String getFamily() {
			return family;
		}
	}

	private static final class TestNamingContainer extends TestComponent implements NamingContainer {
		private TestNamingContainer(String family) {
			super(family);
		}
	}

	@AfterEach
	void clearFacesContext() {
		FacesContextBridge.setCurrent(null);
	}

	@Test
	void contextStoresComponentsAndWidgetsByIdentifier() {
		PrimeFacesAutomationContext context = new PrimeFacesAutomationContext();
		UIComponent first = new TestComponent("first");
		UIComponent second = new TestComponent("second");
		Object firstWidget = new Object();
		Object secondWidget = new Object();

		context.put("customer", first, firstWidget);
		context.put("customer", second, secondWidget);

		assertEquals(List.of(first, second), context.getFacesComponents("customer"));
		assertEquals(List.of(firstWidget, secondWidget), context.getSkyveWidgets("customer"));
		assertNull(context.getFacesComponents("missing"));
		assertNull(context.getSkyveWidgets("missing"));
	}

	@Test
	void clientIdIncludesNamingContainerAncestorsAndOptionalRow() {
		UIComponent form = new TestNamingContainer("form");
		form.setId("form");
		UIComponent panel = new TestComponent("panel");
		panel.setId("panel");
		UIComponent input = new TestComponent("input");
		input.setId("input");
		form.getChildren().add(panel);
		panel.getChildren().add(input);

		assertEquals("form:input", PrimeFacesAutomationContext.clientId(input));
		assertEquals("form:3:input", PrimeFacesAutomationContext.clientId(input, Integer.valueOf(3)));
	}

	@Test
	void collectingBuilderRegistersActionsAndBoundComponents() {
		FacesContextBridge.setCurrent(new MockFacesContext());
		PrimeFacesAutomationContext context = new PrimeFacesAutomationContext();
		ComponentCollectingComponentBuilder builder = new ComponentCollectingComponentBuilder(context, mock(Step.class));
		UIComponent actionComponent = new TestComponent("action");
		UIComponent implicitComponent = new TestComponent("implicit");
		UIComponent checkBoxComponent = new TestComponent("checkBox");
		ActionImpl customAction = new ActionImpl();
		customAction.setName("customAction");
		ActionImpl implicitAction = new ActionImpl();
		CheckBox checkBox = new CheckBox();
		checkBox.setBinding("active");
		EventSourceComponent checkBoxWrapper = new EventSourceComponent(checkBoxComponent, (UIComponentBase) checkBoxComponent);

		assertSame(actionComponent, builder.action(actionComponent,
													null,
													null,
													"Custom",
													null,
													null,
													null,
													null,
													customAction));
		assertSame(implicitComponent, builder.action(implicitComponent,
														null,
														null,
														"Save",
														null,
														null,
														null,
														ImplicitActionName.Save,
														implicitAction));
		assertSame(checkBoxWrapper, builder.checkBox(checkBoxWrapper, null, checkBox, null, null, null));

		assertEquals(List.of(actionComponent), context.getFacesComponents("customAction"));
		assertEquals(List.of(customAction), context.getSkyveWidgets("customAction"));
		assertEquals(List.of(implicitComponent), context.getFacesComponents("Save"));
		assertEquals(List.of(ImplicitActionName.Save), context.getSkyveWidgets("Save"));
		assertEquals(List.of(checkBoxComponent), context.getFacesComponents("active"));
		assertEquals(List.of(checkBox), context.getSkyveWidgets("active"));
	}

	@Test
	void collectingLayoutBuilderRegistersDeferredEditListGridWhenAddedToContainer() {
		FacesContextBridge.setCurrent(new MockFacesContext());
		PrimeFacesAutomationContext context = new PrimeFacesAutomationContext();
		PushEditContext step = mock(PushEditContext.class);
		when(step.getModuleName()).thenReturn("sales");
		ComponentCollectingComponentBuilder componentBuilder = new ComponentCollectingComponentBuilder(context, step);
		ComponentCollectingLayoutBuilder layoutBuilder = new ComponentCollectingLayoutBuilder(componentBuilder);
		UIComponent generatedListGrid = new TestComponent("generatedListGrid");
		UIComponent addedListGrid = new TestComponent("addedListGrid");
		org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid listGrid = new org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid();

		componentBuilder.listGrid(generatedListGrid, "sales", "Customer", "customers", "desktop", null, null, listGrid, false);

		assertSame(generatedListGrid, componentBuilder.listGrid(generatedListGrid,
																"sales",
																"Customer",
																"customers",
																"desktop",
																null,
																null,
																listGrid,
																false));
		assertNull(context.getFacesComponents("sales.customers"));
		assertNull(layoutBuilder.addToContainer(null, null, null, addedListGrid, null, null, null, null, null, null, null, null));

		assertEquals(List.of(addedListGrid), context.getFacesComponents("sales.customers"));
		assertEquals(List.of("sales.customers"), context.getSkyveWidgets("sales.customers"));
	}

	@Test
	void collectingBuilderRegistersDataGridActionColumnControls() {
		FacesContextBridge.setCurrent(new MockFacesContext());
		PrimeFacesAutomationContext context = new PrimeFacesAutomationContext();
		ComponentCollectingComponentBuilder builder = new ComponentCollectingComponentBuilder(context, mock(Step.class));
		UIComponent dataGridComponent = new TestComponent("dataGrid");
		UIComponent selectColumn = new TestComponent("select");
		UIComponent actionColumn = new TestComponent("actions");
		CommandButton newButton = new CommandButton();
		CommandButton zoomButton = new CommandButton();
		UIComponent spacer = new TestComponent("spacer");
		CommandButton removeButton = new CommandButton();
		DataGrid grid = new DataGrid();
		grid.setBinding("contacts");
		actionColumn.getFacets().put("header", newButton);
		actionColumn.getChildren().add(zoomButton);
		actionColumn.getChildren().add(spacer);
		actionColumn.getChildren().add(removeButton);
		dataGridComponent.getChildren().add(selectColumn);
		dataGridComponent.getChildren().add(actionColumn);

		assertSame(dataGridComponent, builder.addDataGridActionColumn(dataGridComponent,
																		dataGridComponent,
																		grid,
																		"contactRow",
																		"",
																		"Contact",
																		false,
																		true,
																		true));

		assertEquals(List.of(dataGridComponent), context.getFacesComponents("contacts"));
		assertEquals(List.of(newButton), context.getFacesComponents("contacts.new"));
		assertEquals(List.of(zoomButton), context.getFacesComponents("contacts.zoom"));
		assertEquals(List.of(removeButton), context.getFacesComponents("contacts.remove"));
		assertEquals(List.of(selectColumn), context.getFacesComponents("contacts.select"));
	}
}
