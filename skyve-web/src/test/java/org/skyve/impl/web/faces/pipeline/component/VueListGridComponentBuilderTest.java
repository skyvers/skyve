package org.skyve.impl.web.faces.pipeline.component;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.primefaces.component.remotecommand.RemoteCommand;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.web.faces.components.VueListGridScript;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.model.list.ListModel;

import jakarta.el.ELContext;
import jakarta.el.ExpressionFactory;
import jakarta.el.MethodExpression;
import jakarta.faces.application.Application;
import jakarta.faces.component.UIComponent;
import jakarta.faces.component.UIOutput;
import jakarta.faces.component.html.HtmlPanelGroup;
import jakarta.faces.context.FacesContext;

@SuppressWarnings({"static-method", "boxing"})
class VueListGridComponentBuilderTest {
	private abstract static class FacesContextBridge extends FacesContext {
		static void setCurrent(FacesContext facesContext) {
			setCurrentInstance(facesContext);
		}
	}

	private Application application;
	private ExpressionFactory expressionFactory;

	@BeforeEach
	void setUpFacesContext() {
		FacesContext facesContext = mock(FacesContext.class);
		application = mock(Application.class);
		expressionFactory = mock(ExpressionFactory.class);
		when(facesContext.getApplication()).thenReturn(application);
		when(facesContext.getELContext()).thenReturn(mock(ELContext.class));
		when(application.getExpressionFactory()).thenReturn(expressionFactory);
		FacesContextBridge.setCurrent(facesContext);
	}

	@AfterEach
	void tearDown() {
		FacesContextBridge.setCurrent(null);
		clearThreadPersistence();
	}

	@Test
	void listGridReturnsExistingComponentUnchanged() {
		VueListGridComponentBuilder builder = new VueListGridComponentBuilder();
		UIComponent existing = new HtmlPanelGroup();

		assertSame(existing, builder.listGrid(existing, "sales", "Order", "recent", "desktop", null, null, null, false));
	}

	@Test
	void listGridCreatesContainerAndVueScriptWithDerivedState() {
		User user = mock(User.class);
		Document drivingDocument = mock(Document.class);
		ListModel<Bean> model = mock(ListModel.class);
		FacesView facesView = mock(FacesView.class);
		ListGrid grid = new ListGrid();
		grid.setShowAdd(Boolean.TRUE);
		grid.setShowZoom(Boolean.FALSE);
		grid.setShowFilter(Boolean.FALSE);
		grid.setShowSummary(Boolean.TRUE);
		grid.setShowSnap(Boolean.FALSE);
		bindPersistenceForUser(user);
		when(user.canCreateDocument(drivingDocument)).thenReturn(Boolean.TRUE);
		when(drivingDocument.getOwningModuleName()).thenReturn("crm");
		when(drivingDocument.getName()).thenReturn("Contact");
		when(model.getDrivingDocument()).thenReturn(drivingDocument);
		when(facesView.nextId()).thenReturn("lg1");
		HtmlPanelGroup container = new HtmlPanelGroup();
		when(application.createComponent(HtmlPanelGroup.COMPONENT_TYPE)).thenReturn(container);

		VueListGridComponentBuilder builder = new VueListGridComponentBuilder();
		builder.setSAILManagedBean(facesView);

		HtmlPanelGroup result = (HtmlPanelGroup) builder.listGrid(null, "sales", "Order", "recent", "desktop", model, null, grid, false);

		assertEquals("block", result.getLayout());
		assertEquals("lg1", result.getId());
		assertEquals(1, result.getChildCount());
		VueListGridScript script = (VueListGridScript) result.getChildren().get(0);
		assertEquals("lg1", script.getAttributes().get("containerId"));
		assertEquals("sales", script.getAttributes().get("owningModuleName"));
		assertEquals("Order", script.getAttributes().get("owningDocumentName"));
		assertEquals("crm", script.getAttributes().get("drivingModuleName"));
		assertEquals("Contact", script.getAttributes().get("drivingDocumentName"));
		assertEquals("recent", script.getAttributes().get("modelName"));
		assertTrue((Boolean) script.getAttributes().get("showAdd"));
		assertFalse((Boolean) script.getAttributes().get("showZoom"));
		assertFalse((Boolean) script.getAttributes().get("showFilter"));
		assertTrue((Boolean) script.getAttributes().get("showSummary"));
		assertFalse((Boolean) script.getAttributes().get("showSnap"));
	}

	@Test
	void listGridAggregateQueryUsesDrivingDocumentAndSuppressesInteractiveFeatures() {
		User user = mock(User.class);
		Document drivingDocument = mock(Document.class);
		ListModel<Bean> model = mock(ListModel.class);
		FacesView facesView = mock(FacesView.class);
		ListGrid grid = new ListGrid();
		grid.setQueryName("contacts");
		bindPersistenceForUser(user);
		when(user.canCreateDocument(drivingDocument)).thenReturn(Boolean.TRUE);
		when(drivingDocument.getOwningModuleName()).thenReturn("crm");
		when(drivingDocument.getName()).thenReturn("Contact");
		when(model.getDrivingDocument()).thenReturn(drivingDocument);
		when(facesView.nextId()).thenReturn("lg2");
		HtmlPanelGroup container = new HtmlPanelGroup();
		when(application.createComponent(HtmlPanelGroup.COMPONENT_TYPE)).thenReturn(container);

		VueListGridComponentBuilder builder = new VueListGridComponentBuilder();
		builder.setSAILManagedBean(facesView);

		HtmlPanelGroup result = (HtmlPanelGroup) builder.listGrid(null, "sales", null, "ignoredModel", "desktop", model, null, grid, true);

		VueListGridScript script = (VueListGridScript) result.getChildren().get(0);
		assertEquals("Contact", script.getAttributes().get("owningDocumentName"));
		assertEquals("contacts", script.getAttributes().get("queryName"));
		assertFalse(script.getAttributes().containsKey("modelName"));
		assertFalse((Boolean) script.getAttributes().get("showAdd"));
		assertFalse((Boolean) script.getAttributes().get("showZoom"));
		assertFalse((Boolean) script.getAttributes().get("showFilter"));
		assertFalse((Boolean) script.getAttributes().get("showSummary"));
		assertTrue((Boolean) script.getAttributes().get("showSnap"));
	}

	@Test
	void listGridWithSelectedBindingAddsRemoteCommandBeforeVueScript() {
		User user = mock(User.class);
		Document drivingDocument = mock(Document.class);
		ListModel<Bean> model = mock(ListModel.class);
		FacesView facesView = mock(FacesView.class);
		MethodExpression selectedExpression = mock(MethodExpression.class);
		ListGrid grid = new ListGrid();
		grid.setSelectedIdBinding("selectedContactId");
		bindPersistenceForUser(user);
		when(user.canCreateDocument(drivingDocument)).thenReturn(Boolean.FALSE);
		when(drivingDocument.getOwningModuleName()).thenReturn("crm");
		when(drivingDocument.getName()).thenReturn("Contact");
		when(model.getDrivingDocument()).thenReturn(drivingDocument);
		when(facesView.nextId()).thenReturn("lg3");
		when(expressionFactory.createMethodExpression(any(ELContext.class), anyString(), any(), any())).thenReturn(selectedExpression);
		HtmlPanelGroup container = new HtmlPanelGroup();
		RemoteCommand selectedCommand = new RemoteCommand();
		when(application.createComponent(HtmlPanelGroup.COMPONENT_TYPE)).thenReturn(container);
		when(application.createComponent(RemoteCommand.COMPONENT_TYPE)).thenReturn(selectedCommand);

		VueListGridComponentBuilder builder = new VueListGridComponentBuilder();
		builder.setSAILManagedBean(facesView);

		HtmlPanelGroup result = (HtmlPanelGroup) builder.listGrid(null, "sales", "Order", "recent", "desktop", model, null, grid, false);

		assertEquals(2, result.getChildCount());
		RemoteCommand command = (RemoteCommand) result.getChildren().get(0);
		assertEquals("lg3_selected", command.getName());
		assertEquals("@this", command.getProcess());
		assertEquals("@none", command.getUpdate());
		assertSame(selectedExpression, command.getActionExpression());
		VueListGridScript script = (VueListGridScript) result.getChildren().get(1);
		assertEquals("lg3_selected", script.getAttributes().get("selectedRemoteCommand"));
		assertFalse((Boolean) script.getAttributes().get("showAdd"));
	}

	@Test
	void listGridContextMenuReturnsHiddenEmptyOutput() {
		VueListGridComponentBuilder builder = new VueListGridComponentBuilder();

		UIOutput output = (UIOutput) builder.listGridContextMenu(null, "gridId", new ListGrid());

		assertEquals("", output.getValue());
		assertFalse(output.isRendered());
	}

	private static void bindPersistenceForUser(User user) {
		AbstractPersistence persistence = mock(AbstractPersistence.class, CALLS_REAL_METHODS);
		persistence.setUser(user);
		persistence.setForThread();
	}

	@SuppressWarnings("unchecked")
	private static void clearThreadPersistence() {
		try {
			Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
			field.setAccessible(true);
			ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
			threadLocal.remove();
		}
		catch (ReflectiveOperationException e) {
			throw new RuntimeException("Unable to clear thread-local persistence", e);
		}
	}
}
