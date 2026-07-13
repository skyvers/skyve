package org.skyve.impl.web.faces.pipeline.component;

import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.container.Collapsible;
import org.skyve.impl.metadata.view.container.Sidebar;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.widget.Blurb;
import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.impl.metadata.view.widget.Chart;
import org.skyve.impl.metadata.view.widget.DynamicImage;
import org.skyve.impl.metadata.view.widget.Link;
import org.skyve.impl.metadata.view.widget.MapDisplay;
import org.skyve.impl.metadata.view.widget.Spacer;
import org.skyve.impl.metadata.view.widget.StaticImage;
import org.skyve.impl.metadata.view.widget.bound.Label;
import org.skyve.impl.metadata.view.widget.bound.ZoomIn;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
import org.skyve.impl.metadata.view.widget.bound.input.ContentSignature;
import org.skyve.impl.metadata.view.widget.bound.input.Geometry;
import org.skyve.impl.metadata.view.widget.bound.input.GeometryMap;
import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.impl.metadata.view.widget.bound.input.ListMembership;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.Password;
import org.skyve.impl.metadata.view.widget.bound.input.Radio;
import org.skyve.impl.metadata.view.widget.bound.input.RichText;
import org.skyve.impl.metadata.view.widget.bound.input.Slider;
import org.skyve.impl.metadata.view.widget.bound.input.Spinner;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.metadata.view.widget.bound.tabular.AbstractDataWidget;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridContainerColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.impl.web.faces.pipeline.component.ComponentBuilder.EventSourceComponent;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.query.QueryDefinition;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.web.UserAgentType;

import jakarta.el.ELContext;
import jakarta.el.ExpressionFactory;
import jakarta.faces.application.Application;
import jakarta.faces.component.UIComponent;
import jakarta.faces.component.UIComponentBase;
import jakarta.faces.context.FacesContext;

/**
 * Tests for ComponentBuilderChain delegation and state propagation.
 */
@SuppressWarnings({"static-method", "boxing"})
class ComponentBuilderChainTest {

	private abstract static class FacesContextBridge extends FacesContext {
		static void setCurrent(FacesContext facesContext) {
			setCurrentInstance(facesContext);
		}
	}

	@BeforeAll
	static void setUpFacesContext() {
		FacesContext facesContext = mock(FacesContext.class);
		Application mockApplication = mock(Application.class);
		ExpressionFactory mockExpressionFactory = mock(ExpressionFactory.class);
		ELContext elContext = mock(ELContext.class);
		when(facesContext.getApplication()).thenReturn(mockApplication);
		when(facesContext.getELContext()).thenReturn(elContext);
		when(mockApplication.getExpressionFactory()).thenReturn(mockExpressionFactory);
		FacesContextBridge.setCurrent(facesContext);
	}

	@AfterAll
	static void tearDownFacesContext() {
		FacesContextBridge.setCurrent(null);
	}

	private ComponentBuilder mockBuilder;
	private ComponentBuilderChain chain;
	private UIComponent mockComponent;
	private EventSourceComponent mockEventSource;

	@BeforeEach
	void setUp() {
		mockBuilder = mock(ComponentBuilder.class);
		chain = new ComponentBuilderChain(mockBuilder);
		mockComponent = mock(UIComponent.class);
		mockEventSource = new EventSourceComponent(mockComponent, mock(UIComponentBase.class));
	}

	// ===== Empty chain =====

	@Test
	void emptyChainViewReturnsInputComponent() {
		ComponentBuilderChain emptyChain = new ComponentBuilderChain();
		assertSame(mockComponent, emptyChain.view(mockComponent, false));
	}

	@Test
	void emptyChainToolbarsReturnsInputList() {
		ComponentBuilderChain emptyChain = new ComponentBuilderChain();
		List<UIComponent> list = new ArrayList<>();
		assertSame(list, emptyChain.toolbars(list, "id"));
	}

	// ===== State propagation =====

	@Test
	void setSAILManagedBeanPropagatestoBuilders() {
		FacesView view = mock(FacesView.class);
		chain.setSAILManagedBean(view);
		verify(mockBuilder).setSAILManagedBean(view);
	}

	@Test
	void setProcessPropagatestoBuilders() {
		chain.setProcess("@form");
		verify(mockBuilder).setProcess("@form");
	}

	@Test
	void setUpdatePropagatestoBuilders() {
		chain.setUpdate("@(form)");
		verify(mockBuilder).setUpdate("@(form)");
	}

	@Test
	void setUserAgentTypePropagatestoBuilders() {
		chain.setUserAgentType(UserAgentType.desktop);
		verify(mockBuilder).setUserAgentType(UserAgentType.desktop);
	}

	// ===== Delegation methods =====

	@Test
	void viewDelegatesToBuilder() {
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.view(mockComponent, true)).thenReturn(result);
		assertSame(result, chain.view(mockComponent, true));
	}

	@Test
	void toolbarsDelegatesToBuilder() {
		List<UIComponent> list = new ArrayList<>();
		List<UIComponent> result = new ArrayList<>();
		when(mockBuilder.toolbars(list, "id")).thenReturn(result);
		assertSame(result, chain.toolbars(list, "id"));
	}

	@Test
	void tabPaneDelegatesToBuilder() {
		TabPane tabPane = new TabPane();
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.tabPane(mockComponent, tabPane, "mod", "doc")).thenReturn(result);
		assertSame(result, chain.tabPane(mockComponent, tabPane, "mod", "doc"));
	}

	@Test
	void tabDelegatesToBuilder() {
		Tab tab = new Tab();
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.tab(mockComponent, "title", tab)).thenReturn(result);
		assertSame(result, chain.tab(mockComponent, "title", tab));
	}

	@Test
	void tabPaneScriptDelegatesToBuilder() {
		TabPane tabPane = new TabPane();
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.tabPaneScript(mockComponent, tabPane, "mod", "doc", "compId")).thenReturn(result);
		assertSame(result, chain.tabPaneScript(mockComponent, tabPane, "mod", "doc", "compId"));
	}

	@Test
	void sidebarScriptDelegatesToBuilder() {
		Sidebar sidebar = mock(Sidebar.class);
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.sidebarScript(mockComponent, sidebar, false, "compId")).thenReturn(result);
		assertSame(result, chain.sidebarScript(mockComponent, sidebar, false, "compId"));
	}

	@Test
	void borderDelegatesToBuilder() {
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.border(mockComponent, "title", "invisible", 100, Collapsible.open)).thenReturn(result);
		assertSame(result, chain.border(mockComponent, "title", "invisible", 100, Collapsible.open));
	}

	@Test
	void labelValueDelegatesToBuilder() {
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.label(mockComponent, "value")).thenReturn(result);
		assertSame(result, chain.label(mockComponent, "value"));
	}

	@Test
	void spacerDelegatesToBuilder() {
		Spacer spacer = mock(Spacer.class);
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.spacer(mockComponent, spacer)).thenReturn(result);
		assertSame(result, chain.spacer(mockComponent, spacer));
	}

	@Test
	void zoomInDelegatesToBuilder() {
		ZoomIn zoomIn = mock(ZoomIn.class);
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.zoomIn(mockComponent, "label", "icon", "tip", zoomIn, "disabled")).thenReturn(result);
		assertSame(result, chain.zoomIn(mockComponent, "label", "icon", "tip", zoomIn, "disabled"));
	}

	@Test
	void actionButtonDelegatesToBuilder() {
		Button button = mock(Button.class);
		Action action = mock(Action.class);
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.actionButton(mockComponent, "binding", "var", "label", "icon", "tip", "confirm", button, "disabled", action)).thenReturn(result);
		assertSame(result, chain.actionButton(mockComponent, "binding", "var", "label", "icon", "tip", "confirm", button, "disabled", action));
	}

	@Test
	void reportButtonDelegatesToBuilder() {
		Button button = mock(Button.class);
		Action action = mock(Action.class);
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.reportButton(mockComponent, "label", "icon", "tip", "confirm", button, "disabled", action)).thenReturn(result);
		assertSame(result, chain.reportButton(mockComponent, "label", "icon", "tip", "confirm", button, "disabled", action));
	}

	@Test
	void downloadButtonDelegatesToBuilder() {
		Button button = mock(Button.class);
		Action action = mock(Action.class);
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.downloadButton(mockComponent, "binding", "var", "label", "icon", "tip", "confirm", button, "disabled", action)).thenReturn(result);
		assertSame(result, chain.downloadButton(mockComponent, "binding", "var", "label", "icon", "tip", "confirm", button, "disabled", action));
	}

	@Test
	void uploadButtonDelegatesToBuilder() {
		Button button = mock(Button.class);
		Action action = mock(Action.class);
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.uploadButton(mockComponent, "label", "icon", "tip", "confirm", button, "disabled", action)).thenReturn(result);
		assertSame(result, chain.uploadButton(mockComponent, "label", "icon", "tip", "confirm", button, "disabled", action));
	}

	@Test
	void staticImageDelegatesToBuilder() {
		StaticImage image = mock(StaticImage.class);
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.staticImage(mockComponent, "url", image)).thenReturn(result);
		assertSame(result, chain.staticImage(mockComponent, "url", image));
	}

	@Test
	void dynamicImageDelegatesToBuilder() {
		DynamicImage image = mock(DynamicImage.class);
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.dynamicImage(mockComponent, image, "mod", "doc")).thenReturn(result);
		assertSame(result, chain.dynamicImage(mockComponent, image, "mod", "doc"));
	}

	@Test
	void blurbDelegatesToBuilder() {
		Blurb blurb = mock(Blurb.class);
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.blurb(mockComponent, "var", "value", "binding", blurb)).thenReturn(result);
		assertSame(result, chain.blurb(mockComponent, "var", "value", "binding", blurb));
	}

	@Test
	void labelBindingDelegatesToBuilder() {
		Label label = mock(Label.class);
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.label(mockComponent, "var", "value", "binding", label)).thenReturn(result);
		assertSame(result, chain.label(mockComponent, "var", "value", "binding", label));
	}

	@Test
	void dataGridDelegatesToBuilder() {
		DataGrid grid = mock(DataGrid.class);
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.dataGrid(mockComponent, "var", true, grid)).thenReturn(result);
		assertSame(result, chain.dataGrid(mockComponent, "var", true, grid));
	}

	@Test
	void dataRepeaterDelegatesToBuilder() {
		DataRepeater repeater = mock(DataRepeater.class);
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.dataRepeater(mockComponent, "var", repeater)).thenReturn(result);
		assertSame(result, chain.dataRepeater(mockComponent, "var", repeater));
	}

	@Test
	void addDataGridBoundColumnDelegatesToBuilder() {
		UIComponent current = mock(UIComponent.class);
		AbstractDataWidget widget = mock(AbstractDataWidget.class);
		DataGridBoundColumn column = mock(DataGridBoundColumn.class);
		StringBuilder sb = new StringBuilder();
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.addDataGridBoundColumn(mockComponent, current, widget, column, "var", "title", "binding", sb, HorizontalAlignment.left, 100)).thenReturn(result);
		assertSame(result, chain.addDataGridBoundColumn(mockComponent, current, widget, column, "var", "title", "binding", sb, HorizontalAlignment.left, 100));
	}

	@Test
	void addedDataGridBoundColumnDelegatesToBuilder() {
		UIComponent current = mock(UIComponent.class);
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.addedDataGridBoundColumn(mockComponent, current, HorizontalAlignment.centre)).thenReturn(result);
		assertSame(result, chain.addedDataGridBoundColumn(mockComponent, current, HorizontalAlignment.centre));
	}

	@Test
	void addDataGridContainerColumnDelegatesToBuilder() {
		UIComponent current = mock(UIComponent.class);
		AbstractDataWidget widget = mock(AbstractDataWidget.class);
		DataGridContainerColumn column = mock(DataGridContainerColumn.class);
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.addDataGridContainerColumn(mockComponent, current, widget, "title", column, HorizontalAlignment.right)).thenReturn(result);
		assertSame(result, chain.addDataGridContainerColumn(mockComponent, current, widget, "title", column, HorizontalAlignment.right));
	}

	@Test
	void addedDataGridContainerColumnDelegatesToBuilder() {
		UIComponent current = mock(UIComponent.class);
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.addedDataGridContainerColumn(mockComponent, current)).thenReturn(result);
		assertSame(result, chain.addedDataGridContainerColumn(mockComponent, current));
	}

	@Test
	void addDataGridActionColumnDelegatesToBuilder() {
		UIComponent current = mock(UIComponent.class);
		DataGrid grid = mock(DataGrid.class);
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.addDataGridActionColumn(mockComponent, current, grid, "var", "expr", "alias", false, true, false)).thenReturn(result);
		assertSame(result, chain.addDataGridActionColumn(mockComponent, current, grid, "var", "expr", "alias", false, true, false));
	}

	@Test
	void mapQueryDelegatesToBuilder() {
		MapDisplay map = mock(MapDisplay.class);
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.map(mockComponent, map, "mod", "query", "binding")).thenReturn(result);
		assertSame(result, chain.map(mockComponent, map, "mod", "query", "binding"));
	}

	@Test
	void mapModelDelegatesToBuilder() {
		MapDisplay map = mock(MapDisplay.class);
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.map(mockComponent, map, "modelName")).thenReturn(result);
		assertSame(result, chain.map(mockComponent, map, "modelName"));
	}

	@Test
	void geometryDelegatesToBuilder() {
		Geometry geometry = mock(Geometry.class);
		EventSourceComponent result = new EventSourceComponent(mock(UIComponent.class), mock(UIComponentBase.class));
		when(mockBuilder.geometry(mockEventSource, "var", geometry, "disabled", "title", null, HorizontalAlignment.left)).thenReturn(result);
		assertSame(result, chain.geometry(mockEventSource, "var", geometry, "disabled", "title", null, HorizontalAlignment.left));
	}

	@Test
	void geometryMapDelegatesToBuilder() {
		GeometryMap geometry = mock(GeometryMap.class);
		EventSourceComponent result = new EventSourceComponent(mock(UIComponent.class), mock(UIComponentBase.class));
		when(mockBuilder.geometryMap(mockEventSource, geometry, "disabled", "title", null)).thenReturn(result);
		assertSame(result, chain.geometryMap(mockEventSource, geometry, "disabled", "title", null));
	}

	@Test
	void chartDelegatesToBuilder() {
		Chart chart = mock(Chart.class);
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.chart(mockComponent, chart)).thenReturn(result);
		assertSame(result, chain.chart(mockComponent, chart));
	}

	@Test
	void listGridDelegatesToBuilder() {
		ListModel<Bean> model = mock(ListModel.class);
		Document doc = mock(Document.class);
		ListGrid listGrid = mock(ListGrid.class);
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.listGrid(mockComponent, "mod", "modelDoc", "model", "uxui", model, doc, listGrid, "#header", false)).thenReturn(result);
		assertSame(result, chain.listGrid(mockComponent, "mod", "modelDoc", "model", "uxui", model, doc, listGrid, "#header", false));
	}

	@Test
	void listGridContextMenuDelegatesToBuilder() {
		ListGrid listGrid = mock(ListGrid.class);
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.listGridContextMenu(mockComponent, "gridId", listGrid)).thenReturn(result);
		assertSame(result, chain.listGridContextMenu(mockComponent, "gridId", listGrid));
	}

	@Test
	void listRepeaterDelegatesToBuilder() {
		ListModel<Bean> model = mock(ListModel.class);
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.listRepeater(mockComponent, "modelDoc", "model", "uxui", model, null, null, true, false)).thenReturn(result);
		assertSame(result, chain.listRepeater(mockComponent, "modelDoc", "model", "uxui", model, null, null, true, false));
	}

	@Test
	void listMembershipDelegatesToBuilder() {
		ListMembership membership = mock(ListMembership.class);
		EscapableText candidates = EscapableText.of("candidates", true);
		EscapableText members = EscapableText.of("members", false);
		EventSourceComponent result = new EventSourceComponent(mock(UIComponent.class), mock(UIComponentBase.class));
		when(mockBuilder.listMembership(mockEventSource, candidates, members, membership)).thenReturn(result);
		assertSame(result, chain.listMembership(mockEventSource, candidates, members, membership));
	}

	@Test
	void checkBoxDelegatesToBuilder() {
		CheckBox checkBox = mock(CheckBox.class);
		EventSourceComponent result = new EventSourceComponent(mock(UIComponent.class), mock(UIComponentBase.class));
		when(mockBuilder.checkBox(mockEventSource, "var", checkBox, "disabled", "title", null)).thenReturn(result);
		assertSame(result, chain.checkBox(mockEventSource, "var", checkBox, "disabled", "title", null));
	}

	@Test
	void colourPickerDelegatesToBuilder() {
		ColourPicker colour = new ColourPicker();
		EventSourceComponent result = new EventSourceComponent(mock(UIComponent.class), mock(UIComponentBase.class));
		when(mockBuilder.colourPicker(mockEventSource, "var", colour, "disabled", "title", null, HorizontalAlignment.left)).thenReturn(result);
		assertSame(result, chain.colourPicker(mockEventSource, "var", colour, "disabled", "title", null, HorizontalAlignment.left));
	}

	@Test
	void comboDelegatesToBuilder() {
		Combo combo = new Combo();
		EventSourceComponent result = new EventSourceComponent(mock(UIComponent.class), mock(UIComponentBase.class));
		when(mockBuilder.combo(mockEventSource, "var", combo, "disabled", "title", null)).thenReturn(result);
		assertSame(result, chain.combo(mockEventSource, "var", combo, "disabled", "title", null));
	}

	@Test
	void addContentSignatureDelegatesToBuilder() {
		UIComponent layout = mock(UIComponent.class);
		ContentSignature signature = mock(ContentSignature.class);
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.addContentSignature(mockComponent, layout, signature, "disabled", "title", null)).thenReturn(result);
		assertSame(result, chain.addContentSignature(mockComponent, layout, signature, "disabled", "title", null));
	}

	@Test
	void htmlDelegatesToBuilder() {
		HTML html = mock(HTML.class);
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.html(mockComponent, "var", html, "disabled", "title", null)).thenReturn(result);
		assertSame(result, chain.html(mockComponent, "var", html, "disabled", "title", null));
	}

	@Test
	void lookupDescriptionDelegatesToBuilder() {
		LookupDescription lookup = mock(LookupDescription.class);
		QueryDefinition query = mock(QueryDefinition.class);
		EventSourceComponent result = new EventSourceComponent(mock(UIComponent.class), mock(UIComponentBase.class));
		when(mockBuilder.lookupDescription(mockEventSource, "var", lookup, "disabled", "title", null, HorizontalAlignment.left, "displayBinding", query)).thenReturn(result);
		assertSame(result, chain.lookupDescription(mockEventSource, "var", lookup, "disabled", "title", null, HorizontalAlignment.left, "displayBinding", query));
	}

	@Test
	void passwordDelegatesToBuilder() {
		Password password = mock(Password.class);
		EventSourceComponent result = new EventSourceComponent(mock(UIComponent.class), mock(UIComponentBase.class));
		when(mockBuilder.password(mockEventSource, "var", password, "disabled", "title", null, HorizontalAlignment.left)).thenReturn(result);
		assertSame(result, chain.password(mockEventSource, "var", password, "disabled", "title", null, HorizontalAlignment.left));
	}

	@Test
	void radioDelegatesToBuilder() {
		Radio radio = new Radio();
		EventSourceComponent result = new EventSourceComponent(mock(UIComponent.class), mock(UIComponentBase.class));
		when(mockBuilder.radio(mockEventSource, "var", radio, "disabled", "title", null)).thenReturn(result);
		assertSame(result, chain.radio(mockEventSource, "var", radio, "disabled", "title", null));
	}

	@Test
	void richTextDelegatesToBuilder() {
		RichText text = mock(RichText.class);
		EventSourceComponent result = new EventSourceComponent(mock(UIComponent.class), mock(UIComponentBase.class));
		when(mockBuilder.richText(mockEventSource, "var", text, "disabled", "title", null)).thenReturn(result);
		assertSame(result, chain.richText(mockEventSource, "var", text, "disabled", "title", null));
	}

	@Test
	void spinnerDelegatesToBuilder() {
		Spinner spinner = mock(Spinner.class);
		EventSourceComponent result = new EventSourceComponent(mock(UIComponent.class), mock(UIComponentBase.class));
		jakarta.faces.convert.Converter<?> facesConverter = mock(jakarta.faces.convert.Converter.class);
		when(mockBuilder.spinner(mockEventSource, "var", spinner, "disabled", "title", null, HorizontalAlignment.left, facesConverter)).thenReturn(result);
		assertSame(result, chain.spinner(mockEventSource, "var", spinner, "disabled", "title", null, HorizontalAlignment.left, facesConverter));
	}

	@Test
	void sliderDelegatesToBuilder() {
		Slider slider = mock(Slider.class);
		EventSourceComponent result = new EventSourceComponent(mock(UIComponent.class), mock(UIComponentBase.class));
		jakarta.faces.convert.Converter<?> facesConverter = mock(jakarta.faces.convert.Converter.class);
		when(mockBuilder.slider(mockEventSource, "var", slider, "disabled", "title", null, facesConverter)).thenReturn(result);
		assertSame(result, chain.slider(mockEventSource, "var", slider, "disabled", "title", null, facesConverter));
	}

	@Test
	void textAreaDelegatesToBuilder() {
		TextArea text = mock(TextArea.class);
		EventSourceComponent result = new EventSourceComponent(mock(UIComponent.class), mock(UIComponentBase.class));
		when(mockBuilder.textArea(mockEventSource, "var", text, "disabled", "title", null, HorizontalAlignment.left, 100)).thenReturn(result);
		assertSame(result, chain.textArea(mockEventSource, "var", text, "disabled", "title", null, HorizontalAlignment.left, 100));
	}

	@Test
	void textDelegatesToBuilder() {
		TextField textField = mock(TextField.class);
		Converter<?> converter = mock(Converter.class);
		Format<?> format = mock(Format.class);
		jakarta.faces.convert.Converter<?> facesConverter = mock(jakarta.faces.convert.Converter.class);
		EventSourceComponent result = new EventSourceComponent(mock(UIComponent.class), mock(UIComponentBase.class));
		when(mockBuilder.text(mockEventSource, "var", textField, "disabled", "title", null, HorizontalAlignment.left, 100, converter, format, facesConverter)).thenReturn(result);
		assertSame(result, chain.text(mockEventSource, "var", textField, "disabled", "title", null, HorizontalAlignment.left, 100, converter, format, facesConverter));
	}

	@Test
	void actionLinkDelegatesToBuilder() {
		Link link = mock(Link.class);
		Action action = mock(Action.class);
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.actionLink(mockComponent, "binding", "var", "value", "icon", "tip", "confirm", link, action)).thenReturn(result);
		assertSame(result, chain.actionLink(mockComponent, "binding", "var", "value", "icon", "tip", "confirm", link, action));
	}

	@Test
	void reportDelegatesToBuilder() {
		Action action = mock(Action.class);
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.report(mockComponent, "label", "icon", "tip", "confirm", action)).thenReturn(result);
		assertSame(result, chain.report(mockComponent, "label", "icon", "tip", "confirm", action));
	}

	@Test
	void downloadDelegatesToBuilder() {
		Action action = mock(Action.class);
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.download(mockComponent, "binding", "var", "label", "icon", "tip", "confirm", action)).thenReturn(result);
		assertSame(result, chain.download(mockComponent, "binding", "var", "label", "icon", "tip", "confirm", action));
	}

	@Test
	void uploadDelegatesToBuilder() {
		Action action = mock(Action.class);
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.upload(mockComponent, "label", "icon", "tip", "confirm", action)).thenReturn(result);
		assertSame(result, chain.upload(mockComponent, "label", "icon", "tip", "confirm", action));
	}

	@Test
	void removeDelegatesToBuilder() {
		Action action = mock(Action.class);
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.remove(mockComponent, "label", "icon", "tip", "confirm", action, true)).thenReturn(result);
		assertSame(result, chain.remove(mockComponent, "label", "icon", "tip", "confirm", action, true));
	}

	@Test
	void actionDelegatesToBuilder() {
		Action action = mock(Action.class);
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.action(mockComponent, "binding", "var", "label", "icon", "tip", "confirm", ImplicitActionName.Save, action)).thenReturn(result);
		assertSame(result, chain.action(mockComponent, "binding", "var", "label", "icon", "tip", "confirm", ImplicitActionName.Save, action));
	}

	// ===== Two-builder chaining =====

	@Test
	void twoBuilderChainPassesResultThrough() {
		ComponentBuilder second = mock(ComponentBuilder.class);
		ComponentBuilderChain twoChain = new ComponentBuilderChain(mockBuilder, second);
		UIComponent midResult = mock(UIComponent.class);
		UIComponent finalResult = mock(UIComponent.class);
		when(mockBuilder.view(mockComponent, false)).thenReturn(midResult);
		when(second.view(midResult, false)).thenReturn(finalResult);
		assertSame(finalResult, twoChain.view(mockComponent, false));
	}

	@Test
	void viewWithNullInputReturnsBuilderResult() {
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.view(null, false)).thenReturn(result);
		assertSame(result, chain.view(null, false));
	}
}
