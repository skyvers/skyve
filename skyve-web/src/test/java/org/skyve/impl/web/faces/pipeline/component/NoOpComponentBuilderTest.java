package org.skyve.impl.web.faces.pipeline.component;

import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.types.converters.Converter;
import org.skyve.impl.metadata.view.HorizontalAlignment;
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
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.query.QueryDefinition;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.model.list.ListModel;
import jakarta.el.ELContext;
import jakarta.el.ExpressionFactory;
import jakarta.faces.application.Application;
import jakarta.faces.component.UIComponent;
import jakarta.faces.component.UIComponentBase;
import jakarta.faces.context.FacesContext;

/**
 * Tests for NoOpComponentBuilder - verifies each method returns the input component unchanged.
 */
class NoOpComponentBuilderTest {

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

	private NoOpComponentBuilder builder;
	private UIComponent component;
	private EventSourceComponent eventSourceComponent;

	@BeforeEach
	void setUp() {
		builder = new NoOpComponentBuilder();
		component = mock(UIComponent.class);
		eventSourceComponent = new EventSourceComponent(component, mock(UIComponentBase.class));
	}

	@Test
	void viewReturnsInputComponent() {
		assertSame(component, builder.view(component, false));
	}

	@Test
	void viewCreateReturnsInputComponent() {
		assertSame(component, builder.view(component, true));
	}

	@Test
	void toolbarsReturnsInputList() {
		List<UIComponent> list = new ArrayList<>();
		assertSame(list, builder.toolbars(list, "widgetId"));
	}

	@Test
	void tabPaneReturnsInputComponent() {
		assertSame(component, builder.tabPane(component, new TabPane(), "module", "document"));
	}

	@Test
	void tabReturnsInputComponent() {
		assertSame(component, builder.tab(component, "title", new Tab()));
	}

	@Test
	void tabPaneScriptReturnsInputComponent() {
		assertSame(component, builder.tabPaneScript(component, new TabPane(), "module", "document", "compId"));
	}

	@Test
	void sidebarScriptReturnsInputComponent() {
		assertSame(component, builder.sidebarScript(component, new Sidebar(), false, "sidebarId"));
	}

	@Test
	void borderReturnsInputComponent() {
		assertSame(component, builder.border(component, "title", null, null, null));
	}

	@Test
	void labelSimpleReturnsInputComponent() {
		assertSame(component, builder.label(component, "value"));
	}

	@Test
	void spacerReturnsInputComponent() {
		assertSame(component, builder.spacer(component, new Spacer()));
	}

	@Test
	void zoomInReturnsInputComponent() {
		assertSame(component, builder.zoomIn(component, "label", "icon", "tip", new ZoomIn(), "disabled"));
	}

	@Test
	void actionButtonReturnsInputComponent() {
		assertSame(component, builder.actionButton(component, "binding", "var", "label", "icon", "tip", "confirm", mock(Button.class), "disabled", mock(Action.class)));
	}

	@Test
	void reportButtonReturnsInputComponent() {
		assertSame(component, builder.reportButton(component, "label", "icon", "tip", "confirm", mock(Button.class), "disabled", mock(Action.class)));
	}

	@Test
	void downloadButtonReturnsInputComponent() {
		assertSame(component, builder.downloadButton(component, "binding", "var", "label", "icon", "tip", "confirm", mock(Button.class), "disabled", mock(Action.class)));
	}

	@Test
	void uploadButtonReturnsInputComponent() {
		assertSame(component, builder.uploadButton(component, "label", "icon", "tip", "confirm", mock(Button.class), "disabled", mock(Action.class)));
	}

	@Test
	void staticImageReturnsInputComponent() {
		assertSame(component, builder.staticImage(component, "url", new StaticImage()));
	}

	@Test
	void dynamicImageReturnsInputComponent() {
		assertSame(component, builder.dynamicImage(component, new DynamicImage(), "module", "document"));
	}

	@Test
	void blurbReturnsInputComponent() {
		assertSame(component, builder.blurb(component, "var", "value", "binding", new Blurb()));
	}

	@Test
	void labelWithBindingReturnsInputComponent() {
		assertSame(component, builder.label(component, "var", "value", "binding", new Label()));
	}

	@Test
	void dataGridReturnsInputComponent() {
		assertSame(component, builder.dataGrid(component, "var", false, new DataGrid()));
	}

	@Test
	void dataRepeaterReturnsInputComponent() {
		assertSame(component, builder.dataRepeater(component, "var", new DataRepeater()));
	}

	@Test
	void addDataGridBoundColumnReturnsInputComponent() {
		assertSame(component, builder.addDataGridBoundColumn(component, component, mock(AbstractDataWidget.class), new DataGridBoundColumn(), "var", "title", "binding", new StringBuilder(), null, null));
	}

	@Test
	void addedDataGridBoundColumnReturnsInputComponent() {
		assertSame(component, builder.addedDataGridBoundColumn(component, component, null));
	}

	@Test
	void addDataGridContainerColumnReturnsInputComponent() {
		assertSame(component, builder.addDataGridContainerColumn(component, component, mock(AbstractDataWidget.class), "title", new DataGridContainerColumn(), null));
	}

	@Test
	void addedDataGridContainerColumnReturnsInputComponent() {
		assertSame(component, builder.addedDataGridContainerColumn(component, component));
	}

	@Test
	void addDataGridActionColumnReturnsInputComponent() {
		assertSame(component, builder.addDataGridActionColumn(component, component, new DataGrid(), "var", "expr", "alias", false, true, true));
	}

	@Test
	void mapWithQueryReturnsInputComponent() {
		assertSame(component, builder.map(component, new MapDisplay(), "module", "query", "binding"));
	}

	@Test
	void mapWithModelReturnsInputComponent() {
		assertSame(component, builder.map(component, new MapDisplay(), "modelName"));
	}

	@Test
	void geometryReturnsInputEventSource() {
		assertSame(eventSourceComponent, builder.geometry(eventSourceComponent, "var", new Geometry(), "disabled", "title", null, HorizontalAlignment.left));
	}

	@Test
	void geometryMapReturnsInputEventSource() {
		assertSame(eventSourceComponent, builder.geometryMap(eventSourceComponent, new GeometryMap(), "disabled", "title", null));
	}

	@Test
	void chartReturnsInputComponent() {
		assertSame(component, builder.chart(component, new Chart()));
	}

	@Test
	void listGridReturnsInputComponent() {
		assertSame(component, builder.listGrid(component, "module", "docName", "model", "uxui", mock(ListModel.class), mock(Document.class), new ListGrid(), null, false));
	}

	@Test
	void listGridContextMenuReturnsInputComponent() {
		assertSame(component, builder.listGridContextMenu(component, "listGridId", new ListGrid()));
	}

	@Test
	void listRepeaterReturnsInputComponent() {
		assertSame(component, builder.listRepeater(component, "docName", "model", "uxui", mock(ListModel.class), new ArrayList<>(), new ArrayList<>(), true, true));
	}

	@Test
	void listMembershipReturnsInputEventSource() {
		assertSame(eventSourceComponent,
					builder.listMembership(eventSourceComponent,
											EscapableText.of("candidates", true),
											EscapableText.of("members", true),
											new ListMembership()));
	}

	@Test
	void checkBoxReturnsInputEventSource() {
		assertSame(eventSourceComponent, builder.checkBox(eventSourceComponent, "var", new CheckBox(), "disabled", "title", null));
	}

	@Test
	void colourPickerReturnsInputEventSource() {
		assertSame(eventSourceComponent, builder.colourPicker(eventSourceComponent, "var", new ColourPicker(), "disabled", "title", null, HorizontalAlignment.left));
	}

	@Test
	void comboReturnsInputEventSource() {
		assertSame(eventSourceComponent, builder.combo(eventSourceComponent, "var", new Combo(), "disabled", "title", null));
	}



	@Test
	void addContentSignatureReturnsInputComponent() {
		assertSame(component, builder.addContentSignature(component, component, new ContentSignature(), "disabled", "title", null));
	}

	@Test
	void htmlReturnsInputComponent() {
		assertSame(component, builder.html(component, "var", new HTML(), "disabled", "title", null));
	}

	@Test
	void lookupDescriptionReturnsInputEventSource() {
		assertSame(eventSourceComponent, builder.lookupDescription(eventSourceComponent, "var", new LookupDescription(), "disabled", "title", null, HorizontalAlignment.left, "displayBinding", mock(QueryDefinition.class)));
	}

	@Test
	void passwordReturnsInputEventSource() {
		assertSame(eventSourceComponent, builder.password(eventSourceComponent, "var", new Password(), "disabled", "title", null, HorizontalAlignment.left));
	}

	@Test
	void radioReturnsInputEventSource() {
		assertSame(eventSourceComponent, builder.radio(eventSourceComponent, "var", new Radio(), "disabled", "title", null));
	}

	@Test
	void richTextReturnsInputEventSource() {
		assertSame(eventSourceComponent, builder.richText(eventSourceComponent, "var", new RichText(), "disabled", "title", null));
	}

	@Test
	void spinnerReturnsInputEventSource() {
		assertSame(eventSourceComponent, builder.spinner(eventSourceComponent, "var", new Spinner(), "disabled", "title", null, HorizontalAlignment.left, null));
	}

	@Test
	void sliderReturnsInputEventSource() {
		assertSame(eventSourceComponent, builder.slider(eventSourceComponent, "var", new Slider(), "disabled", "title", null, null));
	}

	@Test
	void textAreaReturnsInputEventSource() {
		assertSame(eventSourceComponent, builder.textArea(eventSourceComponent, "var", new TextArea(), "disabled", "title", null, HorizontalAlignment.left, null));
	}

	@Test
	void textReturnsInputEventSource() {
		assertSame(eventSourceComponent, builder.text(eventSourceComponent, "var", new TextField(), "disabled", "title", null, HorizontalAlignment.left, null, mock(Converter.class), null, null));
	}

	@Test
	void actionLinkReturnsInputComponent() {
		assertSame(component, builder.actionLink(component, "binding", "var", "value", "icon", "tip", "confirm", new Link(), mock(Action.class)));
	}

	@Test
	void reportReturnsInputComponent() {
		assertSame(component, builder.report(component, "label", "icon", "tip", "confirm", mock(Action.class)));
	}

	@Test
	void downloadReturnsInputComponent() {
		assertSame(component, builder.download(component, "binding", "var", "label", "icon", "tip", "confirm", mock(Action.class)));
	}

	@Test
	void uploadReturnsInputComponent() {
		assertSame(component, builder.upload(component, "label", "icon", "tip", "confirm", mock(Action.class)));
	}

	@Test
	void removeReturnsInputComponent() {
		assertSame(component, builder.remove(component, "label", "icon", "tip", "confirm", mock(Action.class), true));
	}

	@Test
	void actionReturnsInputComponent() {
		assertSame(component, builder.action(component, "binding", "var", "label", "icon", "tip", "confirm", ImplicitActionName.OK, mock(Action.class)));
	}
}
