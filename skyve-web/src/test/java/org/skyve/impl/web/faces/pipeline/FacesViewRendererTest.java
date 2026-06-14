package org.skyve.impl.web.faces.pipeline;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

import org.primefaces.component.picklist.PickList;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.skyve.domain.types.converters.Converter;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.container.Collapsible;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.widget.Blurb;
import org.skyve.impl.metadata.view.widget.Chart;
import org.skyve.impl.metadata.view.widget.DynamicImage;
import org.skyve.impl.metadata.view.widget.MapDisplay;
import org.skyve.impl.metadata.view.widget.Spacer;
import org.skyve.impl.metadata.view.widget.StaticImage;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.sail.mock.MockFacesContext;
import org.skyve.impl.web.faces.pipeline.component.ComponentBuilder;
import org.skyve.impl.web.faces.pipeline.layout.LayoutBuilder;
import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View.ViewType;

import jakarta.faces.component.UIComponent;
import jakarta.faces.component.UIComponentBase;
import jakarta.faces.context.FacesContext;

@SuppressWarnings("static-method")
class FacesViewRendererTest {

	private abstract static class FacesContextBridge extends FacesContext {
		static void setCurrent(FacesContext context) {
			setCurrentInstance(context);
		}
	}

	private static final class TestComponent extends UIComponentBase {
		private final String family;

		private TestComponent(String family) {
			this.family = family;
		}

		@Override
		public String getFamily() {
			return family;
		}
	}

	@AfterEach
	void clearFacesContext() {
		FacesContextBridge.setCurrent(null);
	}

	@Test
	void renderViewAddsLayoutWhenWidgetIdIsNull() {
		UIComponent root = new TestComponent("root");
		UIComponent layout = new TestComponent("layout");
		ComponentBuilder cb = mock(ComponentBuilder.class);
		LayoutBuilder lb = mock(LayoutBuilder.class);
		ViewImpl view = createView(null);

		when(cb.view(null, false)).thenReturn(root);
		when(lb.toolbarLayouts(null)).thenReturn(new ArrayList<>());
		when(lb.viewLayout(null)).thenReturn(layout);

		FacesViewRenderer renderer = newRenderer(view, null, cb, lb);
		renderer.renderView(null, null);

		assertEquals(1, root.getChildren().size());
		assertSame(layout, root.getChildren().get(0));
	}

	@Test
	void renderViewSkipsLayoutWhenWidgetIdIsPresent() {
		UIComponent root = new TestComponent("root");
		UIComponent layout = new TestComponent("layout");
		ComponentBuilder cb = mock(ComponentBuilder.class);
		LayoutBuilder lb = mock(LayoutBuilder.class);
		ViewImpl view = createView(null);

		when(cb.view(null, false)).thenReturn(root);
		when(lb.toolbarLayouts(null)).thenReturn(new ArrayList<>());
		when(lb.viewLayout(null)).thenReturn(layout);

		FacesViewRenderer renderer = newRenderer(view, "fragment", cb, lb);
		renderer.renderView(null, null);

		assertEquals(0, root.getChildren().size());
	}

	@Test
	void renderViewMarksCreateViewsForBuilder() {
		UIComponent root = new TestComponent("root");
		ComponentBuilder cb = mock(ComponentBuilder.class);
		LayoutBuilder lb = mock(LayoutBuilder.class);
		ViewImpl view = createView(null);
		view.setName(ViewType.create.toString());

		when(cb.view(null, true)).thenReturn(root);
		when(lb.toolbarLayouts(null)).thenReturn(new ArrayList<>());
		when(lb.viewLayout(null)).thenReturn(null);

		FacesViewRenderer renderer = newRenderer(view, null, cb, lb);
		renderer.renderView(null, null);

		verify(cb).view(null, true);
		assertSame(root, renderer.getFacesView());
	}

	@Test
	void renderedViewAddsToolbarsWhenActionsWidgetMatches() {
		UIComponent root = new TestComponent("root");
		UIComponent toolbarLayout = new TestComponent("toolbarLayout");
		toolbarLayout.getChildren().add(new TestComponent("child"));
		UIComponent toolbar = new TestComponent("toolbar");
		ComponentBuilder cb = mock(ComponentBuilder.class);
		LayoutBuilder lb = mock(LayoutBuilder.class);
		ViewImpl view = createView("actions");

		when(cb.view(null, false)).thenReturn(root);
		when(cb.toolbars(null, "actions")).thenReturn(List.of(toolbar));
		when(lb.toolbarLayouts(null)).thenReturn(List.of(toolbarLayout));
		when(lb.viewLayout(null)).thenReturn(null);

		FacesViewRenderer renderer = newRenderer(view, null, cb, lb);
		renderer.renderView(null, null);
		renderer.renderedView(null, null);

		verify(cb).toolbars(null, "actions");
		verify(lb).addToolbarLayouts(List.of(toolbar), List.of(toolbarLayout));
		verify(lb).addToolbarsOrLayouts(root, List.of(toolbar));
	}

	@Test
	void renderedViewFallsBackToToolbarLayoutsWhenComponentBuilderReturnsNoToolbars() {
		UIComponent root = new TestComponent("root");
		UIComponent toolbarLayout = new TestComponent("toolbarLayout");
		toolbarLayout.getChildren().add(new TestComponent("child"));
		ComponentBuilder cb = mock(ComponentBuilder.class);
		LayoutBuilder lb = mock(LayoutBuilder.class);
		ViewImpl view = createView("actions");

		when(cb.view(null, false)).thenReturn(root);
		when(cb.toolbars(null, "actions")).thenReturn(null);
		when(lb.toolbarLayouts(null)).thenReturn(List.of(toolbarLayout));
		when(lb.viewLayout(null)).thenReturn(null);

		FacesViewRenderer renderer = newRenderer(view, null, cb, lb);
		renderer.renderView(null, null);
		renderer.renderedView(null, null);

		verify(lb).addToolbarsOrLayouts(root, List.of(toolbarLayout));
	}

	@Test
	void renderedViewRejectsMismatchedToolbarCounts() {
		UIComponent root = new TestComponent("root");
		UIComponent toolbarLayout = new TestComponent("toolbarLayout");
		toolbarLayout.getChildren().add(new TestComponent("child"));
		ComponentBuilder cb = mock(ComponentBuilder.class);
		LayoutBuilder lb = mock(LayoutBuilder.class);
		ViewImpl view = createView("actions");

		when(cb.view(null, false)).thenReturn(root);
		when(cb.toolbars(null, "actions")).thenReturn(List.of(new TestComponent("toolbar"), new TestComponent("extraToolbar")));
		when(lb.toolbarLayouts(null)).thenReturn(List.of(toolbarLayout));
		when(lb.viewLayout(null)).thenReturn(null);

		FacesViewRenderer renderer = newRenderer(view, null, cb, lb);
		renderer.renderView(null, null);

		IllegalStateException exception = assertThrows(IllegalStateException.class, () -> renderer.renderedView(null, null));
		assertEquals(String.format("The component Builder %s yielded 2 toolbars but Layout Builder %s yielded 1 toolbar layouts",
									cb.getClass().getName(),
									lb.getClass().getName()),
						exception.getMessage());
	}

	@Test
	void renderedViewSkipsToolbarsWhenWidgetIdDoesNotMatchActionsWidgetId() {
		UIComponent root = new TestComponent("root");
		ComponentBuilder cb = mock(ComponentBuilder.class);
		LayoutBuilder lb = mock(LayoutBuilder.class);
		ViewImpl view = createView("actions");

		when(cb.view(null, false)).thenReturn(root);
		when(lb.toolbarLayouts(null)).thenReturn(List.of());
		when(lb.viewLayout(null)).thenReturn(null);

		FacesViewRenderer renderer = newRenderer(view, "fragment", cb, lb);
		renderer.renderView(null, null);
		renderer.renderedView(null, null);

		verify(cb, never()).toolbars(null, "actions");
	}

	@Test
	void visitRendersStaticDisplayWidgetsThroughBuilders() {
		UIComponent root = new TestComponent("root");
		UIComponent blurbComponent = new TestComponent("blurb");
		UIComponent staticImageComponent = new TestComponent("staticImage");
		UIComponent dynamicImageComponent = new TestComponent("dynamicImage");
		UIComponent mapComponent = new TestComponent("map");
		UIComponent chartComponent = new TestComponent("chart");
		UIComponent spacerComponent = new TestComponent("spacer");
		ComponentBuilder cb = mock(ComponentBuilder.class);
		LayoutBuilder lb = mock(LayoutBuilder.class);
		ViewImpl view = createView(null);
		Blurb blurb = new Blurb();
		blurb.setMarkup("<b>markup</b>");
		StaticImage staticImage = new StaticImage();
		staticImage.setRelativeFile("images/example.png");
		DynamicImage dynamicImage = new DynamicImage();
		dynamicImage.setName("thumb");
		MapDisplay map = new MapDisplay();
		map.setModelName("locations");
		Chart chart = new Chart();
		chart.setModelName("totals");
		Spacer spacer = new Spacer();
		view.getContained().add(blurb);
		view.getContained().add(staticImage);
		view.getContained().add(dynamicImage);
		view.getContained().add(map);
		view.getContained().add(chart);
		view.getContained().add(spacer);

		when(cb.view(null, false)).thenReturn(root);
		when(cb.blurb(null, null, "<b>markup</b>", null, blurb)).thenReturn(blurbComponent);
		when(cb.staticImage(null, "images/images/example.png", staticImage)).thenReturn(staticImageComponent);
		when(cb.dynamicImage(null, dynamicImage, "testModule", "testDocument")).thenReturn(dynamicImageComponent);
		when(cb.map(null, map, "locations")).thenReturn(mapComponent);
		when(cb.chart(null, chart)).thenReturn(chartComponent);
		when(cb.spacer(null, spacer)).thenReturn(spacerComponent);
		when(lb.toolbarLayouts(null)).thenReturn(new ArrayList<>());
		when(lb.viewLayout(null)).thenReturn(null);
		when(lb.addToContainer(any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any())).thenReturn(root);
		when(lb.addedToContainer(any(), any(), any())).thenReturn(root);

		FacesViewRenderer renderer = newRenderer(view, null, cb, lb);
		renderer.visit();

		assertSame(root, renderer.getFacesView());
		verify(cb).blurb(null, null, "<b>markup</b>", null, blurb);
		verify(cb).staticImage(null, "images/images/example.png", staticImage);
		verify(cb).dynamicImage(null, dynamicImage, "testModule", "testDocument");
		verify(cb).map(null, map, "locations");
		verify(cb).chart(null, chart);
		verify(cb).spacer(null, spacer);
	}

	@Test
	void actionPanelRenderingDispatchesToMatchingBuilderMethods() {
		UIComponent root = new TestComponent("root");
		UIComponent toolbarLayout = new TestComponent("toolbarLayout");
		UIComponent reportComponent = new TestComponent("report");
		UIComponent downloadComponent = new TestComponent("download");
		UIComponent uploadComponent = new TestComponent("upload");
		UIComponent removeComponent = new TestComponent("remove");
		UIComponent zoomOutComponent = new TestComponent("zoomOut");
		UIComponent okComponent = new TestComponent("ok");
		UIComponent saveComponent = new TestComponent("save");
		UIComponent cancelComponent = new TestComponent("cancel");
		UIComponent deleteComponent = new TestComponent("delete");
		UIComponent bizExportComponent = new TestComponent("bizExport");
		UIComponent bizImportComponent = new TestComponent("bizImport");
		UIComponent customComponent = new TestComponent("custom");
		ComponentBuilder cb = mock(ComponentBuilder.class);
		LayoutBuilder lb = mock(LayoutBuilder.class);
		ViewImpl view = createView(null);
		ActionImpl action = new ActionImpl();

		when(cb.view(null, false)).thenReturn(root);
		when(lb.toolbarLayouts(null)).thenReturn(List.of(toolbarLayout));
		when(lb.viewLayout(null)).thenReturn(null);
		when(cb.report(null, "Report", "fa-report", "tip", "confirm", action)).thenReturn(reportComponent);
		when(cb.download(null, null, null, "Download", "fa-download", "tip", "confirm", action)).thenReturn(downloadComponent);
		when(cb.upload(null, "Upload", "fa-upload", "tip", "confirm", action)).thenReturn(uploadComponent);
		when(cb.remove(null, "Remove", "fa-remove", "tip", "confirm", action, true)).thenReturn(removeComponent);
		when(cb.action(null, null, null, "Zoom Out", "fa-zoom-out", "tip", "confirm", ImplicitActionName.ZoomOut, action)).thenReturn(zoomOutComponent);
		when(cb.action(null, null, null, "OK", "fa-ok", "tip", "confirm", ImplicitActionName.OK, action)).thenReturn(okComponent);
		when(cb.action(null, null, null, "Save", "fa-save", "tip", "confirm", ImplicitActionName.Save, action)).thenReturn(saveComponent);
		when(cb.action(null, null, null, "Cancel", "fa-cancel", "tip", "confirm", ImplicitActionName.Cancel, action)).thenReturn(cancelComponent);
		when(cb.action(null, null, null, "Delete", "fa-delete", "tip", "confirm", ImplicitActionName.Delete, action)).thenReturn(deleteComponent);
		when(cb.action(null, null, null, "Biz Export", "fa-biz-export", "tip", "confirm", ImplicitActionName.BizExport, action)).thenReturn(bizExportComponent);
		when(cb.action(null, null, null, "Biz Import", "fa-biz-import", "tip", "confirm", ImplicitActionName.BizImport, action)).thenReturn(bizImportComponent);
		when(cb.action(null, null, null, "Custom", "fa-custom", "tip", "confirm", null, action)).thenReturn(customComponent);

		FacesViewRenderer renderer = newRenderer(view, null, cb, lb);
		renderer.renderView(null, null);
		renderer.renderReportAction("report", "Report", null, "fa-report", "tip", "confirm", action);
		renderer.renderDownloadAction("download", "Download", null, "fa-download", "tip", "confirm", action);
		renderer.renderUploadAction("upload", "Upload", null, "fa-upload", "tip", "confirm", action);
		renderer.renderRemoveAction("remove", "Remove", null, "fa-remove", "tip", "confirm", action, true);
		renderer.renderZoomOutAction("zoomOut", "Zoom Out", null, "fa-zoom-out", "tip", "confirm", action);
		renderer.renderOKAction("ok", "OK", null, "fa-ok", "tip", "confirm", action);
		renderer.renderSaveAction("save", "Save", null, "fa-save", "tip", "confirm", action);
		renderer.renderCancelAction("cancel", "Cancel", null, "fa-cancel", "tip", "confirm", action);
		renderer.renderDeleteAction("delete", "Delete", null, "fa-delete", "tip", "confirm", action);
		renderer.renderBizExportAction("bizExport", "Biz Export", null, "fa-biz-export", "tip", "confirm", action);
		renderer.renderBizImportAction("bizImport", "Biz Import", null, "fa-biz-import", "tip", "confirm", action);
		renderer.renderCustomAction("custom", "Custom", null, "fa-custom", "tip", "confirm", action);

		assertEquals(List.of(reportComponent,
								downloadComponent,
								uploadComponent,
								removeComponent,
								zoomOutComponent,
								okComponent,
								saveComponent,
								cancelComponent,
								deleteComponent,
								bizExportComponent,
								bizImportComponent,
								customComponent),
						toolbarLayout.getChildren());
	}

	@Test
	void actionPanelRenderingLeavesUnimplementedActionsOutOfToolbar() {
		UIComponent root = new TestComponent("root");
		UIComponent toolbarLayout = new TestComponent("toolbarLayout");
		ComponentBuilder cb = mock(ComponentBuilder.class);
		LayoutBuilder lb = mock(LayoutBuilder.class);
		ViewImpl view = createView(null);
		ActionImpl action = new ActionImpl();

		when(cb.view(null, false)).thenReturn(root);
		when(lb.toolbarLayouts(null)).thenReturn(List.of(toolbarLayout));
		when(lb.viewLayout(null)).thenReturn(null);

		FacesViewRenderer renderer = newRenderer(view, null, cb, lb);
		renderer.renderView(null, null);
		renderer.renderAddAction("add", "Add", null, "fa-add", "tip", "confirm", action);
		renderer.renderNavigateAction("navigate", "Navigate", null, "fa-navigate", "tip", "confirm", action);
		renderer.renderNewAction("new", "New", null, "fa-new", "tip", "confirm", action);
		renderer.renderEditAction("edit", "Edit", null, "fa-edit", "tip", "confirm", action);
		renderer.renderPrintAction("print", "Print", null, "fa-print", "tip", "confirm", action);

		assertEquals(0, toolbarLayout.getChildren().size());
		verify(cb, never()).action(any(), any(), any(), any(), any(), any(), any(), any(), any());
	}

	@Test
	void actionPanelRenderingSkipsActionsOutsideActionPanel() {
		UIComponent root = new TestComponent("root");
		UIComponent toolbarLayout = new TestComponent("toolbarLayout");
		ComponentBuilder cb = mock(ComponentBuilder.class);
		LayoutBuilder lb = mock(LayoutBuilder.class);
		ViewImpl view = createView(null);
		ActionImpl action = new ActionImpl();
		action.setInActionPanel(Boolean.FALSE);

		when(cb.view(null, false)).thenReturn(root);
		when(lb.toolbarLayouts(null)).thenReturn(List.of(toolbarLayout));
		when(lb.viewLayout(null)).thenReturn(null);

		FacesViewRenderer renderer = newRenderer(view, null, cb, lb);
		renderer.renderView(null, null);
		renderer.renderReportAction("report", "Report", null, "fa-report", "tip", "confirm", action);
		renderer.renderCustomAction("custom", "Custom", null, "fa-custom", "tip", "confirm", action);

		assertEquals(0, toolbarLayout.getChildren().size());
	}

	@Test
	void eventHandlersWireExpectedAjaxBehaviorNames() throws Exception {
		UIComponentBase eventSource = new TestComponent("eventSource");
		ComponentBuilder cb = mock(ComponentBuilder.class);
		LayoutBuilder lb = mock(LayoutBuilder.class);
		ViewImpl view = createView(null);
		TextField text = new TextField();
		text.setBinding("name");
		LookupDescription lookup = new LookupDescription();
		lookup.setBinding("customer");

		FacesViewRenderer renderer = newRenderer(view, null, cb, lb);
		setEventSource(renderer, eventSource);
		renderer.visitOnChangedEventHandler(text, true, true);
		renderer.visitOnFocusEventHandler(text, true, true);
		renderer.visitOnBlurEventHandler(text, true, true);
		renderer.visitOnPickedEventHandler(lookup, true, true);
		renderer.visitOnClearedEventHandler(lookup, true, true);

		verify(cb).addAjaxBehavior(eventSource, "change", null, null, "name", text.getChangedActions());
		verify(cb).addAjaxBehavior(eventSource, "focus", null, null, "name", text.getFocusActions());
		verify(cb).addAjaxBehavior(eventSource, "blur", null, null, "name", text.getBlurActions());
		verify(cb).addAjaxBehavior(eventSource, "itemSelect", null, null, "customer", lookup.getPickedActions());
		verify(cb).addAjaxBehavior(eventSource, "itemUnselect", null, null, "customer", lookup.getClearedActions());
	}

	@Test
	void changedEventUsesTransferEventForPickLists() throws Exception {
		PickList eventSource = new PickList();
		ComponentBuilder cb = mock(ComponentBuilder.class);
		LayoutBuilder lb = mock(LayoutBuilder.class);
		ViewImpl view = createView(null);
		TextField text = new TextField();
		text.setBinding("members");

		FacesViewRenderer renderer = newRenderer(view, null, cb, lb);
		setEventSource(renderer, eventSource);
		renderer.visitOnChangedEventHandler(text, true, true);

		verify(cb).addAjaxBehavior(eventSource, "transfer", null, null, "members", text.getChangedActions());
	}

	@Test
	void visitWrapsBorderedContainerAndMovesMatchingFragmentToViewRoot() {
		UIComponent root = new TestComponent("root");
		UIComponent border = new TestComponent("border");
		UIComponent layout = new TestComponent("vboxLayout");
		ComponentBuilder cb = mock(ComponentBuilder.class);
		LayoutBuilder lb = mock(LayoutBuilder.class);
		ViewImpl view = createView(null);
		VBox vbox = new VBox();
		vbox.setWidgetId("details");
		vbox.setBorder(Boolean.TRUE);
		vbox.setBorderTitle("Details");
		vbox.setPixelWidth(Integer.valueOf(320));
		vbox.setCollapsible(Collapsible.open);
		view.getContained().add(vbox);

		when(cb.view(null, false)).thenReturn(root);
		when(cb.border(null, "Details", null, Integer.valueOf(320), Collapsible.open)).thenReturn(border);
		when(lb.toolbarLayouts(null)).thenReturn(new ArrayList<>());
		when(lb.viewLayout(null)).thenReturn(null);
		when(lb.vboxLayout(null, vbox)).thenReturn(layout);
		when(lb.addToContainer(null,
								view,
								root,
								border,
								Integer.valueOf(320),
								null,
								null,
								null,
								null,
								null,
								null,
								null)).thenReturn(border);
		when(lb.addedBorderLayout(null, layout)).thenReturn(border);
		when(lb.addedToContainer(null, vbox, border)).thenReturn(root);

		FacesViewRenderer renderer = newRenderer(view, "details", cb, lb);
		renderer.visit();

		assertEquals(List.of(border), root.getChildren());
		verify(lb).addBorderLayout(border, layout);
	}

	@Test
	void visitRejectsCollapsibleContainerWithoutBorderTitle() {
		UIComponent root = new TestComponent("root");
		UIComponent layout = new TestComponent("vboxLayout");
		ComponentBuilder cb = mock(ComponentBuilder.class);
		LayoutBuilder lb = mock(LayoutBuilder.class);
		ViewImpl view = createView(null);
		VBox vbox = new VBox();
		vbox.setCollapsible(Collapsible.closed);
		view.getContained().add(vbox);

		when(cb.view(null, false)).thenReturn(root);
		when(lb.toolbarLayouts(null)).thenReturn(new ArrayList<>());
		when(lb.viewLayout(null)).thenReturn(null);
		when(lb.vboxLayout(null, vbox)).thenReturn(layout);

		FacesViewRenderer renderer = newRenderer(view, null, cb, lb);

		MetaDataException exception = assertThrows(MetaDataException.class, renderer::visit);
		assertEquals("Border title must be defined if the collapsible attribute is present", exception.getMessage());
	}

	@ParameterizedTest
	@MethodSource("skyveConverterMappings")
	void convertConverterMapsSkyveConvertersToFacesConverters(Converter<?> skyveConverter,
																Class<?> facesConverterClass)
	throws Exception {
		jakarta.faces.convert.Converter<?> facesConverter = convertConverter(skyveConverter, null);

		assertInstanceOf(facesConverterClass, facesConverter);
	}

	@ParameterizedTest
	@MethodSource("defaultConverterMappings")
	void convertConverterSuppliesDefaultNumericConverters(AttributeType type, Class<?> facesConverterClass)
	throws Exception {
		jakarta.faces.convert.Converter<?> facesConverter = convertConverter(null, type);

		assertInstanceOf(facesConverterClass, facesConverter);
	}

	@Test
	void convertConverterReturnsNullWhenNoExplicitOrDefaultConverterApplies() throws Exception {
		assertNull(convertConverter(null, AttributeType.text));
	}

	@Test
	void convertConverterRejectsUnknownSkyveConverter() {
		Converter<?> converter = mock(Converter.class);

		Exception exception = assertThrows(IllegalArgumentException.class, () -> convertConverter(converter, null));

		assertEquals(converter + " cannot be converted to a faces converter", exception.getMessage());
	}

	private static ViewImpl createView(String actionsWidgetId) {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");
		view.setActionsWidgetId(actionsWidgetId);
		return view;
	}

	private static FacesViewRenderer newRenderer(ViewImpl view,
						String widgetId,
						ComponentBuilder cb,
						LayoutBuilder lb) {
		FacesContextBridge.setCurrent(new MockFacesContext());
		CustomerImpl customer = new CustomerImpl();
		User user = mock(User.class);
		when(user.getCustomer()).thenReturn(customer);
		ModuleImpl module = new ModuleImpl();
		module.setName("testModule");
		DocumentImpl document = new DocumentImpl();
		document.setName("testDocument");
		document.setOwningModuleName("testModule");
		return new FacesViewRenderer(user, module, document, view, "external", widgetId, cb, lb);
	}

	private static jakarta.faces.convert.Converter<?> convertConverter(Converter<?> converter, AttributeType type)
	throws Exception {
		Method method = FacesViewRenderer.class.getDeclaredMethod("convertConverter", Converter.class, AttributeType.class);
		method.setAccessible(true);
		try {
			return (jakarta.faces.convert.Converter<?>) method.invoke(null, converter, type);
		}
		catch (InvocationTargetException e) {
			Throwable cause = e.getCause();
			if (cause instanceof Exception exception) {
				throw exception;
			}
			throw e;
		}
	}

	private static void setEventSource(FacesViewRenderer renderer, UIComponentBase eventSource) throws Exception {
		Field field = FacesViewRenderer.class.getDeclaredField("eventSource");
		field.setAccessible(true);
		field.set(renderer, eventSource);
	}

	private static Stream<Arguments> defaultConverterMappings() {
		return Stream.of(
				Arguments.of(AttributeType.decimal2, org.skyve.impl.web.faces.converters.decimal.Decimal2Converter.class),
				Arguments.of(AttributeType.decimal5, org.skyve.impl.web.faces.converters.decimal.Decimal5Converter.class),
				Arguments.of(AttributeType.decimal10, org.skyve.impl.web.faces.converters.decimal.Decimal10Converter.class),
				Arguments.of(AttributeType.integer, org.skyve.impl.web.faces.converters.integer.IntegerConverter.class),
				Arguments.of(AttributeType.longInteger, org.skyve.impl.web.faces.converters.integer.LongIntegerConverter.class));
	}

	private static Stream<Arguments> skyveConverterMappings() {
		return Stream.of(
				Arguments.of(new org.skyve.domain.types.converters.date.DD_MM_YYYY(),
								org.skyve.impl.web.faces.converters.date.DD_MM_YYYY.class),
				Arguments.of(new org.skyve.domain.types.converters.date.DD_MMM_YYYY(),
								org.skyve.impl.web.faces.converters.date.DD_MMM_YYYY.class),
				Arguments.of(new org.skyve.domain.types.converters.date.MM_DD_YYYY(),
								org.skyve.impl.web.faces.converters.date.MM_DD_YYYY.class),
				Arguments.of(new org.skyve.domain.types.converters.date.MMM_DD_YYYY(),
								org.skyve.impl.web.faces.converters.date.MMM_DD_YYYY.class),
				Arguments.of(new org.skyve.domain.types.converters.date.YYYY_MM_DD(),
								org.skyve.impl.web.faces.converters.date.YYYY_MM_DD.class),
				Arguments.of(new org.skyve.domain.types.converters.datetime.DD_MM_YYYY_HH_MI(),
								org.skyve.impl.web.faces.converters.datetime.DD_MM_YYYY_HH_MI.class),
				Arguments.of(new org.skyve.domain.types.converters.datetime.DD_MM_YYYY_HH24_MI(),
								org.skyve.impl.web.faces.converters.datetime.DD_MM_YYYY_HH24_MI.class),
				Arguments.of(new org.skyve.domain.types.converters.datetime.DD_MM_YYYY(),
								org.skyve.impl.web.faces.converters.datetime.DD_MM_YYYY.class),
				Arguments.of(new org.skyve.domain.types.converters.datetime.DD_MMM_YYYY_HH_MI(),
								org.skyve.impl.web.faces.converters.datetime.DD_MMM_YYYY_HH_MI.class),
				Arguments.of(new org.skyve.domain.types.converters.datetime.DD_MMM_YYYY_HH24_MI(),
								org.skyve.impl.web.faces.converters.datetime.DD_MMM_YYYY_HH24_MI.class),
				Arguments.of(new org.skyve.domain.types.converters.datetime.DD_MMM_YYYY(),
								org.skyve.impl.web.faces.converters.datetime.DD_MMM_YYYY.class),
				Arguments.of(new org.skyve.domain.types.converters.datetime.MM_DD_YYYY_HH_MI(),
								org.skyve.impl.web.faces.converters.datetime.MM_DD_YYYY_HH_MI.class),
				Arguments.of(new org.skyve.domain.types.converters.datetime.MM_DD_YYYY_HH24_MI(),
								org.skyve.impl.web.faces.converters.datetime.MM_DD_YYYY_HH24_MI.class),
				Arguments.of(new org.skyve.domain.types.converters.datetime.MM_DD_YYYY(),
								org.skyve.impl.web.faces.converters.datetime.MM_DD_YYYY.class),
				Arguments.of(new org.skyve.domain.types.converters.datetime.MMM_DD_YYYY_HH_MI(),
								org.skyve.impl.web.faces.converters.datetime.MMM_DD_YYYY_HH_MI.class),
				Arguments.of(new org.skyve.domain.types.converters.datetime.MMM_DD_YYYY_HH24_MI(),
								org.skyve.impl.web.faces.converters.datetime.MMM_DD_YYYY_HH24_MI.class),
				Arguments.of(new org.skyve.domain.types.converters.datetime.MMM_DD_YYYY(),
								org.skyve.impl.web.faces.converters.datetime.MMM_DD_YYYY.class),
				Arguments.of(new org.skyve.domain.types.converters.datetime.YYYY_MM_DD_HH_MI(),
								org.skyve.impl.web.faces.converters.datetime.YYYY_MM_DD_HH_MI.class),
				Arguments.of(new org.skyve.domain.types.converters.datetime.YYYY_MM_DD_HH24_MI(),
								org.skyve.impl.web.faces.converters.datetime.YYYY_MM_DD_HH24_MI.class),
				Arguments.of(new org.skyve.domain.types.converters.datetime.YYYY_MM_DD(),
								org.skyve.impl.web.faces.converters.datetime.YYYY_MM_DD.class),
				Arguments.of(new org.skyve.domain.types.converters.decimal.currency.Decimal10DollarsAndCents(),
								org.skyve.impl.web.faces.converters.decimal.currency.Decimal10DollarsAndCents.class),
				Arguments.of(new org.skyve.domain.types.converters.decimal.currency.Decimal2DollarsAndCents(),
								org.skyve.impl.web.faces.converters.decimal.currency.Decimal2DollarsAndCents.class),
				Arguments.of(new org.skyve.domain.types.converters.decimal.currency.Decimal2DollarsAndCentsAbsolute(),
								org.skyve.impl.web.faces.converters.decimal.currency.Decimal2DollarsAndCentsAbsolute.class),
				Arguments.of(new org.skyve.domain.types.converters.decimal.currency.Decimal5DollarsAndCents(),
								org.skyve.impl.web.faces.converters.decimal.currency.Decimal5DollarsAndCents.class),
				Arguments.of(new org.skyve.domain.types.converters.decimal.Decimal10Converter(),
								org.skyve.impl.web.faces.converters.decimal.Decimal10Converter.class),
				Arguments.of(new org.skyve.domain.types.converters.decimal.Decimal10TwoDecimalPlaces(),
								org.skyve.impl.web.faces.converters.decimal.Decimal10TwoDecimalPlaces.class),
				Arguments.of(new org.skyve.domain.types.converters.decimal.Decimal2Converter(),
								org.skyve.impl.web.faces.converters.decimal.Decimal2Converter.class),
				Arguments.of(new org.skyve.domain.types.converters.decimal.Decimal2Integer(),
								org.skyve.impl.web.faces.converters.decimal.Decimal2Integer.class),
				Arguments.of(new org.skyve.domain.types.converters.decimal.Decimal2IntegerPercentage(),
								org.skyve.impl.web.faces.converters.decimal.Decimal2IntegerPercentage.class),
				Arguments.of(new org.skyve.domain.types.converters.decimal.Decimal2OneDecimalPlace(),
								org.skyve.impl.web.faces.converters.decimal.Decimal2OneDecimalPlace.class),
				Arguments.of(new org.skyve.domain.types.converters.decimal.Decimal5Converter(),
								org.skyve.impl.web.faces.converters.decimal.Decimal5Converter.class),
				Arguments.of(new org.skyve.domain.types.converters.decimal.Decimal5Integer(),
								org.skyve.impl.web.faces.converters.decimal.Decimal5Integer.class),
				Arguments.of(new org.skyve.domain.types.converters.decimal.Decimal5IntegerPercentage(),
								org.skyve.impl.web.faces.converters.decimal.Decimal5IntegerPercentage.class),
				Arguments.of(new org.skyve.domain.types.converters.decimal.Decimal5OneDecimalPlace(),
								org.skyve.impl.web.faces.converters.decimal.Decimal5OneDecimalPlace.class),
				Arguments.of(new org.skyve.domain.types.converters.decimal.Decimal5TimeDuration(),
								org.skyve.impl.web.faces.converters.decimal.Decimal5TimeDuration.class),
				Arguments.of(new org.skyve.domain.types.converters.decimal.Decimal5TwoDecimalPlaces(),
								org.skyve.impl.web.faces.converters.decimal.Decimal5TwoDecimalPlaces.class),
				Arguments.of(new org.skyve.domain.types.converters.decimal.Decimal5TwoDecimalPlacesPercentage(),
								org.skyve.impl.web.faces.converters.decimal.Decimal5TwoDecimalPlacesPercentage.class),
				Arguments.of(new org.skyve.domain.types.converters.geometry.GeometryConverter(),
								org.skyve.impl.web.faces.converters.geometry.GeometryConverter.class),
				Arguments.of(new org.skyve.domain.types.converters.integer.IntegerConverter(),
								org.skyve.impl.web.faces.converters.integer.IntegerConverter.class),
				Arguments.of(new org.skyve.domain.types.converters.integer.IntegerSeparator(),
								org.skyve.impl.web.faces.converters.integer.IntegerSeparator.class),
				Arguments.of(new org.skyve.domain.types.converters.integer.LongIntegerConverter(),
								org.skyve.impl.web.faces.converters.integer.LongIntegerConverter.class),
				Arguments.of(new org.skyve.domain.types.converters.integer.LongIntegerSeparator(),
								org.skyve.impl.web.faces.converters.integer.LongIntegerSeparator.class),
				Arguments.of(new org.skyve.domain.types.converters.integer.SimplePercentage(),
								org.skyve.impl.web.faces.converters.integer.SimplePercentage.class),
				Arguments.of(new org.skyve.domain.types.converters.time.HH_MI_SS(),
								org.skyve.impl.web.faces.converters.time.HH_MI_SS.class),
				Arguments.of(new org.skyve.domain.types.converters.time.HH_MI(),
								org.skyve.impl.web.faces.converters.time.HH_MI.class),
				Arguments.of(new org.skyve.domain.types.converters.time.HH24_MI_SS(),
								org.skyve.impl.web.faces.converters.time.HH24_MI_SS.class),
				Arguments.of(new org.skyve.domain.types.converters.time.HH24_MI(),
								org.skyve.impl.web.faces.converters.time.HH24_MI.class),
				Arguments.of(new org.skyve.domain.types.converters.timestamp.DD_MM_YYYY_HH_MI_SS(),
								org.skyve.impl.web.faces.converters.timestamp.DD_MM_YYYY_HH_MI_SS.class),
				Arguments.of(new org.skyve.domain.types.converters.timestamp.DD_MM_YYYY_HH24_MI_SS(),
								org.skyve.impl.web.faces.converters.timestamp.DD_MM_YYYY_HH24_MI_SS.class),
				Arguments.of(new org.skyve.domain.types.converters.timestamp.DD_MM_YYYY(),
								org.skyve.impl.web.faces.converters.timestamp.DD_MM_YYYY.class),
				Arguments.of(new org.skyve.domain.types.converters.timestamp.DD_MMM_YYYY_HH_MI_SS(),
								org.skyve.impl.web.faces.converters.timestamp.DD_MMM_YYYY_HH_MI_SS.class),
				Arguments.of(new org.skyve.domain.types.converters.timestamp.DD_MMM_YYYY_HH24_MI_SS(),
								org.skyve.impl.web.faces.converters.timestamp.DD_MMM_YYYY_HH24_MI_SS.class),
				Arguments.of(new org.skyve.domain.types.converters.timestamp.DD_MMM_YYYY(),
								org.skyve.impl.web.faces.converters.timestamp.DD_MMM_YYYY.class),
				Arguments.of(new org.skyve.domain.types.converters.timestamp.MM_DD_YYYY_HH_MI_SS(),
								org.skyve.impl.web.faces.converters.timestamp.MM_DD_YYYY_HH_MI_SS.class),
				Arguments.of(new org.skyve.domain.types.converters.timestamp.MM_DD_YYYY_HH24_MI_SS(),
								org.skyve.impl.web.faces.converters.timestamp.MM_DD_YYYY_HH24_MI_SS.class),
				Arguments.of(new org.skyve.domain.types.converters.timestamp.MM_DD_YYYY(),
								org.skyve.impl.web.faces.converters.timestamp.MM_DD_YYYY.class),
				Arguments.of(new org.skyve.domain.types.converters.timestamp.MMM_DD_YYYY_HH_MI_SS(),
								org.skyve.impl.web.faces.converters.timestamp.MMM_DD_YYYY_HH_MI_SS.class),
				Arguments.of(new org.skyve.domain.types.converters.timestamp.MMM_DD_YYYY_HH24_MI_SS(),
								org.skyve.impl.web.faces.converters.timestamp.MMM_DD_YYYY_HH24_MI_SS.class),
				Arguments.of(new org.skyve.domain.types.converters.timestamp.MMM_DD_YYYY(),
								org.skyve.impl.web.faces.converters.timestamp.MMM_DD_YYYY.class),
				Arguments.of(new org.skyve.domain.types.converters.timestamp.YYYY_MM_DD_HH_MI_SS(),
								org.skyve.impl.web.faces.converters.timestamp.YYYY_MM_DD_HH_MI_SS.class),
				Arguments.of(new org.skyve.domain.types.converters.timestamp.YYYY_MM_DD_HH24_MI_SS(),
								org.skyve.impl.web.faces.converters.timestamp.YYYY_MM_DD_HH24_MI_SS.class),
				Arguments.of(new org.skyve.domain.types.converters.timestamp.YYYY_MM_DD(),
								org.skyve.impl.web.faces.converters.timestamp.YYYY_MM_DD.class));
	}
}
