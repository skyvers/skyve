package org.skyve.impl.web.faces.pipeline.layout;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.primefaces.component.message.Message;
import org.primefaces.component.outputlabel.OutputLabel;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormColumn;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.container.form.FormRow;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.util.Icons;

import jakarta.el.ELContext;
import jakarta.el.ExpressionFactory;
import jakarta.el.ValueExpression;
import jakarta.faces.application.Application;
import jakarta.faces.component.UIComponent;
import jakarta.faces.component.UIViewRoot;
import jakarta.faces.component.html.HtmlOutputText;
import jakarta.faces.component.html.HtmlPanelGroup;
import jakarta.faces.context.FacesContext;

/**
 * Tests for {@link ResponsiveLayoutBuilder} covering all overridden methods.
 * Uses Mockito mocks to avoid the Mojarra runtime dependency.
 */
class ResponsiveLayoutBuilderTest {
	private static final String UNSAFE_TEXT = "<img src=x onerror=alert(1)> & \"quoted\" 'single'";

	private abstract static class FacesContextBridge extends FacesContext {
		static void setCurrent(FacesContext facesContext) {
			setCurrentInstance(facesContext);
		}
	}

	private static Application mockApplication;
	private static ExpressionFactory mockExpressionFactory;
	private static FacesContext mockFacesContext;
	private static UIViewRoot mockViewRoot;
	private static Map<String, Object> viewRootAttributes;
	private static int idCounter;

	@BeforeAll
	static void setUpFacesContext() {
		mockFacesContext = mock(FacesContext.class);
		mockApplication = mock(Application.class);
		mockExpressionFactory = mock(ExpressionFactory.class);
		ELContext elContext = mock(ELContext.class);
		mockViewRoot = mock(UIViewRoot.class);
		viewRootAttributes = new HashMap<>();

		when(mockFacesContext.getApplication()).thenReturn(mockApplication);
		when(mockFacesContext.getELContext()).thenReturn(elContext);
		when(mockFacesContext.getViewRoot()).thenReturn(mockViewRoot);
		when(mockApplication.getExpressionFactory()).thenReturn(mockExpressionFactory);
		when(mockViewRoot.getAttributes()).thenReturn(viewRootAttributes);

		ValueExpression mockValueExpr = mock(ValueExpression.class);
		when(mockExpressionFactory.createValueExpression(any(ELContext.class), anyString(), any(Class.class))).thenReturn(mockValueExpr);
		when(mockExpressionFactory.createValueExpression(anyString(), any(Class.class))).thenReturn(mockValueExpr);

		idCounter = 0;
		FacesContextBridge.setCurrent(mockFacesContext);
	}

	@AfterAll
	static void tearDownFacesContext() {
		FacesContextBridge.setCurrent(null);
	}

	private ResponsiveLayoutBuilder builder;
	private FacesView managedBean;
	private HtmlPanelGroup mockHtmlPanelGroup;
	private OutputLabel mockOutputLabel;
	private Message mockMessage;

	@BeforeEach
	void setUp() {
		viewRootAttributes.clear();

		mockHtmlPanelGroup = mock(HtmlPanelGroup.class);
		mockOutputLabel = mock(OutputLabel.class);
		mockMessage = mock(Message.class);

		when(mockHtmlPanelGroup.getChildren()).thenReturn(new ArrayList<>());
		when(mockOutputLabel.getChildren()).thenReturn(new ArrayList<>());
		when(mockApplication.createComponent(HtmlPanelGroup.COMPONENT_TYPE)).thenReturn(mockHtmlPanelGroup);
		when(mockApplication.createComponent(OutputLabel.COMPONENT_TYPE)).thenReturn(mockOutputLabel);
		when(mockApplication.createComponent(Message.COMPONENT_TYPE)).thenReturn(mockMessage);
		when(mockApplication.createComponent(HtmlOutputText.COMPONENT_TYPE)).thenAnswer(invocation -> new HtmlOutputText());

		builder = new ResponsiveLayoutBuilder();
		managedBean = mock(FacesView.class);
		when(managedBean.nextId()).thenAnswer(inv -> "id" + (++idCounter));
		builder.setSAILManagedBean(managedBean);
	}

	// ── Short-circuit tests (component != null → return as-is) ──────────────

	@Test
	void viewLayoutReturnsExistingComponent() {
		UIComponent existing = mock(UIComponent.class);
		assertSame(existing, builder.viewLayout(existing));
	}

	@Test
	void tabLayoutReturnsExistingComponent() {
		UIComponent existing = mock(UIComponent.class);
		assertSame(existing, builder.tabLayout(existing));
	}

	@Test
	void vboxLayoutReturnsExistingComponent() {
		UIComponent existing = mock(UIComponent.class);
		assertSame(existing, builder.vboxLayout(existing, new VBox()));
	}

	@Test
	void hboxLayoutReturnsExistingComponent() {
		UIComponent existing = mock(UIComponent.class);
		assertSame(existing, builder.hboxLayout(existing, new HBox()));
	}

	@Test
	void addToContainerReturnsExistingComponent() {
		UIComponent existing = mock(UIComponent.class);
		UIComponent container = mock(UIComponent.class);
		UIComponent child = mock(UIComponent.class);
		assertSame(existing, builder.addToContainer(existing, new VBox(), container, child, null, null, null, null, null, null, null, null));
	}

	@Test
	void addedToContainerReturnsExistingComponent() {
		UIComponent existing = mock(UIComponent.class);
		UIComponent container = mock(UIComponent.class);
		assertSame(existing, builder.addedToContainer(existing, new VBox(), container));
	}

	@Test
	void formLayoutReturnsExistingComponent() {
		UIComponent existing = mock(UIComponent.class);
		assertSame(existing, builder.formLayout(existing, new Form()));
	}

	@Test
	void formRowLayoutReturnsExistingComponent() {
		UIComponent existing = mock(UIComponent.class);
		assertSame(existing, builder.formRowLayout(existing, new FormRow()));
	}

	@Test
	void addFormRowLayoutReturnsExistingComponent() {
		UIComponent existing = mock(UIComponent.class);
		UIComponent formLayout = mock(UIComponent.class);
		UIComponent rowLayout = mock(UIComponent.class);
		assertSame(existing, builder.addFormRowLayout(existing, formLayout, rowLayout));
	}

	// ── Creation tests (component == null → create new) ─────────────────────

	@Test
	void viewLayoutCreatesHtmlPanelGroup() {
		UIComponent result = builder.viewLayout(null);
		assertSame(mockHtmlPanelGroup, result);
	}

	@Test
	void tabLayoutCreatesHtmlPanelGroup() {
		UIComponent result = builder.tabLayout(null);
		assertSame(mockHtmlPanelGroup, result);
	}

	@Test
	void vboxLayoutCreatesHtmlPanelGroup() {
		UIComponent result = builder.vboxLayout(null, new VBox());
		assertSame(mockHtmlPanelGroup, result);
	}

	@Test
	void hboxLayoutCreatesHtmlPanelGroup() {
		UIComponent result = builder.hboxLayout(null, new HBox());
		assertSame(mockHtmlPanelGroup, result);
	}

	@Test
	void vboxLayoutSetsStyleClassForVerticalAlignment() {
		VBox vbox = new VBox();
		vbox.setVerticalAlignment(org.skyve.impl.metadata.view.VerticalAlignment.top);
		builder.vboxLayout(null, vbox);
		// Should set style class with alignment - just verify setStyleClass was called
		verify(mockHtmlPanelGroup).setStyleClass(anyString());
	}

	@Test
	void addToolbarsOrLayoutsAddsGridDivToView() {
		// Two HtmlPanelGroup mocks needed: one for the grid div, one for the toolbar
		HtmlPanelGroup gridDiv = mock(HtmlPanelGroup.class);
		List<UIComponent> gridDivChildren = new ArrayList<>();
		when(gridDiv.getChildren()).thenReturn(gridDivChildren);
		when(mockApplication.createComponent(HtmlPanelGroup.COMPONENT_TYPE)).thenReturn(gridDiv);

		UIComponent view = mock(UIComponent.class);
		List<UIComponent> viewChildren = new ArrayList<>();
		when(view.getChildren()).thenReturn(viewChildren);
		UIComponent toolbar = mock(UIComponent.class);
		List<UIComponent> toolbars = new ArrayList<>();
		toolbars.add(toolbar);
		builder.addToolbarsOrLayouts(view, toolbars);
		// The grid div should be added to the view at index 0
		assertEquals(1, viewChildren.size());
		assertSame(gridDiv, viewChildren.get(0));
		// The toolbar should be added inside the grid div
		assertTrue(gridDivChildren.contains(toolbar));
	}

	@Test
	void addToContainerAddsChildWrappedInDiv() {
		// The container needs children list; the created div (mockHtmlPanelGroup) gets child added to it
		UIComponent container = mock(UIComponent.class);
		List<UIComponent> containerChildren = new ArrayList<>();
		when(container.getChildren()).thenReturn(containerChildren);
		List<UIComponent> divChildren = new ArrayList<>();
		when(mockHtmlPanelGroup.getChildren()).thenReturn(divChildren);
		UIComponent child = mock(UIComponent.class);
		UIComponent result = builder.addToContainer(null, new VBox(), container, child, null, null, null, null, null, null, null, null);
		assertSame(child, result);
		// The div should be added to the container
		assertEquals(1, containerChildren.size());
		assertSame(mockHtmlPanelGroup, containerChildren.get(0));
		// The child should be added to the div
		assertTrue(divChildren.contains(child));
	}

	@Test
	void addedToContainerReturnsContainerGrandParent() {
		// addedToContainer: result = container.getParent().getParent()
		UIComponent grandParent = mock(UIComponent.class);
		UIComponent parent = mock(UIComponent.class);
		UIComponent container = mock(UIComponent.class);
		when(container.getParent()).thenReturn(parent);
		when(parent.getParent()).thenReturn(grandParent);
		UIComponent result = builder.addedToContainer(null, new VBox(), container);
		assertSame(grandParent, result);
	}

	@Test
	void formLayoutCreatesHtmlPanelGroupAndAddsResponsiveStyles() {
		Form form = new Form();
		form.getColumns().add(new FormColumn()); // at least one column
		UIComponent result = builder.formLayout(null, form);
		assertSame(mockHtmlPanelGroup, result);
		// addResponsiveStyles should have placed grid in viewRoot attributes
		assertNotNull(viewRootAttributes.get(org.skyve.impl.web.faces.FacesUtil.FORM_STYLES_KEY));
	}

	@Test
	void formRowLayoutCreatesHtmlPanelGroup() {
		UIComponent result = builder.formRowLayout(null, new FormRow());
		assertSame(mockHtmlPanelGroup, result);
	}

	@Test
	void addFormRowLayoutAddsGridToRowAndRowToFormAndReturnsGrid() {
		// addFormRowLayout creates a grid panelGroup, adds rowLayout to it, adds row to formLayout
		HtmlPanelGroup gridDiv = mock(HtmlPanelGroup.class);
		List<UIComponent> gridDivChildren = new ArrayList<>();
		when(gridDiv.getChildren()).thenReturn(gridDivChildren);
		when(mockApplication.createComponent(HtmlPanelGroup.COMPONENT_TYPE)).thenReturn(gridDiv);

		UIComponent formLayout = mock(UIComponent.class);
		List<UIComponent> formChildren = new ArrayList<>();
		when(formLayout.getChildren()).thenReturn(formChildren);
		UIComponent rowLayout = mock(UIComponent.class);
		List<UIComponent> rowChildren = new ArrayList<>();
		when(rowLayout.getChildren()).thenReturn(rowChildren);
		UIComponent result = builder.addFormRowLayout(null, formLayout, rowLayout);
		// Returns the grid div (inner component for children)
		assertSame(gridDiv, result);
		// Row should be added to formLayout
		assertTrue(formChildren.contains(rowLayout));
		// Grid should be added to rowLayout
		assertTrue(rowChildren.contains(gridDiv));
	}

	@Test
	void layoutFormItemLabelAddsLabelDivToRow() {
		UIComponent rowLayout = mock(UIComponent.class);
		List<UIComponent> rowChildren = new ArrayList<>();
		when(rowLayout.getChildren()).thenReturn(rowChildren);
		List<UIComponent> divChildren = new ArrayList<>();
		when(mockHtmlPanelGroup.getChildren()).thenReturn(divChildren);
		UIComponent widget = mock(UIComponent.class);
		when(widget.getId()).thenReturn("widget1");
		Form form = new Form();
		FormItem formItem = new FormItem();
		FormColumn formColumn = new FormColumn();
		builder.layoutFormItemLabel(rowLayout, widget, form, formItem, formColumn, "Label", true, null, true, null, null);
		// A div should be added to rowLayout
		assertEquals(1, rowChildren.size());
		assertSame(mockHtmlPanelGroup, rowChildren.get(0));
		// The label should be added to the div
		assertTrue(divChildren.contains(mockOutputLabel));
	}

	@Test
	void layoutFormItemLabelStoresRawLabelWithEscapedDefaultAndSeparateRequiredMarker() {
		UIComponent rowLayout = mock(UIComponent.class);
		when(rowLayout.getChildren()).thenReturn(new ArrayList<>());
		UIComponent widget = mock(UIComponent.class);
		when(widget.getId()).thenReturn("widget1");
		builder.layoutFormItemLabel(rowLayout, widget, new Form(), new FormItem(), new FormColumn(), UNSAFE_TEXT, true, "Required", false, null, null);
		verify(mockOutputLabel).setEscape(false);
		assertEquals(2, mockOutputLabel.getChildren().size());
		HtmlOutputText labelText = (HtmlOutputText) mockOutputLabel.getChildren().get(0);
		HtmlOutputText suffix = (HtmlOutputText) mockOutputLabel.getChildren().get(1);
		assertEquals(UNSAFE_TEXT, labelText.getValue());
		assertTrue(labelText.isEscape());
		assertEquals("&nbsp;*:", suffix.getValue());
		assertFalse(suffix.isEscape());
	}

	@Test
	void layoutFormItemWidgetAddsFlexDivToRow() {
		UIComponent rowLayout = mock(UIComponent.class);
		List<UIComponent> rowChildren = new ArrayList<>();
		when(rowLayout.getChildren()).thenReturn(rowChildren);
		List<UIComponent> flexChildren = new ArrayList<>();
		when(mockHtmlPanelGroup.getChildren()).thenReturn(flexChildren);
		UIComponent widget = mock(UIComponent.class);
		Form form = new Form();
		FormItem formItem = new FormItem();
		FormColumn formColumn = new FormColumn();
		builder.layoutFormItemWidget(rowLayout, widget, form, formItem, formColumn, "Label", true, 1, null, true, null, null, true, null, true, false);
		// A flex div should be added to rowLayout
		assertEquals(1, rowChildren.size());
		assertSame(mockHtmlPanelGroup, rowChildren.get(0));
		// Message and widget should be inside flex
		assertTrue(flexChildren.contains(mockMessage));
		assertTrue(flexChildren.contains(widget));
	}

	@Test
	void layoutFormItemWidgetWithTopLabelAddsFieldDiv() {
		// Two HtmlPanelGroup mocks needed for flex + fieldDiv
		HtmlPanelGroup flexDiv = mock(HtmlPanelGroup.class);
		List<UIComponent> flexChildren = new ArrayList<>();
		when(flexDiv.getChildren()).thenReturn(flexChildren);
		HtmlPanelGroup fieldDiv = mock(HtmlPanelGroup.class);
		List<UIComponent> fieldChildren = new ArrayList<>();
		when(fieldDiv.getChildren()).thenReturn(fieldChildren);
		HtmlPanelGroup floatSpan = mock(HtmlPanelGroup.class);
		List<UIComponent> floatChildren = new ArrayList<>();
		when(floatSpan.getChildren()).thenReturn(floatChildren);
		when(mockApplication.createComponent(HtmlPanelGroup.COMPONENT_TYPE))
			.thenReturn(flexDiv)
			.thenReturn(fieldDiv)
			.thenReturn(floatSpan);

		UIComponent rowLayout = mock(UIComponent.class);
		List<UIComponent> rowChildren = new ArrayList<>();
		when(rowLayout.getChildren()).thenReturn(rowChildren);
		UIComponent widget = mock(UIComponent.class);
		Form form = new Form();
		FormItem formItem = new FormItem();
		FormColumn formColumn = new FormColumn();
		builder.layoutFormItemWidget(rowLayout, widget, form, formItem, formColumn, "Label", true, 1, null, true, null, null, true, null, true, true);
		// The flex div should be added to rowLayout
		assertEquals(1, rowChildren.size());
		assertSame(flexDiv, rowChildren.get(0));
		// Message should be in flex
		assertTrue(flexChildren.contains(mockMessage));
		// fieldDiv should be in flex
		assertTrue(flexChildren.contains(fieldDiv));
		// widget should be in floatSpan
		assertTrue(floatChildren.contains(widget));
	}

	@Test
	void layoutFormItemWidgetStoresRawTopLabelWithEscapeFlagBeforeRequiredMarker() {
		HtmlPanelGroup flexDiv = mock(HtmlPanelGroup.class);
		when(flexDiv.getChildren()).thenReturn(new ArrayList<>());
		HtmlPanelGroup fieldDiv = mock(HtmlPanelGroup.class);
		when(fieldDiv.getChildren()).thenReturn(new ArrayList<>());
		HtmlPanelGroup floatSpan = mock(HtmlPanelGroup.class);
		when(floatSpan.getChildren()).thenReturn(new ArrayList<>());
		when(mockApplication.createComponent(HtmlPanelGroup.COMPONENT_TYPE))
			.thenReturn(flexDiv)
			.thenReturn(fieldDiv)
			.thenReturn(floatSpan);

		UIComponent rowLayout = mock(UIComponent.class);
		when(rowLayout.getChildren()).thenReturn(new ArrayList<>());
		UIComponent widget = mock(UIComponent.class);
		when(widget.getId()).thenReturn("widget1");
		when(mockOutputLabel.getChildren()).thenReturn(new ArrayList<>());
		builder.layoutFormItemWidget(rowLayout, widget, new Form(), new FormItem(), new FormColumn(), UNSAFE_TEXT, true, 1, "Required", false, null, null, true, null, true, true);
		verify(mockOutputLabel).setEscape(false);
		assertEquals(2, mockOutputLabel.getChildren().size());
		HtmlOutputText labelText = (HtmlOutputText) mockOutputLabel.getChildren().get(0);
		HtmlOutputText suffix = (HtmlOutputText) mockOutputLabel.getChildren().get(1);
		assertEquals(UNSAFE_TEXT, labelText.getValue());
		assertTrue(labelText.isEscape());
		assertEquals("&nbsp;*:", suffix.getValue());
		assertFalse(suffix.isEscape());
		verify(mockMessage).setEscape(false);
	}

	@Test
	void layoutFormItemWidgetEscapesHelpTooltipForAttributeContext() {
		UIComponent rowLayout = mock(UIComponent.class);
		when(rowLayout.getChildren()).thenReturn(new ArrayList<>());
		List<UIComponent> flexChildren = new ArrayList<>();
		when(mockHtmlPanelGroup.getChildren()).thenReturn(flexChildren);
		UIComponent widget = mock(UIComponent.class);

		builder.layoutFormItemWidget(rowLayout, widget, new Form(), new FormItem(), new FormColumn(), "Label", true, 1, null, true, null, UNSAFE_TEXT, true, null, true, false);

		HtmlOutputText help = null;
		for (UIComponent child : flexChildren) {
			if (child instanceof HtmlOutputText outputText) {
				help = outputText;
			}
		}
		assertNotNull(help);
		String value = (String) help.getValue();
		assertEquals(String.format("<i class=\"%s help\" data-tooltip=\"&amp;lt;img src=x onerror=alert(1)&amp;gt; &amp;amp; &amp;#34;quoted&amp;#34; &amp;#39;single&amp;#39;\"></i>", Icons.FONT_HELP), value);
		assertFalse(value.contains("data-tooltip=\"<"));
		assertFalse(value.contains("'single'"));
	}

	@Test
	void layoutFormItemWidgetLeavesTrustedHelpMarkupButEscapesAttributeDelimiters() {
		UIComponent rowLayout = mock(UIComponent.class);
		when(rowLayout.getChildren()).thenReturn(new ArrayList<>());
		List<UIComponent> flexChildren = new ArrayList<>();
		when(mockHtmlPanelGroup.getChildren()).thenReturn(flexChildren);
		UIComponent widget = mock(UIComponent.class);

		builder.layoutFormItemWidget(rowLayout, widget, new Form(), new FormItem(), new FormColumn(), "Label", true, 1, null, true, null, UNSAFE_TEXT, false, null, true, false);

		HtmlOutputText help = null;
		for (UIComponent child : flexChildren) {
			if (child instanceof HtmlOutputText outputText) {
				help = outputText;
			}
		}
		assertNotNull(help);
		String value = (String) help.getValue();
		assertEquals(String.format("<i class=\"%s help\" data-tooltip=\"&lt;img src=x onerror=alert(1)> &amp; &#34;quoted&#34; &#39;single&#39;\"></i>", Icons.FONT_HELP), value);
		assertFalse(value.contains("\"quoted\""));
		assertFalse(value.contains("'single'"));
	}

	@Test
	void addToContainerHBoxWithUnsizedColumns() {
		// HBox viewContainer with no widths provided: should auto-compute percentageWidth
		UIComponent container = mock(UIComponent.class);
		List<UIComponent> containerChildren = new ArrayList<>();
		when(container.getChildren()).thenReturn(containerChildren);
		List<UIComponent> divChildren = new ArrayList<>();
		when(mockHtmlPanelGroup.getChildren()).thenReturn(divChildren);
		UIComponent child = mock(UIComponent.class);

		HBox hbox = new HBox();
		// Add two unsized contained items (neither AbsoluteWidth nor Inject)
		org.skyve.impl.metadata.view.widget.Spacer spacer1 = new org.skyve.impl.metadata.view.widget.Spacer();
		org.skyve.impl.metadata.view.widget.Spacer spacer2 = new org.skyve.impl.metadata.view.widget.Spacer();
		// Spacer doesn't implement AbsoluteWidth, so both are unsized
		hbox.getContained().add(spacer1);
		hbox.getContained().add(spacer2);

		UIComponent result = builder.addToContainer(null, hbox, container, child,
			null, null, null, null, null, null, null, null);
		assertSame(child, result);
		assertEquals(1, containerChildren.size());
	}

	@Test
	void addToContainerHBoxWithPixelWidth() {
		// When pixelWidth is not null, HBox branch should be skipped
		UIComponent container = mock(UIComponent.class);
		List<UIComponent> containerChildren = new ArrayList<>();
		when(container.getChildren()).thenReturn(containerChildren);
		List<UIComponent> divChildren = new ArrayList<>();
		when(mockHtmlPanelGroup.getChildren()).thenReturn(divChildren);
		UIComponent child = mock(UIComponent.class);

		HBox hbox = new HBox();
		UIComponent result = builder.addToContainer(null, hbox, container, child,
			Integer.valueOf(200), null, null, null, null, null, null, null);
		assertSame(child, result);
		assertEquals(1, containerChildren.size());
	}

	@Test
	void vboxLayoutWithPrimeFlexTopAlignmentSetsStyleClass() {
		try {
			org.skyve.impl.util.UtilImpl.PRIMEFLEX = true;
			VBox vbox = new VBox();
			vbox.setVerticalAlignment(org.skyve.impl.metadata.view.VerticalAlignment.top);
			vbox.setHorizontalAlignment(org.skyve.impl.metadata.view.HorizontalAlignment.centre);
			builder.vboxLayout(null, vbox);
			verify(mockHtmlPanelGroup).setStyleClass(org.mockito.ArgumentMatchers.contains("p-grid"));
		}
		finally {
			org.skyve.impl.util.UtilImpl.PRIMEFLEX = false;
		}
	}

	@Test
	void vboxLayoutWithPrimeFlexMiddleAlignmentSetsStyleClass() {
		try {
			org.skyve.impl.util.UtilImpl.PRIMEFLEX = true;
			VBox vbox = new VBox();
			vbox.setVerticalAlignment(org.skyve.impl.metadata.view.VerticalAlignment.middle);
			vbox.setHorizontalAlignment(org.skyve.impl.metadata.view.HorizontalAlignment.right);
			builder.vboxLayout(null, vbox);
			verify(mockHtmlPanelGroup).setStyleClass(org.mockito.ArgumentMatchers.contains("p-align-center"));
		}
		finally {
			org.skyve.impl.util.UtilImpl.PRIMEFLEX = false;
		}
	}

	@Test
	void vboxLayoutWithPrimeFlexBottomAlignmentSetsStyleClass() {
		try {
			org.skyve.impl.util.UtilImpl.PRIMEFLEX = true;
			VBox vbox = new VBox();
			vbox.setVerticalAlignment(org.skyve.impl.metadata.view.VerticalAlignment.bottom);
			builder.vboxLayout(null, vbox);
			verify(mockHtmlPanelGroup).setStyleClass(org.mockito.ArgumentMatchers.contains("p-align-end"));
		}
		finally {
			org.skyve.impl.util.UtilImpl.PRIMEFLEX = false;
		}
	}

	@Test
	void layoutFormItemWidgetWithColspanGreaterThanOneUsesColspanExpression() {
		UIComponent rowLayout = mock(UIComponent.class);
		List<UIComponent> rowChildren = new ArrayList<>();
		when(rowLayout.getChildren()).thenReturn(rowChildren);
		List<UIComponent> flexChildren = new ArrayList<>();
		when(mockHtmlPanelGroup.getChildren()).thenReturn(flexChildren);
		UIComponent widget = mock(UIComponent.class);
		Form form = new Form();
		FormItem formItem = new FormItem();
		FormColumn formColumn = new FormColumn();
		// colspan = 2 triggers the else (colspan > 1) branch
		builder.layoutFormItemWidget(rowLayout, widget, form, formItem, formColumn, "Label", true, 2, null, true, null, null, true, null, true, false);
		// A flex div should be added to rowLayout
		assertEquals(1, rowChildren.size());
		assertSame(mockHtmlPanelGroup, rowChildren.get(0));
	}
}
