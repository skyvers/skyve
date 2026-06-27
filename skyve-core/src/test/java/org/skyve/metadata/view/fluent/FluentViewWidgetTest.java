package org.skyve.metadata.view.fluent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.widget.bound.input.CompleteType;
import org.skyve.impl.metadata.view.widget.bound.input.KeyboardType;
import org.skyve.metadata.view.Action.ActionShow;
import org.skyve.metadata.view.TextOutput.Sanitisation;

@SuppressWarnings("static-method")
class FluentViewWidgetTest {

	// ---- FluentTab ----------------------------------------------------------

	@Test
	void fluentTabDefaultConstructorCreatesInstance() {
		assertNotNull(new FluentTab().get());
	}

	@Test
	void fluentTabTitleSetsValue() {
		assertEquals("My Tab", new FluentTab().title("My Tab").get().getTitle());
	}

	@Test
	void fluentTabIcon16x16SetsValue() {
		assertEquals("img/t.png", new FluentTab().icon16x16RelativeFileName("img/t.png").get().getIcon16x16RelativeFileName());
	}

	@Test
	void fluentTabIconStyleClassSetsValue() {
		assertEquals("fa fa-tab", new FluentTab().iconStyleClass("fa fa-tab").get().getIconStyleClass());
	}

	@Test
	void fluentTabDisabledConditionSetsValue() {
		assertEquals("notReady", new FluentTab().disabledConditionName("notReady").get().getDisabledConditionName());
	}

	@Test
	void fluentTabInvisibleConditionSetsValue() {
		assertEquals("hidden", new FluentTab().invisibleConditionName("hidden").get().getInvisibleConditionName());
	}

	// ---- FluentLabel --------------------------------------------------------

	@Test
	void fluentLabelDefaultConstructorCreatesInstance() {
		assertNotNull(new FluentLabel().get());
	}

	@Test
	void fluentLabelValueSetsValue() {
		assertEquals("Hello", new FluentLabel().value("Hello").get().getValue());
	}

	@Test
	void fluentLabelForBindingSetsValue() {
		assertEquals("name", new FluentLabel().forBinding("name").get().getFor());
	}

	@Test
	void fluentLabelInvisibleConditionSetsValue() {
		assertEquals("cond", new FluentLabel().invisibleConditionName("cond").get().getInvisibleConditionName());
	}

	@Test
	void fluentLabelFormattedTrueSetsTrue() {
		assertEquals(Boolean.TRUE, new FluentLabel().formatted(true).get().getFormatted());
	}

	@Test
	void fluentLabelFormattedFalseSetsFalse() {
		assertNotEquals(Boolean.TRUE, new FluentLabel().formatted(false).get().getFormatted());
	}

	@Test
	void fluentLabelSanitiseSetsValue() {
		assertEquals(Sanitisation.basic, new FluentLabel().sanitise(Sanitisation.basic).get().getSanitise());
	}

	@Test
	void fluentLabelTextAlignmentSetsValue() {
		assertEquals(HorizontalAlignment.right,
				new FluentLabel().textAlignment(HorizontalAlignment.right).get().getTextAlignment());
	}

	@Test
	void fluentLabelEscapeTrueSetsTrue() {
		assertEquals(Boolean.TRUE, new FluentLabel().escape(true).get().getEscape());
	}

	// ---- FluentButton -------------------------------------------------------

	@Test
	void fluentButtonDefaultConstructorCreatesInstance() {
		assertNotNull(new FluentButton().get());
	}

	@Test
	void fluentButtonActionNameSetsValue() {
		assertEquals("Save", new FluentButton().actionName("Save").get().getActionName());
	}

	@Test
	void fluentButtonPixelWidthSetsValue() {
		assertEquals(120, new FluentButton().pixelWidth(120).get().getPixelWidth().intValue());
	}

	@Test
	void fluentButtonPixelHeightSetsValue() {
		assertEquals(40, new FluentButton().pixelHeight(40).get().getPixelHeight().intValue());
	}

	@Test
	void fluentButtonMinPixelHeightSetsValue() {
		assertEquals(20, new FluentButton().minPixelHeight(20).get().getMinPixelHeight().intValue());
	}

	@Test
	void fluentButtonMaxPixelHeightSetsValue() {
		assertEquals(60, new FluentButton().maxPixelHeight(60).get().getMaxPixelHeight().intValue());
	}

	@Test
	void fluentButtonShowSetsValue() {
		assertEquals(ActionShow.icon, new FluentButton().show(ActionShow.icon).get().getShow());
	}

	// ---- FluentTextField ----------------------------------------------------

	@Test
	void fluentTextFieldDefaultConstructorCreatesInstance() {
		assertNotNull(new FluentTextField().get());
	}

	@Test
	void fluentTextFieldEditableTrueSetsTrue() {
		assertEquals(Boolean.TRUE, new FluentTextField().editable(true).get().getEditable());
	}

	@Test
	void fluentTextFieldEditableFalseSetsFalse() {
		assertNotEquals(Boolean.TRUE, new FluentTextField().editable(false).get().getEditable());
	}

	@Test
	void fluentTextFieldKeyboardTypeSetsValue() {
		assertEquals(KeyboardType.email, new FluentTextField().keyboardType(KeyboardType.email).get().getKeyboardType());
	}

	@Test
	void fluentTextFieldCompleteSetsValue() {
		assertEquals(CompleteType.previous, new FluentTextField().complete(CompleteType.previous).get().getComplete());
	}

	@Test
	void fluentTextFieldPixelWidthSetsValue() {
		assertEquals(200, new FluentTextField().pixelWidth(200).get().getPixelWidth().intValue());
	}

	// ---- FluentView ---------------------------------------------------------

	@Test
	void fluentViewDefaultConstructorCreatesInstance() {
		assertNotNull(new FluentView().get());
	}

	@Test
	void fluentViewNameSetsValue() {
		assertEquals("edit", new FluentView().name("edit").get().getName());
	}

	@Test
	void fluentViewTitleSetsValue() {
		assertEquals("Edit User", new FluentView().title("Edit User").get().getTitle());
	}

	@Test
	void fluentViewIconStyleClassSetsValue() {
		assertEquals("fa fa-user", new FluentView().iconStyleClass("fa fa-user").get().getIconStyleClass());
	}

	@Test
	void fluentViewIcon32x32SetsValue() {
		assertEquals("img/32.png", new FluentView().icon32x32RelativeFileName("img/32.png").get().getIcon32x32RelativeFileName());
	}

	@Test
	void fluentViewDocumentationSetsValue() {
		assertEquals("doc text", new FluentView().documentation("doc text").get().getDocumentation());
	}

	@Test
	void fluentViewHelpRelativeFileNameSetsValue() {
		assertEquals("help/view.html", new FluentView().helpRelativeFileName("help/view.html").get().getHelpRelativeFileName());
	}

	@Test
	void fluentViewHelpURLSetsValue() {
		assertEquals("https://example.com/help", new FluentView().helpURL("https://example.com/help").get().getHelpURL());
	}

	@Test
	void fluentViewRefreshTimeInSecondsSetsValue() {
		assertEquals(30, new FluentView().refreshTimeInSeconds(30).get().getRefreshTimeInSeconds().intValue());
	}

	@Test
	void fluentViewRefreshConditionNameSetsValue() {
		assertEquals("needsRefresh", new FluentView().refreshConditionName("needsRefresh").get().getRefreshConditionName());
	}

	@Test
	void fluentViewRefreshActionNameSetsValue() {
		assertEquals("Refresh", new FluentView().refreshActionName("Refresh").get().getRefreshActionName());
	}

	// ---- FluentSpacer -------------------------------------------------------

	@Test
	void fluentSpacerDefaultConstructorCreatesInstance() {
		assertNotNull(new FluentSpacer().get());
	}

	@Test
	void fluentSpacerInvisibleConditionSetsValue() {
		assertEquals("hidden", new FluentSpacer().invisibleConditionName("hidden").get().getInvisibleConditionName());
	}

	// ---- FluentTabPane -------------------------------------------------------

	@Test
	void fluentTabPaneDefaultConstructorCreatesInstance() {
		assertNotNull(new FluentTabPane().get());
	}

	@Test
	void fluentTabPaneAddTabAddsTab() {
		FluentTabPane pane = new FluentTabPane().addTab(new FluentTab().title("Tab 1"));
		assertFalse(pane.get().getTabs().isEmpty());
	}

	@Test
	void fluentTabPaneGetTabByIndex() {
		FluentTabPane pane = new FluentTabPane().addTab(new FluentTab().title("Users"));
		assertNotNull(pane.getTab(0));
		assertEquals("Users", pane.getTab(0).get().getTitle());
	}

	@Test
	void fluentTabPaneRemoveTabByIndex() {
		FluentTabPane pane = new FluentTabPane()
				.addTab(new FluentTab().title("Tab 1"));
		pane.removeTab(0);
		assertTrue(pane.get().getTabs().isEmpty());
	}

	@Test
	void fluentTabPaneClearTabsEmptiesAll() {
		FluentTabPane pane = new FluentTabPane()
				.addTab(new FluentTab().title("A"))
				.addTab(new FluentTab().title("B"));
		pane.clearTabs();
		assertTrue(pane.get().getTabs().isEmpty());
	}

	// ---- FluentForm ---------------------------------------------------------

	@Test
	void fluentFormDefaultConstructorCreatesInstance() {
		assertNotNull(new FluentForm().get());
	}

	@Test
	void fluentFormWidgetIdSetsValue() {
		assertEquals("frm1", new FluentForm().widgetId("frm1").get().getWidgetId());
	}

	@Test
	void fluentFormBorderTrueSetsTrue() {
		assertEquals(Boolean.TRUE, new FluentForm().border(true).get().getBorder());
	}

	@Test
	void fluentFormBorderTitleSetsValue() {
		assertEquals("Details", new FluentForm().borderTitle("Details").get().getBorderTitle());
	}

	@Test
	void fluentFormAddRowAddsRow() {
		FluentForm form = new FluentForm().addRow(new FluentFormRow());
		assertFalse(form.get().getRows().isEmpty());
	}
}
