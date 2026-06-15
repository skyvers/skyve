package org.skyve.metadata.view.fluent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.view.access.ViewUserAccessesMetaData;

/**
 * Tests for {@link FluentView} setter methods, parameters, sidebar, actions,
 * and the wrapping constructor. Access-related methods are tested in
 * {@code FluentViewTest}.
 */
@SuppressWarnings("static-method")
class FluentViewPropertiesTest {

	@Test
	void defaultConstructorCreatesInstance() {
		FluentView v = new FluentView();
		assertNotNull(v.get());
	}

	@Test
	void nameSetsValue() {
		FluentView v = new FluentView().name("edit");
		assertEquals("edit", v.get().getName());
	}

	@Test
	void titleSetsValue() {
		FluentView v = new FluentView().title("My Title");
		assertEquals("My Title", v.get().getTitle());
	}

	@Test
	void iconStyleClassSetsValue() {
		FluentView v = new FluentView().iconStyleClass("fa fa-edit");
		assertEquals("fa fa-edit", v.get().getIconStyleClass());
	}

	@Test
	void icon32x32RelativeFileNameSetsValue() {
		FluentView v = new FluentView().icon32x32RelativeFileName("icons/edit.png");
		assertEquals("icons/edit.png", v.get().getIcon32x32RelativeFileName());
	}

	@Test
	void documentationSetsValue() {
		FluentView v = new FluentView().documentation("View docs");
		assertEquals("View docs", v.get().getDocumentation());
	}

	@Test
	void helpRelativeFileNameSetsValue() {
		FluentView v = new FluentView().helpRelativeFileName("help/edit.html");
		assertEquals("help/edit.html", v.get().getHelpRelativeFileName());
	}

	@Test
	void helpURLSetsValue() {
		FluentView v = new FluentView().helpURL("https://example.com/help");
		assertEquals("https://example.com/help", v.get().getHelpURL());
	}

	@Test
	void refreshTimeInSecondsSetsValue() {
		FluentView v = new FluentView().refreshTimeInSeconds(30);
		assertEquals(Integer.valueOf(30), v.get().getRefreshTimeInSeconds());
	}

	@Test
	void refreshConditionNameSetsValue() {
		FluentView v = new FluentView().refreshConditionName("shouldRefresh");
		assertEquals("shouldRefresh", v.get().getRefreshConditionName());
	}

	@Test
	void refreshActionNameSetsValue() {
		FluentView v = new FluentView().refreshActionName("RefreshAction");
		assertEquals("RefreshAction", v.get().getRefreshActionName());
	}

	@Test
	void generateAccessesTrueSetsFlag() {
		FluentView v = new FluentView();
		v.get().setAccesses(new ViewUserAccessesMetaData());
		v.generateAccesses(true);
		assertTrue(v.get().getAccesses().isGenerate());
	}

	@Test
	void generateAccessesFalseClearsFlag() {
		FluentView v = new FluentView();
		v.get().setAccesses(new ViewUserAccessesMetaData());
		v.generateAccesses(false);
		assertFalse(v.get().getAccesses().isGenerate());
	}

	@Test
	void sidebarSetsSidebar() {
		FluentView v = new FluentView().sidebar(new FluentSidebar().widgetId("side1"));
		assertNotNull(v.get().getSidebar());
	}

	@Test
	void actionsSetsActions() {
		FluentView v = new FluentView().actions(new FluentActions().widgetId("act1"));
		assertNotNull(v.get().getActions());
		assertEquals("act1", v.get().getActions().getWidgetId());
	}

	@Test
	void addParameterAddsEntry() {
		FluentView v = new FluentView();
		v.addParameter(new FluentViewParameter().fromBinding("fromB").boundTo("toB"));
		assertEquals(1, v.get().getParameters().size());
	}

	@Test
	void findParameterReturnsMatch() {
		FluentView v = new FluentView();
		v.addParameter(new FluentViewParameter().fromBinding("f1").boundTo("t1"));
		assertNotNull(v.findParameter("f1"));
	}

	@Test
	void findParameterReturnsNullWhenMissing() {
		FluentView v = new FluentView();
		assertNull(v.findParameter("missing"));
	}

	@Test
	void removeParameterRemovesEntry() {
		FluentView v = new FluentView();
		v.addParameter(new FluentViewParameter().fromBinding("f2").boundTo("t2"));
		assertEquals(1, v.get().getParameters().size());
		v.removeParameter("f2");
		assertEquals(0, v.get().getParameters().size());
	}

	@Test
	void clearParametersRemovesAll() {
		FluentView v = new FluentView();
		v.addParameter(new FluentViewParameter().fromBinding("a").boundTo("b"));
		v.addParameter(new FluentViewParameter().fromBinding("c").boundTo("d"));
		v.clearParameters();
		assertEquals(0, v.get().getParameters().size());
	}
}
