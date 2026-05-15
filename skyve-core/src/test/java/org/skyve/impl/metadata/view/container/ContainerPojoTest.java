package org.skyve.impl.metadata.view.container;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.startsWith;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.ShrinkWrap;
import org.skyve.impl.metadata.view.VerticalAlignment;
import org.skyve.impl.metadata.view.container.Collapsible;

/**
 * Tests for container POJO getter/setter methods in HBox, VBox, and Sidebar.
 */
@SuppressWarnings("static-method")
class ContainerPojoTest {

	// ---- HBox ----

	@Test
	void hboxSetWidgetIdRoundtrip() {
		HBox box = new HBox();
		box.setWidgetId("myHBox");
		assertThat(box.getWidgetId(), is("myHBox"));
	}

	@Test
	void hboxBlankWidgetIdBecomesNull() {
		HBox box = new HBox();
		box.setWidgetId("  ");
		assertNull(box.getWidgetId());
	}

	@Test
	void hboxSetVisibleConditionNameNegatesCondition() {
		HBox box = new HBox();
		box.setVisibleConditionName("isVisible");
		assertThat(box.getInvisibleConditionName(), startsWith("not"));
	}

	@Test
	void hboxSetInvisibleConditionNameRoundtrip() {
		HBox box = new HBox();
		box.setInvisibleConditionName("isHidden");
		assertThat(box.getInvisibleConditionName(), is("isHidden"));
	}

	@Test
	void hboxSetCollapsibleRoundtrip() {
		HBox box = new HBox();
		box.setCollapsible(Collapsible.open);
		assertThat(box.getCollapsible(), is(Collapsible.open));
	}

	@Test
	void hboxSetVerticalAlignmentRoundtrip() {
		HBox box = new HBox();
		box.setVerticalAlignment(VerticalAlignment.top);
		assertThat(box.getVerticalAlignment(), is(VerticalAlignment.top));
	}

	@Test
	void hboxSetHorizontalAlignmentRoundtrip() {
		HBox box = new HBox();
		box.setHorizontalAlignment(HorizontalAlignment.centre);
		assertThat(box.getHorizontalAlignment(), is(HorizontalAlignment.centre));
	}

	@Test
	void hboxSetShrinkWrapRoundtrip() {
		HBox box = new HBox();
		box.setShrinkWrap(ShrinkWrap.width);
		assertThat(box.getShrinkWrap(), is(ShrinkWrap.width));
	}

	@Test
	void hboxGetPropertiesNotNull() {
		assertNotNull(new HBox().getProperties());
	}

	// ---- VBox ----

	@Test
	void vboxSetWidgetIdRoundtrip() {
		VBox box = new VBox();
		box.setWidgetId("myVBox");
		assertThat(box.getWidgetId(), is("myVBox"));
	}

	@Test
	void vboxBlankWidgetIdBecomesNull() {
		VBox box = new VBox();
		box.setWidgetId("  ");
		assertNull(box.getWidgetId());
	}

	@Test
	void vboxSetVisibleConditionNameNegatesCondition() {
		VBox box = new VBox();
		box.setVisibleConditionName("isVisible");
		assertThat(box.getInvisibleConditionName(), startsWith("not"));
	}

	@Test
	void vboxSetInvisibleConditionNameRoundtrip() {
		VBox box = new VBox();
		box.setInvisibleConditionName("hiddenWhen");
		assertThat(box.getInvisibleConditionName(), is("hiddenWhen"));
	}

	@Test
	void vboxSetCollapsibleRoundtrip() {
		VBox box = new VBox();
		box.setCollapsible(Collapsible.closed);
		assertThat(box.getCollapsible(), is(Collapsible.closed));
	}

	@Test
	void vboxSetShrinkWrapRoundtrip() {
		VBox box = new VBox();
		box.setShrinkWrap(ShrinkWrap.both);
		assertThat(box.getShrinkWrap(), is(ShrinkWrap.both));
	}

	@Test
	void vboxGetPropertiesNotNull() {
		assertNotNull(new VBox().getProperties());
	}

	// ---- Sidebar ----

	@Test
	void sidebarSetWidgetIdRoundtrip() {
		Sidebar sidebar = new Sidebar();
		sidebar.setWidgetId("sideBar1");
		assertThat(sidebar.getWidgetId(), is("sideBar1"));
	}

	@Test
	void sidebarBlankWidgetIdBecomesNull() {
		Sidebar sidebar = new Sidebar();
		sidebar.setWidgetId("  ");
		assertNull(sidebar.getWidgetId());
	}

	@Test
	void sidebarSetInvisibleConditionNameRoundtrip() {
		Sidebar sidebar = new Sidebar();
		sidebar.setInvisibleConditionName("hiddenWhen");
		assertThat(sidebar.getInvisibleConditionName(), is("hiddenWhen"));
	}

	@Test
	void sidebarBlankInvisibleConditionNameBecomesNull() {
		Sidebar sidebar = new Sidebar();
		sidebar.setInvisibleConditionName("  ");
		assertNull(sidebar.getInvisibleConditionName());
	}

	@Test
	void sidebarSetVisibleConditionNameNegatesCondition() {
		Sidebar sidebar = new Sidebar();
		sidebar.setVisibleConditionName("isVisible");
		assertThat(sidebar.getInvisibleConditionName(), startsWith("not"));
	}

	@Test
	void sidebarSetFloatingPixelWidthRoundtrip() {
		Sidebar sidebar = new Sidebar();
		sidebar.setFloatingPixelWidth(Integer.valueOf(250));
		assertThat(sidebar.getFloatingPixelWidth(), is(Integer.valueOf(250)));
	}

	@Test
	void sidebarSetFloatingPixelWidthBreakpointRoundtrip() {
		Sidebar sidebar = new Sidebar();
		sidebar.setFloatingPixelWidthBreakpoint(Integer.valueOf(768));
		assertThat(sidebar.getFloatingPixelWidthBreakpoint(), is(Integer.valueOf(768)));
	}

	@Test
	void sidebarGetPropertiesNotNull() {
		assertNotNull(new Sidebar().getProperties());
	}

	// ---- Tab ----

	@Test
	void tabSetTitleRoundtrip() {
		Tab tab = new Tab();
		tab.setTitle("My Tab");
		assertThat(tab.getTitle(), is("My Tab"));
	}

	@Test
	void tabBlankTitleBecomesNull() {
		Tab tab = new Tab();
		tab.setTitle("  ");
		assertNull(tab.getTitle());
	}

	@Test
	void tabGetLocalisedTitleNullWhenTitleNull() {
		Tab tab = new Tab();
		assertThat(tab.getLocalisedTitle(), is(nullValue()));
	}

	@Test
	void tabGetLocalisedTitleNonNullWhenTitleSet() {
		Tab tab = new Tab();
		tab.setTitle("Details");
		assertThat(tab.getLocalisedTitle(), notNullValue());
	}

	@Test
	void tabSetIcon16x16RoundTrip() {
		Tab tab = new Tab();
		tab.setIcon16x16RelativeFileName("icons/tab.png");
		assertThat(tab.getIcon16x16RelativeFileName(), is("icons/tab.png"));
	}

	@Test
	void tabSetIconStyleClassRoundtrip() {
		Tab tab = new Tab();
		tab.setIconStyleClass("fa-folder");
		assertThat(tab.getIconStyleClass(), is("fa-folder"));
	}

	@Test
	void tabSetDisabledConditionNameRoundtrip() {
		Tab tab = new Tab();
		tab.setDisabledConditionName("isDisabled");
		assertThat(tab.getDisabledConditionName(), is("isDisabled"));
	}

	@Test
	void tabSetEnabledConditionNameNegatesCondition() {
		Tab tab = new Tab();
		tab.setEnabledConditionName("isEnabled");
		assertThat(tab.getDisabledConditionName(), startsWith("not"));
	}

	@Test
	void tabSetVisibleConditionNameNegatesCondition() {
		Tab tab = new Tab();
		tab.setVisibleConditionName("isVisible");
		assertThat(tab.getInvisibleConditionName(), startsWith("not"));
	}

	@Test
	void tabSetInvisibleConditionNameRoundtrip() {
		Tab tab = new Tab();
		tab.setInvisibleConditionName("hiddenWhen");
		assertThat(tab.getInvisibleConditionName(), is("hiddenWhen"));
	}

	@Test
	void tabGetPropertiesNotNull() {
		assertNotNull(new Tab().getProperties());
	}
}
