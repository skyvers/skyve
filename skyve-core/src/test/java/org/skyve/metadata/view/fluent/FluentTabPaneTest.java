package org.skyve.metadata.view.fluent;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.container.TabPane;

@SuppressWarnings("static-method")
class FluentTabPaneTest {

	@Test
	void defaultConstructorCreatesPane() {
		assertThat(new FluentTabPane().get(), is(notNullValue()));
	}

	@Test
	void wrappingConstructorUsesPane() {
		TabPane tp = new TabPane();
		assertEquals(tp, new FluentTabPane(tp).get());
	}

	@Test
	void widgetIdSetsValue() {
		assertThat(new FluentTabPane().widgetId("wid").get().getWidgetId(), is("wid"));
	}

	@Test
	void pixelWidthSetsValue() {
		assertEquals(400, new FluentTabPane().pixelWidth(400).get().getPixelWidth().intValue());
	}

	@Test
	void responsiveWidthSetsValue() {
		assertEquals(6, new FluentTabPane().responsiveWidth(6).get().getResponsiveWidth().intValue());
	}

	@Test
	void smSetsValue() {
		assertEquals(3, new FluentTabPane().sm(3).get().getSm().intValue());
	}

	@Test
	void mdSetsValue() {
		assertEquals(6, new FluentTabPane().md(6).get().getMd().intValue());
	}

	@Test
	void lgSetsValue() {
		assertEquals(9, new FluentTabPane().lg(9).get().getLg().intValue());
	}

	@Test
	void xlSetsValue() {
		assertEquals(12, new FluentTabPane().xl(12).get().getXl().intValue());
	}

	@Test
	void percentageWidthSetsValue() {
		assertEquals(50, new FluentTabPane().percentageWidth(50).get().getPercentageWidth().intValue());
	}

	@Test
	void minPixelWidthSetsValue() {
		assertEquals(100, new FluentTabPane().minPixelWidth(100).get().getMinPixelWidth().intValue());
	}

	@Test
	void maxPixelWidthSetsValue() {
		assertEquals(800, new FluentTabPane().maxPixelWidth(800).get().getMaxPixelWidth().intValue());
	}

	@Test
	void pixelHeightSetsValue() {
		assertEquals(300, new FluentTabPane().pixelHeight(300).get().getPixelHeight().intValue());
	}

	@Test
	void percentageHeightSetsValue() {
		assertEquals(75, new FluentTabPane().percentageHeight(75).get().getPercentageHeight().intValue());
	}

	@Test
	void minPixelHeightSetsValue() {
		assertEquals(50, new FluentTabPane().minPixelHeight(50).get().getMinPixelHeight().intValue());
	}

	@Test
	void maxPixelHeightSetsValue() {
		assertEquals(600, new FluentTabPane().maxPixelHeight(600).get().getMaxPixelHeight().intValue());
	}

	@Test
	void disabledConditionNameSetsValue() {
		assertThat(new FluentTabPane().disabledConditionName("disabled").get().getDisabledConditionName(), is("disabled"));
	}

	@Test
	void invisibleConditionNameSetsValue() {
		assertThat(new FluentTabPane().invisibleConditionName("invisible").get().getInvisibleConditionName(), is("invisible"));
	}

	@Test
	void selectedTabIndexBindingSetsValue() {
		assertThat(new FluentTabPane().selectedTabIndexBinding("idx").get().getSelectedTabIndexBinding(), is("idx"));
	}

	@Test
	void addTabAddsTab() {
		FluentTabPane pane = new FluentTabPane().addTab(new FluentTab());
		assertEquals(1, pane.get().getTabs().size());
	}

	@Test
	void addTabAtIndexInserts() {
		FluentTabPane pane = new FluentTabPane()
				.addTab(new FluentTab())
				.addTab(0, new FluentTab());
		assertEquals(2, pane.get().getTabs().size());
	}

	@Test
	void getTabReturnsTab() {
		FluentTabPane pane = new FluentTabPane().addTab(new FluentTab());
		assertThat(pane.getTab(0), is(notNullValue()));
	}

	@Test
	void removeTabRemovesTab() {
		FluentTabPane pane = new FluentTabPane().addTab(new FluentTab()).removeTab(0);
		assertEquals(0, pane.get().getTabs().size());
	}

	@Test
	void clearTabsRemovesAll() {
		FluentTabPane pane = new FluentTabPane()
				.addTab(new FluentTab())
				.addTab(new FluentTab())
				.clearTabs();
		assertEquals(0, pane.get().getTabs().size());
	}

	@Test
	void fromCopiesWidgetId() {
		TabPane src = new TabPane();
		src.setWidgetId("wid");
		assertThat(new FluentTabPane().from(src).get().getWidgetId(), is("wid"));
	}

	@Test
	void fromCopiesDisabledConditionName() {
		TabPane src = new TabPane();
		src.setDisabledConditionName("disabled");
		assertThat(new FluentTabPane().from(src).get().getDisabledConditionName(), is("disabled"));
	}

	@Test
	void fromCopiesSelectedTabIndexBinding() {
		TabPane src = new TabPane();
		src.setSelectedTabIndexBinding("idx");
		assertThat(new FluentTabPane().from(src).get().getSelectedTabIndexBinding(), is("idx"));
	}

	@Test
	void fromCopiesTabs() {
		TabPane src = new TabPane();
		src.getTabs().add(new FluentTab().get());
		assertEquals(1, new FluentTabPane().from(src).get().getTabs().size());
	}
}
