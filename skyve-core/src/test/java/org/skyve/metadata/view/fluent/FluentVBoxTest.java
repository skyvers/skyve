package org.skyve.metadata.view.fluent;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.ShrinkWrap;
import org.skyve.impl.metadata.view.VerticalAlignment;
import org.skyve.impl.metadata.view.container.Collapsible;
import org.skyve.impl.metadata.view.container.VBox;

@SuppressWarnings("static-method")
class FluentVBoxTest {

	@Test
	void defaultConstructorCreatesBox() {
		assertThat(new FluentVBox().get(), is(notNullValue()));
	}

	@Test
	void wrappingConstructorUsesBox() {
		VBox vb = new VBox();
		assertEquals(vb, new FluentVBox(vb).get());
	}

	@Test
	void widgetIdSetsValue() {
		assertThat(new FluentVBox().widgetId("wid").get().getWidgetId(), is("wid"));
	}

	@Test
	void borderSetsTrue() {
		assertThat(new FluentVBox().border(true).get().getBorder(), is(Boolean.TRUE));
	}

	@Test
	void borderSetsFalse() {
		assertThat(new FluentVBox().border(false).get().getBorder(), is(Boolean.FALSE));
	}

	@Test
	void borderTitleSetsValue() {
		assertThat(new FluentVBox().borderTitle("Box Title").get().getBorderTitle(), is("Box Title"));
	}

	@Test
	void collapsibleSetsValue() {
		assertThat(new FluentVBox().collapsible(Collapsible.closed).get().getCollapsible(), is(Collapsible.closed));
	}

	@Test
	void pixelWidthSetsValue() {
		assertEquals(300, new FluentVBox().pixelWidth(300).get().getPixelWidth().intValue());
	}

	@Test
	void responsiveWidthSetsValue() {
		assertEquals(6, new FluentVBox().responsiveWidth(6).get().getResponsiveWidth().intValue());
	}

	@Test
	void smSetsValue() {
		assertEquals(3, new FluentVBox().sm(3).get().getSm().intValue());
	}

	@Test
	void mdSetsValue() {
		assertEquals(6, new FluentVBox().md(6).get().getMd().intValue());
	}

	@Test
	void lgSetsValue() {
		assertEquals(9, new FluentVBox().lg(9).get().getLg().intValue());
	}

	@Test
	void xlSetsValue() {
		assertEquals(12, new FluentVBox().xl(12).get().getXl().intValue());
	}

	@Test
	void percentageWidthSetsValue() {
		assertEquals(50, new FluentVBox().percentageWidth(50).get().getPercentageWidth().intValue());
	}

	@Test
	void minPixelWidthSetsValue() {
		assertEquals(100, new FluentVBox().minPixelWidth(100).get().getMinPixelWidth().intValue());
	}

	@Test
	void maxPixelWidthSetsValue() {
		assertEquals(800, new FluentVBox().maxPixelWidth(800).get().getMaxPixelWidth().intValue());
	}

	@Test
	void pixelHeightSetsValue() {
		assertEquals(200, new FluentVBox().pixelHeight(200).get().getPixelHeight().intValue());
	}

	@Test
	void percentageHeightSetsValue() {
		assertEquals(75, new FluentVBox().percentageHeight(75).get().getPercentageHeight().intValue());
	}

	@Test
	void minPixelHeightSetsValue() {
		assertEquals(50, new FluentVBox().minPixelHeight(50).get().getMinPixelHeight().intValue());
	}

	@Test
	void maxPixelHeightSetsValue() {
		assertEquals(500, new FluentVBox().maxPixelHeight(500).get().getMaxPixelHeight().intValue());
	}

	@Test
	void pixelPaddingSetsValue() {
		assertEquals(10, new FluentVBox().pixelPadding(10).get().getPixelPadding().intValue());
	}

	@Test
	void pixelMemberPaddingSetsValue() {
		assertEquals(5, new FluentVBox().pixelMemberPadding(5).get().getPixelMemberPadding().intValue());
	}

	@Test
	void shrinkWrapSetsValue() {
		assertThat(new FluentVBox().shrinkWrap(ShrinkWrap.width).get().getShrinkWrap(), is(ShrinkWrap.width));
	}

	@Test
	void horizontalAlignmentSetsValue() {
		assertThat(new FluentVBox().horizontalAlignment(HorizontalAlignment.centre).get().getHorizontalAlignment(),
				is(HorizontalAlignment.centre));
	}

	@Test
	void verticalAlignmentSetsValue() {
		assertThat(new FluentVBox().verticalAlignment(VerticalAlignment.middle).get().getVerticalAlignment(),
				is(VerticalAlignment.middle));
	}

	@Test
	void invisibleConditionNameSetsValue() {
		assertThat(new FluentVBox().invisibleConditionName("invisible").get().getInvisibleConditionName(), is("invisible"));
	}

	@Test
	void fromCopiesWidgetId() {
		VBox src = new VBox();
		src.setWidgetId("wid");
		assertThat(new FluentVBox().from(src).get().getWidgetId(), is("wid"));
	}

	@Test
	void fromCopiesBorderTitle() {
		VBox src = new VBox();
		src.setBorderTitle("Title");
		assertThat(new FluentVBox().from(src).get().getBorderTitle(), is("Title"));
	}

	@Test
	void fromCopiesHorizontalAlignment() {
		VBox src = new VBox();
		src.setHorizontalAlignment(HorizontalAlignment.right);
		assertThat(new FluentVBox().from(src).get().getHorizontalAlignment(), is(HorizontalAlignment.right));
	}

	@Test
	void fromCopiesInvisibleConditionName() {
		VBox src = new VBox();
		src.setInvisibleConditionName("invisible");
		assertThat(new FluentVBox().from(src).get().getInvisibleConditionName(), is("invisible"));
	}
}
