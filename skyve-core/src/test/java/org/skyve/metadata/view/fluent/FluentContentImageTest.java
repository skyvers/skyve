package org.skyve.metadata.view.fluent;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.widget.bound.input.ContentImage;

@SuppressWarnings("static-method")
class FluentContentImageTest {

	@Test
	void defaultConstructorCreatesInstance() {
		assertThat(new FluentContentImage().get(), is(notNullValue()));
	}

	@Test
	void wrappingConstructorUsesInstance() {
		ContentImage ci = new ContentImage();
		assertEquals(ci, new FluentContentImage(ci).get());
	}

	@Test
	void editableSetsTrue() {
		assertThat(new FluentContentImage().editable(true).get().getEditable(), is(Boolean.TRUE));
	}

	@Test
	void editableSetsFalse() {
		assertThat(new FluentContentImage().editable(false).get().getEditable(), is(Boolean.FALSE));
	}

	@Test
	void showMarkupSetsTrue() {
		assertThat(new FluentContentImage().showMarkup(true).get().getShowMarkup(), is(Boolean.TRUE));
	}

	@Test
	void showMarkupSetsFalse() {
		assertThat(new FluentContentImage().showMarkup(false).get().getShowMarkup(), is(Boolean.FALSE));
	}

	@Test
	void pixelWidthSetsValue() {
		assertEquals(200, new FluentContentImage().pixelWidth(200).get().getPixelWidth().intValue());
	}

	@Test
	void pixelHeightSetsValue() {
		assertEquals(150, new FluentContentImage().pixelHeight(150).get().getPixelHeight().intValue());
	}

	@Test
	void minPixelWidthSetsValue() {
		assertEquals(50, new FluentContentImage().minPixelWidth(50).get().getMinPixelWidth().intValue());
	}

	@Test
	void maxPixelWidthSetsValue() {
		assertEquals(400, new FluentContentImage().maxPixelWidth(400).get().getMaxPixelWidth().intValue());
	}

	@Test
	void minPixelHeightSetsValue() {
		assertEquals(30, new FluentContentImage().minPixelHeight(30).get().getMinPixelHeight().intValue());
	}

	@Test
	void maxPixelHeightSetsValue() {
		assertEquals(300, new FluentContentImage().maxPixelHeight(300).get().getMaxPixelHeight().intValue());
	}

	@Test
	void percentageWidthSetsValue() {
		assertEquals(50, new FluentContentImage().percentageWidth(50).get().getPercentageWidth().intValue());
	}

	@Test
	void percentageHeightSetsValue() {
		assertEquals(75, new FluentContentImage().percentageHeight(75).get().getPercentageHeight().intValue());
	}

	@Test
	void responsiveWidthSetsValue() {
		assertEquals(6, new FluentContentImage().responsiveWidth(6).get().getResponsiveWidth().intValue());
	}

	@Test
	void smSetsValue() {
		assertEquals(3, new FluentContentImage().sm(3).get().getSm().intValue());
	}

	@Test
	void mdSetsValue() {
		assertEquals(6, new FluentContentImage().md(6).get().getMd().intValue());
	}

	@Test
	void lgSetsValue() {
		assertEquals(9, new FluentContentImage().lg(9).get().getLg().intValue());
	}

	@Test
	void xlSetsValue() {
		assertEquals(12, new FluentContentImage().xl(12).get().getXl().intValue());
	}

	@Test
	void fromCopiesEditable() {
		ContentImage src = new ContentImage();
		src.setEditable(Boolean.TRUE);
		assertThat(new FluentContentImage().from(src).get().getEditable(), is(Boolean.TRUE));
	}

	@Test
	void fromCopiesShowMarkup() {
		ContentImage src = new ContentImage();
		src.setShowMarkup(Boolean.FALSE);
		assertThat(new FluentContentImage().from(src).get().getShowMarkup(), is(Boolean.FALSE));
	}

	@Test
	void fromWithNullFlagsDoesNotThrow() {
		ContentImage src = new ContentImage();
		assertThat(new FluentContentImage().from(src).get(), is(notNullValue()));
	}
}
