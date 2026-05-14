package org.skyve.metadata.view.fluent;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.widget.bound.input.Slider;

@SuppressWarnings("static-method")
class FluentSliderTest {

	@Test
	void defaultConstructorCreatesInstance() {
		assertThat(new FluentSlider().get(), is(notNullValue()));
	}

	@Test
	void wrappingConstructorUsesInstance() {
		Slider s = new Slider();
		assertEquals(s, new FluentSlider(s).get());
	}

	@Test
	void minSetsValue() {
		assertThat(new FluentSlider().min(0.5).get().getMin(), is(0.5));
	}

	@Test
	void maxSetsValue() {
		assertThat(new FluentSlider().max(10.0).get().getMax(), is(10.0));
	}

	@Test
	void numberOfDiscreteValuesSetsValue() {
		assertEquals(5, new FluentSlider().numberOfDiscreteValues(5).get().getNumberOfDiscreteValues().intValue());
	}

	@Test
	void roundingPrecisionSetsValue() {
		assertEquals(2, new FluentSlider().roundingPrecision(2).get().getRoundingPrecision().intValue());
	}

	@Test
	void verticalSetsTrue() {
		assertThat(new FluentSlider().vertical(true).get().getVertical(), is(Boolean.TRUE));
	}

	@Test
	void verticalSetsFalse() {
		assertThat(new FluentSlider().vertical(false).get().getVertical(), is(Boolean.FALSE));
	}

	@Test
	void pixelWidthSetsValue() {
		assertEquals(200, new FluentSlider().pixelWidth(200).get().getPixelWidth().intValue());
	}

	@Test
	void pixelHeightSetsValue() {
		assertEquals(100, new FluentSlider().pixelHeight(100).get().getPixelHeight().intValue());
	}

	@Test
	void minPixelHeightSetsValue() {
		assertEquals(20, new FluentSlider().minPixelHeight(20).get().getMinPixelHeight().intValue());
	}

	@Test
	void maxPixelHeightSetsValue() {
		assertEquals(300, new FluentSlider().maxPixelHeight(300).get().getMaxPixelHeight().intValue());
	}

	@Test
	void fromCopiesMin() {
		Slider src = new Slider();
		src.setMin(Double.valueOf(1.0));
		assertThat(new FluentSlider().from(src).get().getMin(), is(1.0));
	}

	@Test
	void fromCopiesMax() {
		Slider src = new Slider();
		src.setMax(Double.valueOf(100.0));
		assertThat(new FluentSlider().from(src).get().getMax(), is(100.0));
	}

	@Test
	void fromCopiesVertical() {
		Slider src = new Slider();
		src.setVertical(Boolean.TRUE);
		assertThat(new FluentSlider().from(src).get().getVertical(), is(Boolean.TRUE));
	}

	@Test
	void fromCopiesNumberOfDiscreteValues() {
		Slider src = new Slider();
		src.setNumberOfDiscreteValues(Integer.valueOf(10));
		assertEquals(10, new FluentSlider().from(src).get().getNumberOfDiscreteValues().intValue());
	}

	@Test
	void fromWithNullsFlagsDoesNotThrow() {
		Slider src = new Slider();
		assertThat(new FluentSlider().from(src).get(), is(notNullValue()));
	}
}
