package org.skyve.metadata.view.fluent;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.container.form.FormColumn;

/**
 * Tests for {@link FluentFormColumn} setters and {@code from()} round-trip.
 */
@SuppressWarnings("static-method")
class FluentFormColumnTest {

	@Test
	void defaultConstructorCreatesInstance() {
		assertThat(new FluentFormColumn().get(), is(notNullValue()));
	}

	@Test
	void wrappingConstructorPreservesInstance() {
		FormColumn fc = new FormColumn();
		assertThat(new FluentFormColumn(fc).get(), is(fc));
	}

	@Test
	void pixelWidthSetsValue() {
		assertThat(new FluentFormColumn().pixelWidth(120).get().getPixelWidth(), is(Integer.valueOf(120)));
	}

	@Test
	void responsiveWidthSetsValue() {
		assertThat(new FluentFormColumn().responsiveWidth(6).get().getResponsiveWidth(), is(Integer.valueOf(6)));
	}

	@Test
	void smSetsValue() {
		assertThat(new FluentFormColumn().sm(4).get().getSm(), is(Integer.valueOf(4)));
	}

	@Test
	void mdSetsValue() {
		assertThat(new FluentFormColumn().md(6).get().getMd(), is(Integer.valueOf(6)));
	}

	@Test
	void lgSetsValue() {
		assertThat(new FluentFormColumn().lg(8).get().getLg(), is(Integer.valueOf(8)));
	}

	@Test
	void xlSetsValue() {
		assertThat(new FluentFormColumn().xl(10).get().getXl(), is(Integer.valueOf(10)));
	}

	@Test
	void percentageWidthSetsValue() {
		assertThat(new FluentFormColumn().percentageWidth(50).get().getPercentageWidth(), is(Integer.valueOf(50)));
	}

	@Test
	void fromCopiesPixelWidth() {
		FormColumn source = new FormColumn();
		source.setPixelWidth(Integer.valueOf(200));
		assertThat(new FluentFormColumn().from(source).get().getPixelWidth(), is(Integer.valueOf(200)));
	}

	@Test
	void fromCopiesResponsiveWidth() {
		FormColumn source = new FormColumn();
		source.setResponsiveWidth(Integer.valueOf(4));
		assertThat(new FluentFormColumn().from(source).get().getResponsiveWidth(), is(Integer.valueOf(4)));
	}

	@Test
	void fromCopiesSm() {
		FormColumn source = new FormColumn();
		source.setSm(Integer.valueOf(3));
		assertThat(new FluentFormColumn().from(source).get().getSm(), is(Integer.valueOf(3)));
	}

	@Test
	void fromCopiesMd() {
		FormColumn source = new FormColumn();
		source.setMd(Integer.valueOf(5));
		assertThat(new FluentFormColumn().from(source).get().getMd(), is(Integer.valueOf(5)));
	}

	@Test
	void fromCopiesLg() {
		FormColumn source = new FormColumn();
		source.setLg(Integer.valueOf(7));
		assertThat(new FluentFormColumn().from(source).get().getLg(), is(Integer.valueOf(7)));
	}

	@Test
	void fromCopiesXl() {
		FormColumn source = new FormColumn();
		source.setXl(Integer.valueOf(9));
		assertThat(new FluentFormColumn().from(source).get().getXl(), is(Integer.valueOf(9)));
	}

	@Test
	void fromCopiesPercentageWidth() {
		FormColumn source = new FormColumn();
		source.setPercentageWidth(Integer.valueOf(33));
		assertThat(new FluentFormColumn().from(source).get().getPercentageWidth(), is(Integer.valueOf(33)));
	}

	@Test
	void fromIgnoresNullFields() {
		FormColumn source = new FormColumn();
		// all fields null — should not throw
		FluentFormColumn result = new FluentFormColumn().from(source);
		assertThat(result.get().getPixelWidth(), is((Integer) null));
		assertThat(result.get().getResponsiveWidth(), is((Integer) null));
	}
}
