package org.skyve.metadata.view.fluent;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.widget.FilterParameterImpl;
import org.skyve.impl.metadata.view.widget.bound.ParameterImpl;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListRepeater;

@SuppressWarnings("static-method")
class FluentListRepeaterTest {

	@Test
	void defaultConstructorCreatesList() {
		assertThat(new FluentListRepeater().get(), is(notNullValue()));
	}

	@Test
	void wrappingConstructorUsesList() {
		ListRepeater lr = new ListRepeater();
		assertEquals(lr, new FluentListRepeater(lr).get());
	}

	@Test
	void showColumnHeadersSetsTrue() {
		assertThat(new FluentListRepeater().showColumnHeaders(true).get().getShowColumnHeaders(), is(Boolean.TRUE));
	}

	@Test
	void showColumnHeadersSetsFalse() {
		assertThat(new FluentListRepeater().showColumnHeaders(false).get().getShowColumnHeaders(), is(Boolean.FALSE));
	}

	@Test
	void showGridSetsTrue() {
		assertThat(new FluentListRepeater().showGrid(true).get().getShowGrid(), is(Boolean.TRUE));
	}

	@Test
	void titleSetsValue() {
		assertThat(new FluentListRepeater().title("My List").get().getTitle(), is("My List"));
	}

	@Test
	void queryNameSetsValue() {
		assertThat(new FluentListRepeater().queryName("qContacts").get().getQueryName(), is("qContacts"));
	}

	@Test
	void modelNameSetsValue() {
		assertThat(new FluentListRepeater().modelName("myModel").get().getModelName(), is("myModel"));
	}

	@Test
	void postRefreshConditionNameSetsValue() {
		assertThat(new FluentListRepeater().postRefreshConditionName("cond").get().getPostRefreshConditionName(), is("cond"));
	}

	@Test
	void invisibleConditionNameSetsValue() {
		assertThat(new FluentListRepeater().invisibleConditionName("invisible").get().getInvisibleConditionName(), is("invisible"));
	}

	@Test
	void pixelWidthSetsValue() {
		assertEquals(400, new FluentListRepeater().pixelWidth(400).get().getPixelWidth().intValue());
	}

	@Test
	void pixelHeightSetsValue() {
		assertEquals(300, new FluentListRepeater().pixelHeight(300).get().getPixelHeight().intValue());
	}

	@Test
	void minPixelWidthSetsValue() {
		assertEquals(100, new FluentListRepeater().minPixelWidth(100).get().getMinPixelWidth().intValue());
	}

	@Test
	void smSetsValue() {
		assertEquals(3, new FluentListRepeater().sm(3).get().getSm().intValue());
	}

	@Test
	void mdSetsValue() {
		assertEquals(6, new FluentListRepeater().md(6).get().getMd().intValue());
	}

	@Test
	void lgSetsValue() {
		assertEquals(9, new FluentListRepeater().lg(9).get().getLg().intValue());
	}

	@Test
	void xlSetsValue() {
		assertEquals(12, new FluentListRepeater().xl(12).get().getXl().intValue());
	}

	@Test
	void responsiveWidthSetsValue() {
		assertEquals(6, new FluentListRepeater().responsiveWidth(6).get().getResponsiveWidth().intValue());
	}

	@Test
	void percentageHeightSetsValue() {
		assertEquals(75, new FluentListRepeater().percentageHeight(75).get().getPercentageHeight().intValue());
	}

	@Test
	void addFilterParameterAddsParam() {
		FluentListRepeater lr = new FluentListRepeater().addFilterParameter(new FluentFilterParameter());
		assertEquals(1, lr.get().getFilterParameters().size());
	}

	@Test
	void addParameterAddsParam() {
		FluentListRepeater lr = new FluentListRepeater().addParameter(new FluentParameter());
		assertEquals(1, lr.get().getParameters().size());
	}

	@Test
	void fromCopiesTitle() {
		ListRepeater src = new ListRepeater();
		src.setTitle("My List");
		assertThat(new FluentListRepeater().from(src).get().getTitle(), is("My List"));
	}

	@Test
	void fromCopiesQueryName() {
		ListRepeater src = new ListRepeater();
		src.setQueryName("qContacts");
		assertThat(new FluentListRepeater().from(src).get().getQueryName(), is("qContacts"));
	}

	@Test
	void fromCopiesShowColumnHeaders() {
		ListRepeater src = new ListRepeater();
		src.setShowColumnHeaders(Boolean.TRUE);
		assertThat(new FluentListRepeater().from(src).get().getShowColumnHeaders(), is(Boolean.TRUE));
	}

	@Test
	void fromCopiesShowGrid() {
		ListRepeater src = new ListRepeater();
		src.setShowGrid(Boolean.FALSE);
		assertThat(new FluentListRepeater().from(src).get().getShowGrid(), is(Boolean.FALSE));
	}

	@Test
	void fromCopiesInvisibleConditionName() {
		ListRepeater src = new ListRepeater();
		src.setInvisibleConditionName("invisible");
		assertThat(new FluentListRepeater().from(src).get().getInvisibleConditionName(), is("invisible"));
	}

	@Test
	void maxPixelWidthRoundtrips() {
		assertThat(new FluentListRepeater().maxPixelWidth(500).get().getMaxPixelWidth(), is(Integer.valueOf(500)));
	}

	@Test
	void maxPixelHeightRoundtrips() {
		assertThat(new FluentListRepeater().maxPixelHeight(400).get().getMaxPixelHeight(), is(Integer.valueOf(400)));
	}

	@Test
	void minPixelHeightRoundtrips() {
		assertThat(new FluentListRepeater().minPixelHeight(100).get().getMinPixelHeight(), is(Integer.valueOf(100)));
	}

	@Test
	void percentageWidthRoundtrips() {
		assertThat(new FluentListRepeater().percentageWidth(75).get().getPercentageWidth(), is(Integer.valueOf(75)));
	}

	@Test
	void fromCopiesParametersViaLambda() {
		ListRepeater src = new ListRepeater();
		src.getParameters().add(new ParameterImpl());
		assertEquals(1, new FluentListRepeater().from(src).get().getParameters().size());
	}

	@Test
	void fromCopiesFilterParametersViaLambda() {
		ListRepeater src = new ListRepeater();
		src.getFilterParameters().add(new FilterParameterImpl());
		assertEquals(1, new FluentListRepeater().from(src).get().getFilterParameters().size());
	}
}
