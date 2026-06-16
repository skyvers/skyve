package org.skyve.metadata.view.fluent;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.skyve.metadata.MetaData;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.widget.Blurb;
import org.skyve.impl.metadata.view.widget.DynamicImage;
import org.skyve.impl.metadata.view.widget.Link;
import org.skyve.impl.metadata.view.widget.StaticImage;
import org.skyve.impl.metadata.view.widget.bound.Label;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridContainerColumn;

/**
 * Tests for {@link FluentDataGridContainerColumn} widget management and round-trip.
 */
@SuppressWarnings("static-method")
class FluentDataGridContainerColumnTest {

	@Test
	void defaultConstructorCreatesInstance() {
		assertThat(new FluentDataGridContainerColumn().get(), is(notNullValue()));
	}

	@Test
	void wrappingConstructorPreservesInstance() {
		DataGridContainerColumn col = new DataGridContainerColumn();
		assertThat(new FluentDataGridContainerColumn(col).get(), is(col));
	}

	// ---- base column setters ----

	@Test
	void titleSetsValue() {
		assertThat(new FluentDataGridContainerColumn().title("Actions").get().getTitle(), is("Actions"));
	}

	@Test
	void alignmentSetsValue() {
		assertThat(new FluentDataGridContainerColumn().alignment(HorizontalAlignment.centre).get().getAlignment(),
				is(HorizontalAlignment.centre));
	}

	@Test
	void pixelWidthSetsValue() {
		assertThat(new FluentDataGridContainerColumn().pixelWidth(100).get().getPixelWidth(), is(Integer.valueOf(100)));
	}

	// ---- addWidget overloads ----

	@Test
	void addLinkWidgetAddsWidget() {
		FluentDataGridContainerColumn col = new FluentDataGridContainerColumn().addWidget(new FluentLink());
		assertEquals(1, col.get().getWidgets().size());
		assertThat(col.get().getWidgets().get(0), instanceOf(Link.class));
	}

	@Test
	void addStaticImageWidgetAddsWidget() {
		FluentDataGridContainerColumn col = new FluentDataGridContainerColumn().addWidget(new FluentStaticImage());
		assertEquals(1, col.get().getWidgets().size());
		assertThat(col.get().getWidgets().get(0), instanceOf(StaticImage.class));
	}

	@Test
	void addDynamicImageWidgetAddsWidget() {
		FluentDataGridContainerColumn col = new FluentDataGridContainerColumn().addWidget(new FluentDynamicImage());
		assertEquals(1, col.get().getWidgets().size());
		assertThat(col.get().getWidgets().get(0), instanceOf(DynamicImage.class));
	}

	@Test
	void addBlurbWidgetAddsWidget() {
		FluentDataGridContainerColumn col = new FluentDataGridContainerColumn().addWidget(new FluentBlurb());
		assertEquals(1, col.get().getWidgets().size());
		assertThat(col.get().getWidgets().get(0), instanceOf(Blurb.class));
	}

	@Test
	void addLabelWidgetAddsWidget() {
		FluentDataGridContainerColumn col = new FluentDataGridContainerColumn().addWidget(new FluentLabel());
		assertEquals(1, col.get().getWidgets().size());
		assertThat(col.get().getWidgets().get(0), instanceOf(Label.class));
	}

	@Test
	void addMultipleWidgets() {
		FluentDataGridContainerColumn col = new FluentDataGridContainerColumn()
				.addWidget(new FluentLink())
				.addWidget(new FluentBlurb());
		assertEquals(2, col.get().getWidgets().size());
	}

	// ---- from() round-trips ----

	@Test
	void fromLinkRoundTrips() {
		DataGridContainerColumn source = new DataGridContainerColumn();
		source.getWidgets().add(new Link());
		FluentDataGridContainerColumn result = new FluentDataGridContainerColumn().from(source);
		assertThat(result.get().getWidgets().get(0), instanceOf(Link.class));
	}

	@Test
	void fromStaticImageRoundTrips() {
		DataGridContainerColumn source = new DataGridContainerColumn();
		source.getWidgets().add(new StaticImage());
		FluentDataGridContainerColumn result = new FluentDataGridContainerColumn().from(source);
		assertThat(result.get().getWidgets().get(0), instanceOf(StaticImage.class));
	}

	@Test
	void fromDynamicImageRoundTrips() {
		DataGridContainerColumn source = new DataGridContainerColumn();
		source.getWidgets().add(new DynamicImage());
		FluentDataGridContainerColumn result = new FluentDataGridContainerColumn().from(source);
		assertThat(result.get().getWidgets().get(0), instanceOf(DynamicImage.class));
	}

	@Test
	void fromBlurbRoundTrips() {
		DataGridContainerColumn source = new DataGridContainerColumn();
		source.getWidgets().add(new Blurb());
		FluentDataGridContainerColumn result = new FluentDataGridContainerColumn().from(source);
		assertThat(result.get().getWidgets().get(0), instanceOf(Blurb.class));
	}

	@Test
	void fromLabelRoundTrips() {
		DataGridContainerColumn source = new DataGridContainerColumn();
		source.getWidgets().add(new Label());
		FluentDataGridContainerColumn result = new FluentDataGridContainerColumn().from(source);
		assertThat(result.get().getWidgets().get(0), instanceOf(Label.class));
	}

	@Test
	void fromEmptyColumnRoundTrips() {
		DataGridContainerColumn source = new DataGridContainerColumn();
		FluentDataGridContainerColumn result = new FluentDataGridContainerColumn().from(source);
		assertEquals(0, result.get().getWidgets().size());
	}

	@Test
	void fromWithUnknownWidgetTypeThrowsIllegalState() {
		DataGridContainerColumn source = new DataGridContainerColumn();
		source.getWidgets().add(new MetaData() {
			// intentionally empty anonymous metadata implementation for invalid widget path
		});
		FluentDataGridContainerColumn fluent = new FluentDataGridContainerColumn();
		assertThrows(IllegalStateException.class, () -> fluent.from(source));
	}
}
