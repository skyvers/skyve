package org.skyve.impl.metadata.view;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.container.HBox;

@SuppressWarnings("static-method")
class BorderedDefaultMethodTest {

	@Test
	void getLocalisedBorderTitleReturnsNullWhenBorderTitleNull() {
		HBox box = new HBox();
		box.setBorderTitle(null);
		assertThat(box.getLocalisedBorderTitle(), nullValue());
	}

	@Test
	void getLocalisedBorderTitleReturnsValueWhenBorderTitleSet() {
		HBox box = new HBox();
		box.setBorderTitle("Section Header");
		assertThat(box.getLocalisedBorderTitle(), notNullValue());
	}
}
