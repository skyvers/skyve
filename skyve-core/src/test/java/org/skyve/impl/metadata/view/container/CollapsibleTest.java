package org.skyve.impl.metadata.view.container;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class CollapsibleTest {

	@Test
	void toCollapsibleStringReturnsCorrectValue() {
		assertThat(Collapsible.open.toCollapsibleString(), is("open"));
		assertThat(Collapsible.closed.toCollapsibleString(), is("closed"));
	}
}
