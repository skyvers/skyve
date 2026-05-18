package org.skyve.impl.metadata.view;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class HorizontalAlignmentTest {

	@Test
	void toFlexAlignmentStringReturnsCorrectValues() {
		assertThat(HorizontalAlignment.left.toFlexAlignmentString(), is("start"));
		assertThat(HorizontalAlignment.centre.toFlexAlignmentString(), is("center"));
		assertThat(HorizontalAlignment.right.toFlexAlignmentString(), is("end"));
	}
}
