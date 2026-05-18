package org.skyve.impl.metadata.view.widget;

import static org.junit.jupiter.api.Assertions.assertFalse;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class ButtonTest {

	@Test
	void showsLabelByDefaultReturnsFalse() {
		assertFalse(new Button().showsLabelByDefault());
	}
}
