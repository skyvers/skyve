package org.skyve.impl.metadata.view;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

/**
 * Tests for {@link ViewImpl}, covering the default method on the {@link org.skyve.metadata.view.View} interface.
 */
@SuppressWarnings("static-method")
class ViewImplTest {

	@Test
	void localisedTitleReturnsValueWhenTitleSet() {
		ViewImpl v = new ViewImpl();
		v.setTitle("My View");
		assertNotNull(v.getLocalisedTitle());
	}

	@Test
	void localisedTitleReturnsNullWhenTitleNull() {
		ViewImpl v = new ViewImpl();
		assertNull(v.getLocalisedTitle());
	}
}
