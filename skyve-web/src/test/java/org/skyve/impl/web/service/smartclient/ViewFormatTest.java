package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.view.TextOutput.Sanitisation;

@SuppressWarnings("static-method")
class ViewFormatTest {
	@Test
	void constructorPropertiesAreExposedByGetters() {
		ViewFormat format = new ViewFormat("#,##0.00", false, Sanitisation.none);

		assertEquals("#,##0.00", format.getFormat());
		assertFalse(format.isEscape());
		assertEquals(Sanitisation.none, format.getSanitise());
	}
}
