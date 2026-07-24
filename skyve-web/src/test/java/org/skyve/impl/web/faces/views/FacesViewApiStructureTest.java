package org.skyve.impl.web.faces.views;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.lang.reflect.Field;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.router.UxUi;
import org.skyve.web.UserAgentType;

@SuppressWarnings("static-method")
class FacesViewApiStructureTest {
	@Test
	void exposesNoRequestSelectionAccessors() {
		assertThrows(NoSuchMethodException.class, () -> FacesView.class.getMethod("getUxUi"));
		assertThrows(NoSuchMethodException.class, () -> FacesView.class.getMethod("getUserAgentType"));
		assertThrows(NoSuchMethodException.class, () -> FacesView.class.getMethod("setUxUi", UxUi.class));
		assertThrows(NoSuchMethodException.class,
				() -> FacesView.class.getMethod("setUserAgentType", UserAgentType.class));
	}

	@Test
	void retainsNoMutableSelectionFields() {
		for (Field field : FacesView.class.getDeclaredFields()) {
			assertFalse(UxUi.class.equals(field.getType()), "Unexpected UX/UI field: " + field.getName());
			assertFalse(UserAgentType.class.equals(field.getType()),
					"Unexpected user-agent field: " + field.getName());
		}
	}
}
