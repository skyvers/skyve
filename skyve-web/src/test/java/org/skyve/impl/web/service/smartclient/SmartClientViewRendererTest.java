package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.user.User;
import org.skyve.util.OWASP;

@SuppressWarnings("static-method")
class SmartClientViewRendererTest {
	private static final String UNSAFE_TEXT = "<img src=x onerror=alert(1)> & \"quoted\" 'single'";

	@Test
	void renderListGridWithoutDocumentContextThrows() {
		CustomerImpl customer = mock(CustomerImpl.class);
		User user = mock(User.class);
		when(user.getCustomer()).thenReturn(customer);

		ModuleImpl module = mock(ModuleImpl.class);
		ViewImpl view = new ViewImpl();
		view.setName("edit");

		SmartClientViewRenderer renderer = new SmartClientViewRenderer(user, module, null, view, "desktop", false);
		ListGrid grid = new ListGrid();

		assertThrows(MetaDataException.class, () -> renderer.renderListGrid("Contacts", false, grid));
	}

	@Test
	void escapeSmartClientTextReturnsNullForNullValue() {
		assertNull(SmartClientViewRenderer.escapeSmartClientText(null, true));
		assertNull(SmartClientViewRenderer.escapeSmartClientText(null, true));
		assertNull(SmartClientViewRenderer.escapeSmartClientText(null, false));
	}

	@Test
	void escapeSmartClientTextEscapesHtmlByDefaultBeforeJavaScriptStringEscaping() {
		assertEquals(OWASP.escapeJsString(OWASP.escapeHtml(UNSAFE_TEXT)),
						SmartClientViewRenderer.escapeSmartClientText(UNSAFE_TEXT, true));
	}

	@Test
	void escapeSmartClientTextEscapesHtmlWhenExplicitlyTrueBeforeJavaScriptStringEscaping() {
		assertEquals(OWASP.escapeJsString(OWASP.escapeHtml(UNSAFE_TEXT)),
						SmartClientViewRenderer.escapeSmartClientText(UNSAFE_TEXT, true));
	}

	@Test
	void escapeSmartClientTextLeavesTrustedHtmlRawWhenFalseButEscapesJavaScriptString() {
		assertEquals(OWASP.escapeJsString(UNSAFE_TEXT),
						SmartClientViewRenderer.escapeSmartClientText(UNSAFE_TEXT, false));
	}
}
