package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.reference.ExternalReference;
import org.skyve.impl.metadata.view.widget.Link;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.TextOutput.Sanitisation;
import org.skyve.metadata.view.View.ViewType;

@SuppressWarnings("static-method")
class ViewJSONManipulatorLinkEscapingTest {
	@Test
	void visitLinkEscapesValueInGeneratedHtmlSnippetByDefault() throws Exception {
		Link link = linkWithExternalReference("<img src=x onerror=alert(1)> & \"quoted\" 'single'");

		ViewJSONManipulator manipulator = newManipulator();
		manipulator.visitLink(link, true, true);

		ViewFormat format = anonymousFormat(manipulator);
		String html = format.getFormat();
		assertTrue(html.contains(">&lt;img src=x onerror=alert(1)&gt; &amp; &#34;quoted&#34; &#39;single&#39;</a>"), html);
		assertFalse(format.isEscape());
	}

	@Test
	void visitLinkEscapesValueInGeneratedHtmlSnippetWhenExplicitTrue() throws Exception {
		Link link = linkWithExternalReference("<img src=x onerror=alert(1)> & \"quoted\" 'single'");
		link.setEscapeValue(Boolean.TRUE);

		ViewJSONManipulator manipulator = newManipulator();
		manipulator.visitLink(link, true, true);

		ViewFormat format = anonymousFormat(manipulator);
		String html = format.getFormat();
		assertTrue(html.contains(">&lt;img src=x onerror=alert(1)&gt; &amp; &#34;quoted&#34; &#39;single&#39;</a>"), html);
		assertFalse(format.isEscape());
	}

	@Test
	void visitLinkAllowsTrustedValueInGeneratedHtmlSnippetWhenEscapeFalse() throws Exception {
		Link link = linkWithExternalReference("<b>Trusted</b>");
		link.setEscapeValue(Boolean.FALSE);

		ViewJSONManipulator manipulator = newManipulator();
		manipulator.visitLink(link, true, true);

		ViewFormat format = anonymousFormat(manipulator);
		assertTrue(format.getFormat().contains("><b>Trusted</b></a>"), format.getFormat());
		assertFalse(format.isEscape());
	}

	private static Link linkWithExternalReference(String value) {
		ExternalReference reference = new ExternalReference();
		reference.setHref("https://example.test/help");
		Link link = new Link();
		link.setReference(reference);
		link.setValue(value);
		return link;
	}

	private static ViewJSONManipulator newManipulator() {
		User user = mock(User.class);
		when(user.getCustomer()).thenReturn(new CustomerImpl());

		ModuleImpl module = new ModuleImpl();
		module.setName("testModule");

		DocumentImpl document = new DocumentImpl();
		document.setName("TestDoc");
		document.setPersistent(new Persistent());

		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test Title");

		return new ViewJSONManipulator(user, module, document, view, "external", mock(Bean.class), 0, 0, false);
	}

	private static ViewFormat anonymousFormat(ViewJSONManipulator manipulator) throws Exception {
		Field field = ViewJSONManipulator.class.getDeclaredField("formats");
		field.setAccessible(true);
		@SuppressWarnings("unchecked")
		Map<String, Map<String, ViewFormat>> formats = (Map<String, Map<String, ViewFormat>>) field.get(manipulator);
		ViewFormat format = formats.get("").get("_0");
		assertTrue(Sanitisation.none.equals(format.getSanitise()));
		return format;
	}
}
