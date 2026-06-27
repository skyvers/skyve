package org.skyve.impl.web.faces.renderers;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import java.io.IOException;

import org.junit.jupiter.api.Test;

import jakarta.faces.context.ResponseWriter;

@SuppressWarnings({"static-method", "resource"})
class SkyveMessageRendererTest {
	@Test
	void encodeIconStripsMarkupFromIconOnlyTitle() throws Exception {
		TestableSkyveMessageRenderer renderer = new TestableSkyveMessageRenderer();
		ResponseWriter writer = mock(ResponseWriter.class);

		renderer.encodeIconForTest(writer, "error", "<b>Name required</b>", true);

		verify(writer).startElement("span", null);
		verify(writer).writeAttribute("class", "ui-message-error-icon", null);
		verify(writer).writeAttribute("title", "Name required", null);
		verify(writer).endElement("span");
	}

	@Test
	void encodeIconLeavesNonIconTitleUnchangedAndUnwritten() throws Exception {
		TestableSkyveMessageRenderer renderer = new TestableSkyveMessageRenderer();
		ResponseWriter writer = mock(ResponseWriter.class);

		renderer.encodeIconForTest(writer, "warn", "<b>Name required</b>", false);

		verify(writer).startElement("span", null);
		verify(writer).writeAttribute("class", "ui-message-warn-icon", null);
		verify(writer, never()).writeAttribute("title", "<b>Name required</b>", null);
		verify(writer).endElement("span");
	}

	private static final class TestableSkyveMessageRenderer extends SkyveMessageRenderer {
		private void encodeIconForTest(ResponseWriter writer, String severity, String title, boolean iconOnly) throws IOException {
			encodeIcon(writer, severity, title, iconOnly);
		}
	}
}
