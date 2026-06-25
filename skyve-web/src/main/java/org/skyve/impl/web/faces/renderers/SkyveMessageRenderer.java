package org.skyve.impl.web.faces.renderers;

import java.io.IOException;

import org.primefaces.component.message.MessageRenderer;
import org.skyve.metadata.view.TextOutput.Sanitisation;
import org.skyve.util.OWASP;

import jakarta.faces.context.ResponseWriter;

/**
 * PrimeFaces message renderer that keeps field-message icon tooltips plain text.
 *
 * <p>PrimeFaces field {@code p:message}, global {@code p:messages}, and
 * {@code p:growl} components consume the same JSF {@code FacesMessage}. This
 * renderer strips markup only at the field-message icon {@code title} attribute
 * boundary, leaving the shared message value available for global message
 * components that intentionally render trusted markup.
 */
public class SkyveMessageRenderer extends MessageRenderer {
	@Override
	protected void encodeIcon(ResponseWriter writer, String severity, String title, boolean iconOnly) throws IOException {
		super.encodeIcon(writer, severity, iconOnly ? OWASP.sanitise(Sanitisation.text, title) : title, iconOnly);
	}
}
