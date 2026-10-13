package org.skyve.impl.generate.client.flutter;

import java.util.Set;

import org.skyve.impl.generate.client.ClientViewRenderer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View;
import org.skyve.web.UserAgentType;

import jakarta.annotation.Nonnull;

/**
 * Flutter view renderer entry point used by {@link FlutterGenerator}.
 */
public class FlutterViewRenderer extends ClientViewRenderer {
	static final String STARTING_INDENT = "";

	protected FlutterViewRenderer(@Nonnull User user,
									@Nonnull Module module,
									@Nonnull Document document,
									@Nonnull View view,
									@Nonnull String uxui,
									@Nonnull Set<String> imports) {
		super(user, module, document, view, uxui);
		FlutterComponentRenderer newCR = new FlutterComponentRenderer(imports, STARTING_INDENT);
		newCR.setUserAgentType(UserAgentType.desktop);
		FlutterLayoutRenderer newLR = new FlutterLayoutRenderer(imports);
		newLR.setUserAgentType(UserAgentType.desktop);
		setRenderers(newCR, newLR);
	}
}
