package org.skyve.impl.generate.client.flutter;

import java.util.Set;

import org.skyve.impl.generate.client.ClientViewRenderer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View;
import org.skyve.web.UserAgentType;

public class FlutterViewRenderer extends ClientViewRenderer {
	static final String STARTING_INDENT = "";

	protected Set<String> imports;

	protected FlutterViewRenderer(User user,
									Module module,
									Document document,
									View view,
									String uxui,
									String projectName,
									Set<String> imports) {
		super(user, module, document, view, uxui);
		this.imports = imports;
		FlutterComponentRenderer newCR = new FlutterComponentRenderer(projectName, imports, STARTING_INDENT);
		newCR.setUserAgentType(UserAgentType.desktop);
		FlutterLayoutRenderer newLR = new FlutterLayoutRenderer(imports);
		newLR.setUserAgentType(UserAgentType.desktop);
		setRenderers(newCR, newLR);
	}
}
