package org.skyve.impl.generate.pwa.react;

import java.util.Map;

import org.skyve.impl.generate.pwa.PWAViewRenderer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View;

public class ReactViewRenderer extends PWAViewRenderer {
	protected Map<String, String> imports;

	public ReactViewRenderer(User user,
								Module module,
								Document document,
								View view,
								Map<String, String> imports) {
		super(user, module, document, view);
		this.imports = imports;
	}
}
