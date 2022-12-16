package org.skyve.impl.generate.client.react;

import java.util.Map;

import org.skyve.impl.generate.client.ClientViewRenderer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View;

public class ReactViewRenderer extends ClientViewRenderer {
	protected Map<String, String> imports;

	public ReactViewRenderer(User user,
								Module module,
								Document document,
								View view,
								String uxui,
								Map<String, String> imports) {
		super(user, module, document, view, uxui);
		this.imports = imports;
	}
}
