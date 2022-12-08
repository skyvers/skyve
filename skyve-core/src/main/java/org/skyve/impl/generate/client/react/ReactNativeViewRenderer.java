package org.skyve.impl.generate.client.react;

import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View;
import org.skyve.web.UserAgentType;

import java.util.Map;

public class ReactNativeViewRenderer extends ReactViewRenderer {
	static final String REACT_NATIVE_IMPORT = "react-native";
	static final String STARTING_INDENT = "\t\t\t";

	protected ReactNativeViewRenderer(User user,
									  Module module,
									  Document document,
									  View view,
									  String uxui,
									  Map<String, String> imports,
									  boolean extraIndent) {
		super(user, module, document, view, uxui, imports);
		final String startingIndent = (extraIndent ? STARTING_INDENT + "\t" : STARTING_INDENT);
		ReactNativeComponentRenderer newCR = new ReactNativeComponentRenderer(imports, startingIndent);
		newCR.setUserAgentType(UserAgentType.desktop);
		ReactNativeLayoutRenderer newLR = new ReactNativeLayoutRenderer(imports);
		newLR.setUserAgentType(UserAgentType.desktop);
		setRenderers(newCR, newLR);
	}
}
