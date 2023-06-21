package org.skyve.impl.generate.client.react;

import java.util.Map;

import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View;
import org.skyve.web.UserAgentType;

public class PrimeReactViewRenderer extends ReactViewRenderer {
	static final String PRIME_REACT_VIEW_FILE = "../../PrimeReactView.js";
	static final String STARTING_INDENT = "\t\t\t";
	
	protected PrimeReactViewRenderer(User user,
										Module module,
										Document document,
										View view,
										String uxui,
										Map<String, String> imports,
										boolean extraIndent) {
		super(user, module, document, view, uxui, imports);
		final String startingIndent = (extraIndent ? STARTING_INDENT + "\t" : STARTING_INDENT);
		PrimeReactComponentRenderer newCR = new PrimeReactComponentRenderer(imports, startingIndent);
		newCR.setUserAgentType(UserAgentType.desktop);
		PrimeReactLayoutRenderer newLR = new PrimeReactLayoutRenderer(imports);
		newLR.setUserAgentType(UserAgentType.desktop);
		setRenderers(newCR, newLR);
	}
}
