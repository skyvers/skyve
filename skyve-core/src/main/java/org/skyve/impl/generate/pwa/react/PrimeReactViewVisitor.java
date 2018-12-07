package org.skyve.impl.generate.pwa.react;

import java.util.Map;

import org.skyve.impl.web.UserAgentType;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.view.View;

public class PrimeReactViewVisitor extends ReactViewVisitor {
	static final String PRIME_REACT_VIEW_FILE = "../../PrimeReactView.js";
	static final String STARTING_INDENT = "\t\t\t";
	
	protected PrimeReactViewVisitor(Customer customer,
									Module module,
									Document document,
									View view,
									String widgetId,
									Map<String, String> imports,
									boolean extraIndent) {
		super(customer, module, document, view, widgetId, imports);
		final String startingIndent = (extraIndent ? STARTING_INDENT + "\t" : STARTING_INDENT);
		PrimeReactComponentRenderer newCR = new PrimeReactComponentRenderer(imports, startingIndent);
		newCR.setUserAgentType(UserAgentType.desktop);
		PrimeReactLayoutRenderer newLR = new PrimeReactLayoutRenderer(imports);
		newLR.setUserAgentType(UserAgentType.desktop);
		setRenderers(newCR, newLR);
	}
}
