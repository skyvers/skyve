package org.skyve.impl.web.service.smartclient;

import org.skyve.domain.Bean;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View;

/**
 * For testing that we can instantiate a ViewJSONManipulator from SmartClientEditServlet from an init parameter.
 */
public class TestExtendingViewJSONManipulator extends ViewJSONManipulator {
	public TestExtendingViewJSONManipulator(User user,
										Module module,
										Document document,
										View view,
										String uxui,
										Bean bean,
										int editIdCounter,
										int createIdCounter,
										boolean forApply) {
		super(user, module, document, view, uxui, bean, editIdCounter, createIdCounter, forApply);
	}
}

