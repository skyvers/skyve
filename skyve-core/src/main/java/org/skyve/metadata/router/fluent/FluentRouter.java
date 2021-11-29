package org.skyve.metadata.router.fluent;

import org.skyve.impl.metadata.repository.router.Router;

public class FluentRouter {
	private Router router = new Router();
	
	public FluentRouter() {
		// nothing to see
	}

	public FluentRouter(Router router) {
		uxuiSelectorClassName(router.getUxuiSelectorClassName());
		router.getUxUis().forEach(u -> addUxUi(new FluentUxUi(u)));
		router.getUnsecuredUrlPrefixes().forEach(u -> addUnsecuredUrlPrefix(u));
	}
	
	public FluentRouter uxuiSelectorClassName(String fullyQualifiedClassName) {
		router.setUxuiSelectorClassName(fullyQualifiedClassName);
		return this;
	}

	public FluentRouter addUxUi(FluentUxUi uxui) {
		router.getUxUis().add(uxui.get());
		return this;
	}

	public FluentRouter addUnsecuredUrlPrefix(String unsecuredUrlPrefix) {
		router.getUnsecuredUrlPrefixes().add(unsecuredUrlPrefix);
		return this;
	}

	public Router get() {
		return router;
	}
}
