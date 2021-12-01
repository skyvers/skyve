package org.skyve.metadata.router.fluent;

import org.skyve.impl.metadata.repository.router.Router;

public class FluentRouter {
	private Router router = null;
	
	public FluentRouter() {
		router = new Router();
	}

	public FluentRouter(Router router) {
		this.router = router;
	}

	public FluentRouter from(@SuppressWarnings("hiding") Router router) {
		uxuiSelectorClassName(router.getUxuiSelectorClassName());
		router.getUxUis().forEach(u -> addUxUi(new FluentUxUi().from(u)));
		router.getUnsecuredUrlPrefixes().forEach(u -> addUnsecuredUrlPrefix(u));
		return this;
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
