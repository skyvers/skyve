package org.skyve.metadata.router.fluent;

import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.metadata.repository.router.UxUiMetadata;

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

	public FluentRouter removeUxUi(String name) {
		router.getUxUis().removeIf(u -> name.equals(u.getName()));
		return this;
	}

	public FluentUxUi findUxUi(String name) {
		UxUiMetadata result = router.getUxUis().stream().filter(u -> name.equals(u.getName())).findAny().orElse(null);
		if (result != null) {
			return new FluentUxUi(result);
		}
		return null;
	}

	public FluentRouter clearUxUis() {
		router.getUxUis().clear();
		return this;
	}

	public FluentRouter addUnsecuredUrlPrefix(String unsecuredUrlPrefix) {
		router.getUnsecuredUrlPrefixes().add(unsecuredUrlPrefix);
		return this;
	}

	public FluentRouter removeUnsecuredUrlPrefix(String unsecuredUrlPrefix) {
		router.getUnsecuredUrlPrefixes().remove(unsecuredUrlPrefix);
		return this;
	}

	public FluentRouter clearUnsecuredUrlPrefixes() {
		router.getUnsecuredUrlPrefixes().clear();
		return this;
	}

	public Router get() {
		return router;
	}
}
