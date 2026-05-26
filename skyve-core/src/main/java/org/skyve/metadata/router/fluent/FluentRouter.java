package org.skyve.metadata.router.fluent;

import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.metadata.repository.router.UxUiMetadata;

/**
 * Builds router metadata including UX/UI variants and unsecured URL prefixes.
 */
public class FluentRouter {
	private Router router = null;
	
	/**
	 * Creates a builder with new empty router metadata.
	 */
	public FluentRouter() {
		router = new Router();
	}

	/**
	 * Creates a builder around existing router metadata.
	 *
	 * @param router backing router metadata
	 */
	public FluentRouter(Router router) {
		this.router = router;
	}

	/**
	 * Copies router metadata into this builder.
	 *
	 * @param router source router metadata
	 * @return this builder
	 */
	public FluentRouter from(@SuppressWarnings("hiding") Router router) {
		uxuiSelectorClassName(router.getUxuiSelectorClassName());
		router.getUxUis().forEach(u -> addUxUi(new FluentUxUi().from(u)));
		router.getUnsecuredUrlPrefixes().forEach(this::addUnsecuredUrlPrefix);
		return this;
	}
	
	/**
	 * Sets the UX/UI selector class used at runtime.
	 *
	 * @param fullyQualifiedClassName selector class name
	 * @return this builder
	 */
	public FluentRouter uxuiSelectorClassName(String fullyQualifiedClassName) {
		router.setUxuiSelectorClassName(fullyQualifiedClassName);
		return this;
	}

	/**
	 * Adds one UX/UI metadata entry.
	 *
	 * @param uxui UX/UI wrapper
	 * @return this builder
	 */
	public FluentRouter addUxUi(FluentUxUi uxui) {
		router.getUxUis().add(uxui.get());
		return this;
	}

	/**
	 * Removes a UX/UI metadata entry by name.
	 *
	 * @param name UX/UI name
	 * @return this builder
	 */
	public FluentRouter removeUxUi(String name) {
		router.getUxUis().removeIf(u -> name.equals(u.getName()));
		return this;
	}

	/**
	 * Finds a UX/UI entry by name.
	 *
	 * @param name UX/UI name
	 * @return matching wrapper, or {@code null}
	 */
	public FluentUxUi findUxUi(String name) {
		UxUiMetadata result = router.getUxUis().stream().filter(u -> name.equals(u.getName())).findAny().orElse(null);
		if (result != null) {
			return new FluentUxUi(result);
		}
		return null;
	}

	/**
	 * Removes all UX/UI metadata entries.
	 *
	 * @return this builder
	 */
	public FluentRouter clearUxUis() {
		router.getUxUis().clear();
		return this;
	}

	/**
	 * Adds an unsecured URL prefix.
	 *
	 * @param unsecuredUrlPrefix URL prefix that bypasses authentication
	 * @return this builder
	 */
	public FluentRouter addUnsecuredUrlPrefix(String unsecuredUrlPrefix) {
		router.getUnsecuredUrlPrefixes().add(unsecuredUrlPrefix);
		return this;
	}

	/**
	 * Removes an unsecured URL prefix.
	 *
	 * @param unsecuredUrlPrefix URL prefix to remove
	 * @return this builder
	 */
	public FluentRouter removeUnsecuredUrlPrefix(String unsecuredUrlPrefix) {
		router.getUnsecuredUrlPrefixes().remove(unsecuredUrlPrefix);
		return this;
	}

	/**
	 * Removes all unsecured URL prefixes.
	 *
	 * @return this builder
	 */
	public FluentRouter clearUnsecuredUrlPrefixes() {
		router.getUnsecuredUrlPrefixes().clear();
		return this;
	}

	/**
	 * Returns the mutable metadata instance being built.
	 *
	 * @return backing router metadata
	 */
	public Router get() {
		return router;
	}
}
