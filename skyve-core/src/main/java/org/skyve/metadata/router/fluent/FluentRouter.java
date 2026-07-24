package org.skyve.metadata.router.fluent;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import org.skyve.impl.metadata.repository.router.Direct;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.metadata.repository.router.UxUiMetadata;
import org.skyve.util.Util;

import jakarta.annotation.Nonnull;

/**
 * Builds router metadata including UX/UI variants, ordered {@code direct} declarations, and
 * unsecured URL prefixes.
 *
 * <p>Threading: not thread-safe. Callers must externally synchronise access when a builder is
 * shared.
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
	 * Copies router metadata into this builder while retaining source declaration order.
	 *
	 * <p>Side effects: replaces the selector class name and appends copied UX/UI, direct, and
	 * unsecured-prefix metadata to the existing backing router. Direct values are copied into
	 * new declarations; the source declarations are not retained by identity.
	 *
	 * @param router source router metadata; must not be {@code null}
	 * @return this builder
	 * @throws NullPointerException if {@code router} is {@code null}
	 */
	public @Nonnull FluentRouter from(@SuppressWarnings("hiding") @Nonnull Router router) {
		uxuiSelectorClassName(router.getUxuiSelectorClassName());
		for (UxUiMetadata uxui : router.getUxUis()) {
			addUxUi(new FluentUxUi().from(uxui));
		}
		for (Direct direct : router.getDirects()) {
			addDirect(new FluentDirect().from(direct));
		}
		for (String unsecuredUrlPrefix : router.getUnsecuredUrlPrefixes()) {
			addUnsecuredUrlPrefix(unsecuredUrlPrefix);
		}
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
	 * Appends one direct declaration without deduplicating or reordering it.
	 *
	 * @param direct declaration wrapper; must not be {@code null}
	 * @return this builder
	 * @throws NullPointerException if {@code direct} is {@code null}
	 * @since 10.0
	 */
	public @Nonnull FluentRouter addDirect(@Nonnull FluentDirect direct) {
		router.getDirects().add(direct.get());
		return this;
	}

	/**
	 * Finds every direct declaration having the supplied normalised path.
	 *
	 * <p>The result retains declaration order and includes duplicate, exact, prefix, and
	 * conditionally distinct declarations. The returned wrappers mutate the declarations owned by
	 * this router.
	 *
	 * <p>Complexity: O(d) time and O(m) space, where d is the declaration count and m is the number
	 * of matching declarations.
	 *
	 * @param path path to normalise and find
	 * @return a new mutable list of wrappers in declaration order; never {@code null}
	 * @since 10.0
	 */
	public @Nonnull List<FluentDirect> findDirects(@Nonnull String path) {
		String normalisedPath = Util.processStringValue(path);
		List<Direct> directs = router.getDirects();
		List<FluentDirect> result = new ArrayList<>(directs.size());
		for (Direct direct : directs) {
			if (Objects.equals(normalisedPath, direct.getPath())) {
				result.add(new FluentDirect(direct));
			}
		}
		return result;
	}

	/**
	 * Removes every direct declaration having the supplied normalised path.
	 *
	 * <p>Side effects: mutates the backing router while preserving the relative order of all
	 * declarations with other paths.
	 *
	 * <p>Complexity: O(d) time and O(1) auxiliary space, where d is the declaration count.
	 *
	 * @param path path to normalise and remove
	 * @return this builder
	 * @since 10.0
	 */
	public @Nonnull FluentRouter removeDirects(@Nonnull String path) {
		String normalisedPath = Util.processStringValue(path);
		router.getDirects().removeIf(direct -> Objects.equals(normalisedPath, direct.getPath()));
		return this;
	}

	/**
	 * Removes every direct declaration from the backing router.
	 *
	 * <p>Side effects: clears the mutable declaration list.
	 *
	 * @return this builder
	 * @since 10.0
	 */
	public @Nonnull FluentRouter clearDirects() {
		router.getDirects().clear();
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
