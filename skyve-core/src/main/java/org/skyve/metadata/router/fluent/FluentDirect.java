package org.skyve.metadata.router.fluent;

import org.skyve.impl.metadata.repository.router.Direct;
import org.skyve.impl.metadata.repository.router.Direct.DirectMatch;
import org.skyve.web.UserAgentType;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Builds one mutable ordered direct router declaration.
 *
 * <p>Threading: not thread-safe. Callers must externally synchronize access when a builder is
 * shared.
 *
 * @since 10.0
 */
public class FluentDirect {
	private Direct direct;

	/**
	 * Creates a builder containing a new empty declaration.
	 *
	 * @since 10.0
	 */
	public FluentDirect() {
		direct = new Direct();
	}

	/**
	 * Creates a builder that mutates the supplied declaration directly.
	 *
	 * @param direct backing declaration; must not be {@code null}
	 * @since 10.0
	 */
	public FluentDirect(@Nonnull Direct direct) {
		this.direct = direct;
	}

	/**
	 * Copies every declaration value into this builder.
	 *
	 * <p>Side effects: replaces the builder's scalar declaration values; the source is not retained.
	 *
	 * @param source declaration to copy; must not be {@code null}
	 * @return this builder
	 * @throws NullPointerException if {@code source} is {@code null}
	 * @since 10.0
	 */
	public @Nonnull FluentDirect from(@Nonnull Direct source) {
		path(source.getPath());
		uxui(source.getUxui());
		match(source.getMatch());
		userAgentType(source.getUserAgentType());
		
		return this;
	}

	/**
	 * Sets the normalized declaration path through the metadata setter.
	 *
	 * @param path context-relative target path, or {@code null} until metadata conversion
	 * @return this builder
	 * @since 10.0
	 */
	public @Nonnull FluentDirect path(@Nullable String path) {
		direct.setPath(path);
		return this;
	}

	/**
	 * Sets the target UX/UI name through the metadata setter.
	 *
	 * @param uxui effective router UX/UI name, or {@code null} until metadata conversion
	 * @return this builder
	 * @since 10.0
	 */
	public @Nonnull FluentDirect uxui(@Nullable String uxui) {
		direct.setUxui(uxui);
		return this;
	}

	/**
	 * Sets the declaration's comparison mode.
	 *
	 * @param match comparison mode, or {@code null} to restore exact matching immediately
	 * @return this builder
	 * @since 10.0
	 */
	public @Nonnull FluentDirect match(@Nullable DirectMatch match) {
		direct.setMatch(match);
		return this;
	}

	/**
	 * Sets the optional user-agent condition.
	 *
	 * @param userAgentType required device category, or {@code null} for a wildcard
	 * @return this builder
	 * @since 10.0
	 */
	public @Nonnull FluentDirect userAgentType(@Nullable UserAgentType userAgentType) {
		direct.setUserAgentType(userAgentType);
		return this;
	}
	/**
	 * Returns the mutable declaration owned by this builder.
	 *
	 * @return the backing declaration by identity; never {@code null}
	 * @since 10.0
	 */
	public @Nonnull Direct get() {
		return direct;
	}
}
