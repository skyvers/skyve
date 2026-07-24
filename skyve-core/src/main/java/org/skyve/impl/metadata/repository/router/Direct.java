package org.skyve.impl.metadata.repository.router;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.SerializableMetaData;
import org.skyve.web.UserAgentType;

import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlEnum;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Declares one ordered direct rule that selects a named UX/UI for a target path.
 *
 * <p>An absent match mode means exact matching, while an absent user-agent condition is a
 * wildcard. The containing
 * {@link Router} evaluates declarations in effective list order, so duplicate and overlapping
 * declarations are retained and the first complete match wins.
 *
 * <p>Threading: not thread-safe. Instances are mutable during metadata construction and are
 * treated as read-only after the containing router is published.
 *
 * @since 10.0
 */
@XmlType(namespace = XMLMetaData.ROUTER_NAMESPACE)
public class Direct implements SerializableMetaData {
	private static final long serialVersionUID = -1524538009980718185L;

	/**
	 * Defines how a direct declaration compares its path with a request target.
	 *
	 * <p>Threading: enum values are immutable and thread-safe.
	 *
	 * @since 10.0
	 */
	@XmlEnum
	@XmlType(namespace = XMLMetaData.ROUTER_NAMESPACE)
	@SuppressWarnings("java:S115") // XML metadata values intentionally use lower-case names.
	public enum DirectMatch {
		/** Matches only an identical normalised target path. */
		exact,
		/** Matches normalised target paths beginning with the declared path. */
		prefix
	}

	private String path;
	private String uxui;
	private DirectMatch match;
	private UserAgentType userAgentType;

	/**
	 * Returns the normalized context-relative target path.
	 *
	 * @return the trimmed path, or {@code null} before a required value is supplied
	 * @since 10.0
	 */
	public @Nullable String getPath() {
		return path;
	}

	/**
	 * Sets the context-relative target path after trimming blank input to {@code null}.
	 *
	 * @param path the path to store; may be {@code null} until conversion validates the declaration
	 * @since 10.0
	 */
	@XmlAttribute(required = true)
	public void setPath(@Nullable String path) {
		this.path = UtilImpl.processStringValue(path);
	}

	/**
	 * Returns the name of the effective router UX/UI selected by this declaration.
	 *
	 * @return the trimmed UX/UI name, or {@code null} before a required value is supplied
	 * @since 10.0
	 */
	public @Nullable String getUxui() {
		return uxui;
	}

	/**
	 * Sets the target UX/UI name after trimming blank input to {@code null}.
	 *
	 * @param uxui the effective router UX/UI name; may be {@code null} until conversion validates
	 *             the declaration
	 * @since 10.0
	 */
	@XmlAttribute(required = true)
	public void setUxui(@Nullable String uxui) {
		this.uxui = UtilImpl.processStringValue(uxui);
	}

	/**
	 * Returns the path comparison mode.
	 *
	 * <p>A {@code null} value is retained so XML marshalling does not emit a default attribute;
	 * consumers interpret it as {@link DirectMatch#exact}.
	 *
	 * @return the explicitly declared comparison mode, or {@code null} for exact matching
	 * @since 10.0
	 */
	public @Nullable DirectMatch getMatch() {
		return match;
	}

	/**
	 * Sets the path comparison mode.
	 *
	 * @param match the explicitly declared comparison mode, or {@code null} for exact matching
	 * @since 10.0
	 */
	@XmlAttribute
	public void setMatch(@Nullable DirectMatch match) {
		this.match = match;
	}

	/**
	 * Returns the optional user-agent condition.
	 *
	 * @return the required device category, or {@code null} to match every category
	 * @throws IllegalArgumentException if JAXB supplied an unsupported value and the containing
	 *                                  router has not yet been converted
	 * @since 10.0
	 */
	public @Nullable UserAgentType getUserAgentType() {
		return userAgentType;
	}

	/**
	 * Sets the optional user-agent condition.
	 *
	 * @param userAgentType the device category to require, or {@code null} for a wildcard
	 * @since 10.0
	 */
	@XmlAttribute
	public void setUserAgentType(@Nullable UserAgentType userAgentType) {
		this.userAgentType = userAgentType;
	}
}
