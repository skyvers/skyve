package org.skyve.metadata.view.fluent;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.view.reference.Reference;
import org.skyve.impl.metadata.view.reference.ReferenceTarget;
import org.skyve.impl.metadata.view.widget.Link;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Builds {@link Link} widget metadata.
 */
public class FluentLink extends FluentWidget implements FluentAbsoluteWidth<FluentLink> {
	private Link link = null;

	/**
	 * Creates a builder backed by a new {@link Link}.
	 */
	public FluentLink() {
		link = new Link();
	}

	/**
	 * Creates a builder backed by the supplied {@link Link}.
	 *
	 * @param link
	 *            the metadata instance to mutate
	 */
	public FluentLink(Link link) {
		this.link = link;
	}

	/**
	 * Copies link reference, target, value, visibility, and width state.
	 *
	 * @param link
	 *            the source metadata to copy
	 * @return this builder
	 */
	public FluentLink from(@SuppressWarnings("hiding") Link link) {

		reference(link.getReference());
		target(link.getTarget());
		value(link.getValue());
		escapeValue(link.getEscapeValue());
		BindUtil.copyAuthoredCondition(link.getXmlInvisibleCondition(), link.getXmlVisibleCondition(), this::invisibleCondition, this::visibleCondition);

		absoluteWidth(link, this);

		return this;
	}

	/**
	 * Sets the navigation reference resolved when the link is activated.
	 *
	 * @param reference
	 *            the link reference target
	 * @return this builder
	 */
	public FluentLink reference(Reference reference) {
		link.setReference(reference);
		return this;
	}

	/**
	 * Sets how the resolved reference is opened.
	 *
	 * @param target
	 *            the reference target behaviour
	 * @return this builder
	 */
	public FluentLink target(ReferenceTarget target) {
		link.setTarget(target);
		return this;
	}

	/**
	 * Sets the rendered link text or binding value.
	 *
	 * @param value
	 *            the rendered link value
	 * @return this builder
	 */
	public FluentLink value(String value) {
		link.setValue(value);
		return this;
	}

	/**
	 * Sets whether the link value should be escaped before rendering.
	 *
	 * @param escapeValue {@code false} to allow trusted markup; {@code true} to escape at the renderer boundary
	 * @return this builder
	 */
	public FluentLink escapeValue(boolean escapeValue) {
		return escapeValue(escapeValue ? Boolean.TRUE : Boolean.FALSE);
	}

	/**
	 * Sets whether the link value should be escaped before rendering.
	 *
	 * @param escapeValue {@code Boolean.FALSE} to allow trusted markup; {@code null} or {@code Boolean.TRUE} to escape at the renderer boundary
	 * @return this builder
	 */
	public FluentLink escapeValue(Boolean escapeValue) {
		link.setEscapeValue(escapeValue);
		return this;
	}

	/**
	 * Sets the {@code invisible} condition as authored; the opposite sense of {@link #visibleCondition(String)}.
	 *
	 * @param invisibleCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	public @Nonnull FluentLink invisibleCondition(@Nullable String invisibleCondition) {
		link.setXmlInvisibleCondition(invisibleCondition);
		return this;
	}

	/**
	 * Sets the {@code visible} condition as authored; the opposite sense of {@link #invisibleCondition(String)}.
	 *
	 * @param visibleCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	public @Nonnull FluentLink visibleCondition(@Nullable String visibleCondition) {
		link.setXmlVisibleCondition(visibleCondition);
		return this;
	}

	/**
	 * Sets the fixed pixel width for the link widget.
	 *
	 * @param width
	 *            the widget width in pixels
	 * @return this builder
	 */
	@Override
	public FluentLink pixelWidth(int width) {
		link.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	/**
	 * Returns the underlying mutable metadata.
	 *
	 * @return the wrapped link metadata
	 */
	@Override
	public Link get() {
		return link;
	}
}
