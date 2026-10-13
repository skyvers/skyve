package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.Password;

import jakarta.annotation.Nonnull;

/**
 * Builds {@link Password} widget metadata using a fluent API.
 */
@SuppressWarnings("java:S110") // This inheritance-depth warning is ridiculous for intentional framework hierarchies.
public class FluentPassword extends FluentChangeableInputWidget<FluentPassword> implements FluentAbsoluteWidth<FluentPassword> {
	private Password password = null;

	/**
	 * Creates a builder backed by a new {@link Password}.
	 */
	public FluentPassword() {
		password = new Password();
	}

	/**
	 * Creates a builder backed by the supplied {@link Password}.
	 *
	 * @param password
	 *            the metadata instance to mutate
	 */
	public FluentPassword(Password password) {
		this.password = password;
	}

	/**
	 * Copies password metadata into this fluent builder.
	 *
	 * @param password
	 *            the source metadata to copy
	 * @return this builder
	 */
	public FluentPassword from(@SuppressWarnings("hiding") Password password) {
		absoluteWidth(password, this);
		super.from(password);
		password.getProperties().entrySet().forEach(p -> putProperty(p.getKey(), p.getValue()));

		return this;
	}

	/**
	 * Sets the pixel width of this password widget.
	 *
	 * @param width
	 *            the pixel width
	 * @return this builder
	 */
	@Override
	public FluentPassword pixelWidth(int width) {
		password.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	/**
	 * Adds or replaces a custom property on the wrapped metadata.
	 * Properties keep the order in which they were first added.
	 *
	 * @param k the property name
	 * @param v the property value
	 * @return this builder
	 */
	public @Nonnull FluentPassword putProperty(@Nonnull String k, @Nonnull String v) {
		password.getProperties().put(k, v);
		return this;
	}

	/**
	 * Returns the underlying mutable metadata.
	 *
	 * @return the wrapped password metadata
	 */
	@Override
	public Password get() {
		return password;
	}
}
