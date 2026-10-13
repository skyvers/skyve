package org.skyve.metadata.view.fluent;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.view.widget.bound.input.InputWidget;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Provides fluent operations shared by input widget metadata builders.
 *
 * @param <T> the concrete fluent builder type
 */
public abstract class FluentInputWidget<T extends FluentInputWidget<T>>  extends FluentBoundWidget<T> {
	/**
	 * Creates a new fluent input-widget builder.
	 */
	protected FluentInputWidget() {
		// nothing to see
	}
	
	/**
	 * Copies shared input-widget state from the source metadata instance.
	 */
	@SuppressWarnings("unchecked")
	protected T from(InputWidget input) {
		super.from(input);
		BindUtil.copyAuthoredCondition(input.getXmlDisabledCondition(), input.getXmlEnabledCondition(), this::disabledCondition, this::enabledCondition);
		BindUtil.copyAuthoredCondition(input.getXmlInvisibleCondition(), input.getXmlVisibleCondition(), this::invisibleCondition, this::visibleCondition);
		return (T) this;
	}

	/**
	 * Sets the {@code disabled} condition as authored; the opposite sense of {@link #enabledCondition(String)}.
	 *
	 * @param disabledCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public @Nonnull T disabledCondition(@Nullable String disabledCondition) {
		get().setXmlDisabledCondition(disabledCondition);
		return (T) this;
	}

	/**
	 * Sets the {@code enabled} condition as authored; the opposite sense of {@link #disabledCondition(String)}.
	 *
	 * @param enabledCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public @Nonnull T enabledCondition(@Nullable String enabledCondition) {
		get().setXmlEnabledCondition(enabledCondition);
		return (T) this;
	}

	/**
	 * Sets the {@code invisible} condition as authored; the opposite sense of {@link #visibleCondition(String)}.
	 *
	 * @param invisibleCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public @Nonnull T invisibleCondition(@Nullable String invisibleCondition) {
		get().setXmlInvisibleCondition(invisibleCondition);
		return (T) this;
	}

	/**
	 * Sets the {@code visible} condition as authored; the opposite sense of {@link #invisibleCondition(String)}.
	 *
	 * @param visibleCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public @Nonnull T visibleCondition(@Nullable String visibleCondition) {
		get().setXmlVisibleCondition(visibleCondition);
		return (T) this;
	}

	/**
	 * Returns the wrapped {@link InputWidget} metadata instance.
	 *
	 * @return the mutable input-widget metadata being configured
	 */
	@Override
	public abstract InputWidget get();
}
