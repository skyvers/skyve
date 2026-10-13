package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.FilterParameterImpl;
import org.skyve.metadata.FilterOperator;
import org.skyve.metadata.view.widget.FilterParameter;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Builds {@link FilterParameter} metadata for list and query filtering.
 */
public class FluentFilterParameter {
	private FilterParameterImpl filterParameter = null;

	/**
	 * Creates a builder backed by a new {@link FilterParameterImpl}.
	 */
	public FluentFilterParameter() {
		filterParameter = new FilterParameterImpl();
	}

	/**
	 * Creates a builder backed by the supplied {@link FilterParameterImpl}.
	 *
	 * @param filterParameter
	 *            the metadata instance to mutate
	 */
	public FluentFilterParameter(@Nonnull FilterParameterImpl filterParameter) {
		this.filterParameter = filterParameter;
	}

	/**
	 * Copies filter-parameter state from runtime metadata.
	 *
	 * @param filterParameter
	 *            the source metadata to copy
	 * @return this builder
	 */
	public @Nonnull FluentFilterParameter from(@SuppressWarnings("hiding") @Nonnull FilterParameter filterParameter) {
		filterBinding(filterParameter.getFilterBinding());
		value(filterParameter.getValue());
		valueBinding(filterParameter.getValueBinding());
		operator(filterParameter.getOperator());
		return this;
	}

	/**
	 * Sets the filter binding targeted by this parameter.
	 *
	 * @param filterBinding
	 *            the filter binding expression
	 * @return this builder
	 */
	public @Nonnull FluentFilterParameter filterBinding(@Nullable String filterBinding) {
		filterParameter.setFilterBinding(filterBinding);
		return this;
	}

	/**
	 * Sets the literal filter value.
	 *
	 * @param value
	 *            the literal filter value
	 * @return this builder
	 */
	public @Nonnull FluentFilterParameter value(@Nullable String value) {
		filterParameter.setValue(value);
		return this;
	}

	/**
	 * Sets the binding that supplies the filter value.
	 *
	 * @param valueBinding
	 *            the value-binding expression
	 * @return this builder
	 */
	public @Nonnull FluentFilterParameter valueBinding(@Nullable String valueBinding) {
		filterParameter.setValueBinding(valueBinding);
		return this;
	}

	/**
	 * Sets the comparison operator applied to the filter value.
	 *
	 * @param operator
	 *            the filter operator
	 * @return this builder
	 */
	public @Nonnull FluentFilterParameter operator(@Nullable FilterOperator operator) {
		filterParameter.setOperator(operator);
		return this;
	}

	/**
	 * Returns the underlying mutable metadata.
	 *
	 * @return the wrapped filter-parameter metadata
	 */
	public @Nonnull FilterParameterImpl get() {
		return filterParameter;
	}
}
