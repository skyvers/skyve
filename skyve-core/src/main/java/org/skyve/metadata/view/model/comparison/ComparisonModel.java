package org.skyve.metadata.view.model.comparison;

import org.skyve.domain.Bean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.view.model.ViewModel;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Abstract base for view models that compare two bean instances side-by-side.
 *
 * <p>Subclass {@code ComparisonModel} and implement {@link #getComparisonComposite} to
 * define which properties are shown in the diff view. The framework drives the
 * Skyve comparison widget using the returned {@link ComparisonComposite} tree.
 *
 * @param <T> the driving document bean type (the "current" version)
 * @param <C> the bean type being compared against (the "other" version)
 * @see ComparisonComposite
 * @see ComparisonProperty
 */
public abstract class ComparisonModel<T extends Bean, C extends Bean> implements ViewModel {
	private T bean;
	public @Nullable T getBean() {
		return bean;
	}
	public void setBean(@Nonnull T bean) {
		this.bean = bean;
	}

	@Override
	public void postConstruct(@Nonnull Customer customer, boolean runtime) {
		// nothing to see here
	}

	/**
	 * Returns the comparison composite tree that defines which properties to diff
	 * between the driving bean and {@code toCompareTo}.
	 *
	 * @param toCompareTo the "other" bean to compare the driving bean against; may be
	 *                    a prior version, a template bean, or a peer record
	 * @throws Exception if the comparison cannot be constructed
	 */
	public abstract @Nullable ComparisonComposite getComparisonComposite(@Nullable C toCompareTo) throws Exception;
}
