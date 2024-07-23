package org.skyve.metadata.view.model.comparison;

import org.skyve.domain.Bean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.view.model.ViewModel;

public abstract class ComparisonModel<T extends Bean, C extends Bean> implements ViewModel {
	private T bean;
	public T getBean() {
		return bean;
	}
	public void setBean(T bean) {
		this.bean = bean;
	}

	@Override
	public void postConstruct(Customer customer, boolean runtime) {
		// nothing to see here
	}

	public abstract ComparisonComposite getComparisonComposite(C toCompareTo) throws Exception;
}
