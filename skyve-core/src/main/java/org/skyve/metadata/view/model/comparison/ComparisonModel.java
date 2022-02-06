package org.skyve.metadata.view.model.comparison;

import org.skyve.domain.Bean;
import org.skyve.metadata.MetaData;

public abstract class ComparisonModel<T extends Bean, C extends Bean> implements MetaData {
	private T bean;
	public T getBean() {
		return bean;
	}
	public void setBean(T bean) {
		this.bean = bean;
	}
	
	public abstract ComparisonComposite getComparisonComposite(C toCompareTo) throws Exception;
}
