package org.skyve.metadata.view.model;

import org.skyve.domain.Bean;
import org.skyve.metadata.MetaData;

public interface ComparisonModel<T extends Bean> extends MetaData {
	public ComparisonComposite getComparisonComposite(T bean) throws Exception;
}
