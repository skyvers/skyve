package org.skyve.persistence;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * 
 */
public interface BizQL extends BeanQuery, ProjectedQuery, ScalarQuery, TupleQuery, PagedQuery, DMLQuery {
	@Nonnull BizQL putParameter(@Nonnull String name, @Nullable Object value);
	
	@Override
	@Nonnull BizQL setFirstResult(int first);
	
	@Override
	@Nonnull BizQL setMaxResults(int max);
	
	int getTimeoutInSeconds();
	void setTimeoutInSeconds(int timeoutInSeconds);
	@Nonnull BizQL noTimeout();
}
