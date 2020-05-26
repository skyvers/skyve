package org.skyve.persistence;

/**
 * 
 */
public interface BizQL extends BeanQuery, ProjectedQuery, ScalarQuery, TupleQuery, PagedQuery, DMLQuery {
	public BizQL putParameter(String name, Object value);
	
	@Override
	public BizQL setFirstResult(int first);
	
	@Override
	public BizQL setMaxResults(int max);
	
	public int getTimeoutInSeconds();
	public void setTimeoutInSeconds(int timeoutInSeconds);
}
