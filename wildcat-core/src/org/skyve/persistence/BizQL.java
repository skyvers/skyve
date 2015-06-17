package org.skyve.persistence;

/**
 * 
 */
public interface BizQL extends Query, PagedQuery, DMLQuery {
	@Override
	public BizQL putParameter(String name, Object value);
	@Override
	public BizQL setFirstResult(int first);
	@Override
	public BizQL setMaxResults(int max);
}
