package org.skyve.persistence;

public interface PagedQuery {
	public PagedQuery setFirstResult(int first);
	public PagedQuery setMaxResults(int max);
}
