package org.skyve.persistence;

import java.util.List;

import org.skyve.domain.Bean;

public interface BeanQuery {

	/**
	 * Returns a list of all the beans which match this query.
	 * 
	 * @see org.skyve.impl.persistence.AbstractBizQL#beanResults()
	 * @return The list of beans that match the query
	 */
	public <T extends Bean> List<T> beanResults();
	
	/**
     * Returns the first bean result from this query, or null if there are no
     * results.
     * 
     * @see org.skyve.impl.persistence.AbstractQuery#returnOneResult(List)
     * @return The first bean that matches the query, or null
     */
	public <T extends Bean> T beanResult();
	
	/**
     * Returns exactly one bean result from this query; or throws a
     * NoResultsException or ManyResultsException if there are no or too many results, 
	 * as appropriate.
     * 
     * @see org.skyve.impl.persistence.AbstractQuery#assertOneResult(List)
     * @return One result bean
     */
	public <T extends Bean> T retrieveBean();

	/**
	 * Returns an iterable for all the beans which match this query.
	 * 
	 * @return The iterarable
	 */
	public <T extends Bean> AutoClosingIterable<T> beanIterable();
}
