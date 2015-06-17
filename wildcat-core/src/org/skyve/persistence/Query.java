package org.skyve.persistence;

import java.util.List;
import java.util.Map;

import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;

/**
 * 
 */
public interface Query {
	public Map<String, Object> getParameters();
	public Query putParameter(String name, Object value);
	
	public <T extends Bean> List<T> beanResults() throws DomainException;
	public <T extends Bean> T beanResult() throws DomainException;
	public <T extends Bean> AutoClosingIterable<T> beanIterable() throws DomainException;
	public <T extends Bean> List<T> projectedResults() throws DomainException;
	public <T extends Bean> T projectedResult() throws DomainException;
	public <T extends Bean> AutoClosingIterable<T> projectedIterable() throws DomainException;
	public <T extends Object> List<T> scalarResults(Class<T> type) throws DomainException;
	public <T extends Object> T scalarResult(Class<T> type) throws DomainException;
	public <T extends Object> AutoClosingIterable<T> scalarIterable(Class<T> type) throws DomainException;
	public List<Object[]> tupleResults() throws DomainException;
	public Object[] tupleResult() throws DomainException;
	public AutoClosingIterable<Object[]> tupleIterable() throws DomainException;
}
