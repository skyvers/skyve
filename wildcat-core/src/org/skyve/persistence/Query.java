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
	
	public <T extends Bean> List<T> beanResults(Class<T> type);
	public <T extends Bean> T beanResult(Class<T> type) throws DomainException;
	public <T extends Bean> AutoClosingIterable<T> beanIterable(Class<T> type);
	public <T extends Bean> List<T> projectedResults();
	public <T extends Bean> T projectedResult() throws DomainException;
	public <T extends Bean> AutoClosingIterable<T> projectedIterable();
	public <T extends Object> List<T> scalarResults(Class<T> type);
	public <T extends Object> T scalarResult(Class<T> type) throws DomainException;
	public <T extends Object> AutoClosingIterable<T> scalarIterable(Class<T> type);
	public List<Object[]> tupleResults();
	public Object[] tupleResult() throws DomainException;
	public AutoClosingIterable<Object[]> tupleIterable();
	
/*
	public <T extends Bean> List<T> retrieveInsecureSQLBeans(SQL sql)
	throws DomainException, MetaDataException {
		SQLQuery query = createQueryFromSQL(sql);
		query.addEntity(getDocumentEntityName("admin", "Contact"));
		return query.list();
	}
 */
}
