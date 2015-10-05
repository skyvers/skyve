package org.skyve.wildcat.dataaccess.sql;

import java.util.ArrayList;
import java.util.List;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.ProjectedQuery;
import org.skyve.wildcat.persistence.AbstractQuery;
import org.skyve.wildcat.persistence.AbstractSQL;

public class SQL extends AbstractSQL implements ProjectedQuery {
	private SQLDataAccess dataAccess;
	private Document document;
	
	SQL(Document document, String query, SQLDataAccess dataAccess) {
		super(document, query);
		this.document = document;
		this.dataAccess = dataAccess;
	}

	SQL(String moduleName, String documentName, String query, SQLDataAccess dataAccess)
	throws MetaDataException {
		super(moduleName, documentName, query);
		Customer customer = CORE.getUser().getCustomer();
		this.document = customer.getModule(moduleName).getDocument(customer, documentName);
		this.dataAccess = dataAccess;
	}

	SQL(String query, SQLDataAccess dataAccess) {
		super(query);
		this.dataAccess = dataAccess;
	}

	@Override
	public SQL putParameter(String name, Object value) {
		super.putParameter(name, value);
		return this;
	}

	@Override
	public SQL putParameter(String name, Object value, AttributeType type) {
		super.putParameter(name, value, type);
		return this;
	}

	@Override
	public <T extends Bean> List<T> beanResults() throws DomainException {
		if (document == null) {
			throw new DomainException("The document must be set to create beans from SQL");
		}
		
		List<T> results = new ArrayList<>(100);
		try (SQLIterable<T> iterable = new SQLIterable<>(document, dataAccess, this, null)) {
			for (T result : iterable) {
				results.add(result);
			}
		}

		return results;
	}

	@Override
	public <T extends Bean> AutoClosingIterable<T> beanIterable()
	throws DomainException {
		if (document == null) {
			throw new DomainException("The document must be set to create beans from SQL");
		}
		
		return new SQLIterable<>(document, dataAccess, this, null);
	}

	@Override
	public <T extends Bean> List<T> projectedResults() throws DomainException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public final <T extends Bean> T projectedResult() throws DomainException {
		List<T> results = projectedResults();
		return AbstractQuery.returnOneResult(results);
	}

	@Override
	public final <T extends Bean> T retrieveProjected() throws DomainException {
		List<T> results = projectedResults();
		return AbstractQuery.assertOneResult(results);
	}

	@Override
	public <T extends Bean> AutoClosingIterable<T> projectedIterable()
	throws DomainException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public <T> List<T> scalarResults(Class<T> type) throws DomainException {
		List<T> results = new ArrayList<>(100);
		try (SQLIterable<T> iterable = new SQLIterable<>(null, dataAccess, this, type)) {
			for (T result : iterable) {
				results.add(result);
			}
		}

		return results;
	}

	@Override
	public <T> AutoClosingIterable<T> scalarIterable(Class<T> type)
	throws DomainException {
		return new SQLIterable<>(null, dataAccess, this, type);
	}

	@Override
	public List<Object[]> tupleResults() throws DomainException {
		List<Object[]> results = new ArrayList<>(100);
		try (SQLIterable<Object[]> iterable = new SQLIterable<>(null, dataAccess, this, null)) {
			for (Object[] result : iterable) {
				results.add(result);
			}
		}

		return results;
	}

	@Override
	public AutoClosingIterable<Object[]> tupleIterable() 
	throws DomainException {
		return new SQLIterable<>(null, dataAccess, this, null);
	}

	@Override
	public int execute() throws DomainException {
		// TODO Auto-generated method stub
		return 0;
	}
}
