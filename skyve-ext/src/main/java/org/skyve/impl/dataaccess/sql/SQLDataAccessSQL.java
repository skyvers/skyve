package org.skyve.impl.dataaccess.sql;

import java.util.ArrayList;
import java.util.List;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.persistence.AbstractQuery;
import org.skyve.impl.persistence.AbstractSQL;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.ProjectedQuery;

class SQLDataAccessSQL extends AbstractSQL implements ProjectedQuery {
	private SQLDataAccessImpl dataAccess;
	private Document document;
	
	SQLDataAccessSQL(Document document, String query, SQLDataAccessImpl dataAccess) {
		super(document, query);
		this.document = document;
		this.dataAccess = dataAccess;
	}

	SQLDataAccessSQL(String moduleName, String documentName, String query, SQLDataAccessImpl dataAccess) {
		super(moduleName, documentName, query);
		Customer customer = CORE.getUser().getCustomer();
		this.document = customer.getModule(moduleName).getDocument(customer, documentName);
		this.dataAccess = dataAccess;
	}

	SQLDataAccessSQL(String query, SQLDataAccessImpl dataAccess) {
		super(query);
		this.dataAccess = dataAccess;
	}

	@Override
	public <T extends Bean> List<T> beanResults() {
		if (document == null) {
			throw new DomainException("The document must be set to create beans from SQL");
		}
		
		List<T> results = new ArrayList<>(100);
		try (SQLIterable<T> iterable = new SQLIterable<>(document, dataAccess, this, null, timeoutInSeconds)) {
			for (T result : iterable) {
				results.add(result);
			}
		}

		return results;
	}

	@Override
	public <T extends Bean> AutoClosingIterable<T> beanIterable() {
		if (document == null) {
			throw new DomainException("The document must be set to create beans from SQL");
		}
		
		return new SQLIterable<>(document, dataAccess, this, null, timeoutInSeconds);
	}

	@Override
	public <T extends Bean> List<T> projectedResults() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public final <T extends Bean> T projectedResult() {
		List<T> results = projectedResults();
		return AbstractQuery.returnOneResult(results);
	}

	@Override
	public final <T extends Bean> T retrieveProjected() {
		List<T> results = projectedResults();
		return AbstractQuery.assertOneResult(results);
	}

	@Override
	public <T extends Bean> AutoClosingIterable<T> projectedIterable() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public <T> List<T> scalarResults(Class<T> type) {
		List<T> results = new ArrayList<>(100);
		try (SQLIterable<T> iterable = new SQLIterable<>(null, dataAccess, this, type, timeoutInSeconds)) {
			for (T result : iterable) {
				results.add(result);
			}
		}

		return results;
	}

	@Override
	public <T> AutoClosingIterable<T> scalarIterable(Class<T> type) {
		return new SQLIterable<>(null, dataAccess, this, type, timeoutInSeconds);
	}

	@Override
	public List<Object[]> tupleResults() {
		List<Object[]> results = new ArrayList<>(100);
		try (SQLIterable<Object[]> iterable = new SQLIterable<>(null, dataAccess, this, null, timeoutInSeconds)) {
			for (Object[] result : iterable) {
				results.add(result);
			}
		}

		return results;
	}

	@Override
	public AutoClosingIterable<Object[]> tupleIterable() {
		return new SQLIterable<>(null, dataAccess, this, null, timeoutInSeconds);
	}

	@Override
	public int execute() {
		// TODO Auto-generated method stub
		return 0;
	}
}
