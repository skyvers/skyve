package org.skyve.impl.dataaccess.sql;

import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.beanutils.DynaBean;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.SkyveException;
import org.skyve.domain.messages.TimeoutException;
import org.skyve.impl.persistence.AbstractSQL;
import org.skyve.impl.persistence.DynaIterable;
import org.skyve.impl.persistence.NamedParameterPreparedStatement;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.persistence.AutoClosingIterable;

class SQLDataAccessSQL extends AbstractSQL {
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
		try (SQLIterable<T> iterable = new SQLIterable<>(document, dataAccess, this, null)) {
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
		
		return new SQLIterable<>(document, dataAccess, this, null);
	}

	@Override
	public <T> List<T> scalarResults(Class<T> type) {
		List<T> results = new ArrayList<>(100);
		try (SQLIterable<T> iterable = new SQLIterable<>(null, dataAccess, this, type)) {
			for (T result : iterable) {
				results.add(result);
			}
		}

		return results;
	}

	@Override
	public <T> AutoClosingIterable<T> scalarIterable(Class<T> type) {
		return new SQLIterable<>(null, dataAccess, this, type);
	}

	@Override
	public List<Object[]> tupleResults() {
		List<Object[]> results = new ArrayList<>(100);
		try (SQLIterable<Object[]> iterable = new SQLIterable<>(null, dataAccess, this, null)) {
			for (Object[] result : iterable) {
				results.add(result);
			}
		}

		return results;
	}

	@Override
	public AutoClosingIterable<Object[]> tupleIterable() {
		return new SQLIterable<>(null, dataAccess, this, null);
	}

	@Override
	public List<DynaBean> dynaResults() {
		try {
			try (NamedParameterPreparedStatement ps = new NamedParameterPreparedStatement(dataAccess.getConnection(), toQueryString())) {
				prepareStatement(ps, dataAccess.dataStore, dataAccess.getDialect());
				try (ResultSet rs = ps.executeQuery()) {
					return dynaList(rs);
				}
			}
		}
		catch (TimeoutException e) {
			throw e;
		}
		catch (SkyveException e) {
			throw e;
		}
		catch (Exception e) {
			throw new DomainException(e);
		}
	}
	
	@Override
	@SuppressWarnings("resource")
	public AutoClosingIterable<DynaBean> dynaIterable() {
		try {
			return new DynaIterable(dataAccess.getConnection(), this, dataAccess.dataStore, dataAccess.getDialect());
		}
		catch (TimeoutException e) {
			throw e;
		}
		catch (SkyveException e) {
			throw e;
		}
		catch (Exception e) {
			throw new DomainException(e);
		}
	}
	
	@Override
	public int execute() {
		// TODO Auto-generated method stub
		return 0;
	}
}
