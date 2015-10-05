package org.skyve.wildcat.persistence;

import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;
import org.skyve.persistence.SQL;

public abstract class AbstractSQL extends AbstractQuery implements SQL {
	private String query = null;
	private String moduleName;
	private String documentName;

	private Map<String, AttributeType> parametersTypes = new TreeMap<>();

	public AbstractSQL(String query) {
		this.query = query;
	}

	public AbstractSQL(String moduleName,
							String documentName,
							String query) {
		this.moduleName = moduleName;
		this.documentName = documentName;
		this.query = query;
	}

	public AbstractSQL(Document document,
							String query) {
		this.moduleName = document.getOwningModuleName();
		this.documentName = document.getName();
		this.query = query;
	}
 
	@Override
	public AbstractSQL putParameter(String name, Object value, AttributeType type) {
		super.putParameter(name, value);
		parametersTypes.put(name, type);
		return this;
	}
	
	@Override
	public AbstractSQL putParameter(String name, Object value) {
		super.putParameter(name, value);
		return this;
	}
	
	public final AttributeType getParameterType(String name) {
		return parametersTypes.get(name);
	}
	
	@Override
	public String toQueryString() {
		return query;
	}
	
	public String getModuleName() {
		return moduleName;
	}
	
	public String getDocumentName() {
		return documentName;
	}
	
	@Override
	public final <T extends Bean> T beanResult() throws DomainException {
		List<T> results = beanResults();
		return AbstractQuery.returnOneResult(results);
	}

	@Override
	public final <T extends Bean> T retrieveBean() throws DomainException {
		List<T> results = beanResults();
		return AbstractQuery.assertOneResult(results);
	}

	@Override
	public final <T> T scalarResult(Class<T> type) throws DomainException {
		List<T> results = scalarResults(type);
		return AbstractQuery.returnOneResult(results);
	}

	@Override
	public final <T> T retrieveScalar(Class<T> type) throws DomainException {
		List<T> results = scalarResults(type);
		return AbstractQuery.assertOneResult(results);
	}

	@Override
	public final Object[] tupleResult() throws DomainException {
		List<Object[]> results = tupleResults();
		return AbstractQuery.returnOneResult(results);
	}

	@Override
	public final Object[] retrieveTuple() throws DomainException {
		List<Object[]> results = tupleResults();
		return AbstractQuery.assertOneResult(results);
	}
}
