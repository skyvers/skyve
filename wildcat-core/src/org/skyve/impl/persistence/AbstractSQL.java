package org.skyve.impl.persistence;

import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Decimal;
import org.skyve.domain.types.Enumeration;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;
import org.skyve.persistence.SQL;
import org.skyve.impl.persistence.AbstractQuery;
import org.skyve.impl.persistence.AbstractSQL;

import com.vividsolutions.jts.geom.Geometry;

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
	public SQL putParameter(String name, DateOnly value) {
		return putParameter(name, value, AttributeType.date);
	}

	@Override
	public SQL putParameter(String name, DateTime value) {
		return putParameter(name, value, AttributeType.dateTime);
	}

	@Override
	public SQL putParameter(String name, TimeOnly value) {
		return putParameter(name, value, AttributeType.time);
	}

	@Override
	public SQL putParameter(String name, Timestamp value) {
		return putParameter(name, value, AttributeType.timestamp);
	}

	@Override
	public SQL putParameter(String name, Decimal value) {
		return putParameter(name, value, AttributeType.decimal10);
	}

	@Override
	public SQL putParameter(String name, Integer value) {
		return putParameter(name, value, AttributeType.integer);
	}

	@Override
	public SQL putParameter(String name, Long value) {
		return putParameter(name, value, AttributeType.longInteger);
	}

	@Override
	public SQL putParameter(String name, String value, boolean memo) {
		return putParameter(name, value, memo ? AttributeType.memo : AttributeType.text);
	}

	@Override
	public SQL putParameter(String name, Bean value) {
		return putParameter(name, (value == null) ? null : value.getBizId(), AttributeType.id);
	}

	@Override
	public SQL putParameter(String name, Geometry value) {
		return putParameter(name, value, AttributeType.geometry);
	}

	@Override
	public SQL putParameter(String name, Boolean value) {
		return putParameter(name, value, AttributeType.bool);
	}

	@Override
	public SQL putParameter(String name, Enumeration value) {
		return putParameter(name, (value == null) ? null : value.toCode(), AttributeType.enumeration);
	}

	@Override
	public AbstractSQL putParameter(String name, Object value, AttributeType type) {
		parameters.put(name, value);
		parametersTypes.put(name, type);
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
