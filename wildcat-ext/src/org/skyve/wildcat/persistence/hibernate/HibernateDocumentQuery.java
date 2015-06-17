package org.skyve.wildcat.persistence.hibernate;

import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.model.document.Document;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.DocumentQuery;
import org.skyve.wildcat.persistence.AbstractDocumentQuery;

public class HibernateDocumentQuery extends AbstractDocumentQuery {
	private AbstractHibernatePersistence persistence;
	
	public HibernateDocumentQuery(Bean queryByExampleBean, AbstractHibernatePersistence persistence)
	throws Exception {
		super(queryByExampleBean);
		this.persistence = persistence;
	}

	public HibernateDocumentQuery(Document document,
									String fromClause,
									String filterClause,
									AbstractHibernatePersistence persistence) {
		super(document, fromClause, filterClause);
		this.persistence = persistence;
	}

	public HibernateDocumentQuery(Document document, AbstractHibernatePersistence persistence) {
		super(document);
		this.persistence = persistence;
	}

	public HibernateDocumentQuery(String moduleName, 
									String documentName,
									AbstractHibernatePersistence persistence)
	throws MetaDataException {
		super(moduleName, documentName);
		this.persistence = persistence;
	}

	@Override
	public DocumentQuery setFirstResult(int first) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public DocumentQuery setMaxResults(int max) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public <T extends Bean> List<T> beanResults(Class<T> type) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public <T extends Bean> AutoClosingIterable<T> beanIterable(Class<T> type) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public <T extends Bean> List<T> projectedResults() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public <T extends Bean> AutoClosingIterable<T> projectedIterable() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public <T> List<T> scalarResults(Class<T> type) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public <T> AutoClosingIterable<T> scalarIterable(Class<T> type) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public List<Object[]> tupleResults() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public AutoClosingIterable<Object[]> tupleIterable() {
		// TODO Auto-generated method stub
		return null;
	}

}
