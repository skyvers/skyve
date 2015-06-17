package org.skyve.wildcat.persistence.hibernate;

import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.hibernate.SQLQuery;
import org.hibernate.Session;
import org.skyve.domain.Bean;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.wildcat.domain.MapBean;
import org.skyve.wildcat.persistence.AbstractSQL;

public class HibernateSQL extends AbstractSQL {
	private AbstractHibernatePersistence persistence;

	public HibernateSQL(String query,
							AbstractHibernatePersistence persistence) {
		super(query);
		this.persistence = persistence;
	}

	@Override
	public <T extends Bean> List<T> beanResults(Class<T> type) {
		return createQueryFromSQL().addEntity(type).list();
	}

	@Override
	public <T extends Bean> AutoClosingIterable<T> beanIterable(Class<T> type) {
		return new HibernateAutoClosingIterable<T>(createQueryFromSQL().addEntity(type).scroll());
	}

	@Override
	public <T extends Bean> List<T> projectedResults() {
		return createQueryFromSQL().list();
	}

	@Override
	public <T extends Bean> AutoClosingIterable<T> projectedIterable() {
		SQLQuery query = createQueryFromSQL();
		return new HibernateAutoClosingIterable<T>(getDrivingModuleName(), 
													getDrivingDocumentName(), 
													query.scroll(),
													query.getReturnAliases());
	}

	@Override
	public <T> List<T> scalarResults(Class<T> type) {
		return createQueryFromSQL().list();
	}

	@Override
	public <T> AutoClosingIterable<T> scalarIterable(Class<T> type) {
		return new HibernateAutoClosingIterable<T>(createQueryFromSQL().scroll());
	}

	@Override
	public List<Object[]> tupleResults() {
		return createQueryFromSQL().list();
	}

	@Override
	public AutoClosingIterable<Object[]> tupleIterable() {
		return new HibernateAutoClosingIterable<Object[]>(createQueryFromSQL().scroll());
	}

	@Override
	public int execute() {
		SQLQuery query = createQueryFromSQL();
		return query.executeUpdate();
	}
	
	private SQLQuery createQueryFromSQL() {
		Session session = persistence.getSession();
		SQLQuery result = session.createSQLQuery(toQueryString());

		Map<String, Object> parameters = getParameters();
		if (parameters != null) {
			for (Entry<String, Object> entry : parameters.entrySet()) {
				result.setParameter(entry.getKey(), entry.getValue());
			}
		}
		
		return result;
	}
}
