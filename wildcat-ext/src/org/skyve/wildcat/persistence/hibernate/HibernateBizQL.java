package org.skyve.wildcat.persistence.hibernate;

import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.hibernate.Query;
import org.hibernate.Session;
import org.skyve.domain.Bean;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.BizQL;
import org.skyve.wildcat.persistence.AbstractBizQL;

public class HibernateBizQL extends AbstractBizQL {
	private AbstractHibernatePersistence persistence;
	
	public HibernateBizQL(String query, 
							AbstractHibernatePersistence persistence) {
		super(query);
		this.persistence = persistence;
	}

	@Override
	public <T extends Bean> List<T> beanResults(Class<T> type) {
		// TODO Auto-generated method stub
		return super.beanResults(type);
	}

	@Override
	public <T extends Bean> AutoClosingIterable<T> beanIterable(Class<T> type) {
		// TODO Auto-generated method stub
		return super.beanIterable(type);
	}

	@Override
	public <T extends Bean> List<T> projectedResults() {
		// TODO Auto-generated method stub
		return super.projectedResults();
	}

	@Override
	public <T extends Bean> AutoClosingIterable<T> projectedIterable() {
		// TODO Auto-generated method stub
		return super.projectedIterable();
	}

	@Override
	public <T> List<T> scalarResults(Class<T> type) {
		// TODO Auto-generated method stub
		return super.scalarResults(type);
	}

	@Override
	public <T> AutoClosingIterable<T> scalarIterable(Class<T> type) {
		// TODO Auto-generated method stub
		return super.scalarIterable(type);
	}

	@Override
	public List<Object[]> tupleResults() {
		// TODO Auto-generated method stub
		return super.tupleResults();
	}

	@Override
	public AutoClosingIterable<Object[]> tupleIterable() {
		// TODO Auto-generated method stub
		return super.tupleIterable();
	}

	@Override
	public int execute() {
		Session session = persistence.getSession();
		Query query = session.createQuery(toQueryString());
		Map<String, Object> parameters = getParameters();
		if (parameters != null) {
			for (Entry<String, Object> entry : parameters.entrySet()) {
				query.setParameter(entry.getKey(), entry.getValue());
			}
		}

		return query.executeUpdate();
	}

	@Override
	public BizQL setFirstResult(int first) {
		// TODO Auto-generated method stub
		return super.setFirstResult(first);
	}

	@Override
	public BizQL setMaxResults(int max) {
		// TODO Auto-generated method stub
		return super.setMaxResults(max);
	}
}
