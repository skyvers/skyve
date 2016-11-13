package org.skyve.impl.persistence.hibernate;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.hibernate.Query;
import org.hibernate.ScrollMode;
import org.hibernate.ScrollableResults;
import org.hibernate.Session;
import org.hibernatespatial.GeometryUserType;
import org.skyve.domain.MapBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.persistence.AbstractQuery;
import org.skyve.impl.util.UtilImpl;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.impl.persistence.hibernate.AbstractHibernatePersistence;
import org.skyve.impl.persistence.hibernate.HibernateAutoClosingIterable;

import com.vividsolutions.jts.geom.Geometry;

class HibernateQueryDelegate {
	private Session session;
	private int firstResult = Integer.MIN_VALUE;
	private int maxResults = Integer.MIN_VALUE;
	private String drivingModuleName;
	private String drivingDocumentName;
	
	HibernateQueryDelegate(AbstractHibernatePersistence persistence) {
		this.session = persistence.getSession();
	}
	
	void setFirstResult(int first) {
		firstResult = first;
	}
	
	void setMaxResults(int max) {
		maxResults = max;
	}
	
	Query createHibernateQuery(AbstractQuery query) {
		// This needs to be be before we set the driving document (below)
		// as it sets the driving document in a BizQL
		String queryString = query.toQueryString();
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(queryString + " executed on thread " + Thread.currentThread());

		drivingModuleName = query.getDrivingModuleName();
		drivingDocumentName = query.getDrivingDocumentName();
		

		Query result = session.createQuery(queryString);
		if (firstResult >= 0) {
			result.setFirstResult(firstResult);
		}
		if (maxResults > 0) {
			result.setMaxResults(maxResults);
		}

		for (String parameterName : query.getParameterNames()) {
			Object value = query.getParameter(parameterName);
			if (value instanceof Geometry) {
				result.setParameter(parameterName, value, GeometryUserType.TYPE);
			}
			else {
				result.setParameter(parameterName, value);
			}
		}

		return result;
	}
	
	@SuppressWarnings("unchecked")
	<T> List<T> list(Query query, boolean asIs, boolean assertSingle, boolean assertMultiple) {
		try {
			if (asIs) {
				if (assertSingle && (query.getReturnAliases().length != 1)) {
					throw new DomainException("There should be only 1 projected value in the query");
				}
				else if (assertMultiple && (query.getReturnAliases().length <= 1)) {
					throw new DomainException("There should be more than 1 projected value in the query");
				}
				return query.list();
			}

			// Replace bogus _ property names with the dot
			String[] aliases = query.getReturnAliases().clone();
			for (int i = 0, length = aliases.length; i < length; i++) {
				aliases[i] = aliases[i].replace('_', '.');
			}

			List<?> results = query.list();
			List<T> beans = new ArrayList<>(results.size());

			for (Object result : results) {
				Map<String, Object> properties = new TreeMap<>();

				if (result instanceof Object[]) {
					Object[] resultArray = (Object[]) result;

					int index = 0;
					while (index < aliases.length) {
						properties.put(aliases[index], resultArray[index]);
						index++;
					}
				}
				else {
					properties.put(aliases[0], result);
				}

				beans.add((T) new MapBean(drivingModuleName, 
											drivingDocumentName, 
											properties));
			}

			return beans;
		}
		catch (Exception e) {
			throw new DomainException(e);
		}
	}

	<T> AutoClosingIterable<T> iterate(Query query, boolean asIs, boolean assertSingle, boolean assertMultiple) {
		try {
			ScrollableResults results = query.scroll(ScrollMode.FORWARD_ONLY);
			if (asIs) {
				if (assertSingle && (query.getReturnAliases().length != 1)) {
					throw new DomainException("There should be only 1 projected value in the query");
				}
				else if (assertMultiple && (query.getReturnAliases().length <= 1)) {
					throw new DomainException("There should be more than 1 projected value in the query");
				}
				return new HibernateAutoClosingIterable<>(results, false, false);
			}

			// Replace bogus _ property names with the dot
			String[] aliases = query.getReturnAliases().clone();
			for (int i = 0, length = aliases.length; i < length; i++) {
				aliases[i] = aliases[i].replace('_', '.');
			}

			return new HibernateAutoClosingIterable<>(drivingModuleName, 
														drivingDocumentName, 
														results, 
														aliases,
														false,
														false);
		}
		catch (Exception e) {
			throw new DomainException(e);
		}
	}
	
	int execute(AbstractQuery query) {
		Query hibernateQuery = session.createQuery(query.toQueryString());
		for (String parameterName : query.getParameterNames()) {
			Object value = query.getParameter(parameterName);
			if (value instanceof Geometry) {
				hibernateQuery.setParameter(parameterName, value, GeometryUserType.TYPE);
			}
			else {
				hibernateQuery.setParameter(parameterName, value);
			}
		}

		return hibernateQuery.executeUpdate();
	}
}
