package org.skyve.impl.persistence.hibernate;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.hibernate.ScrollMode;
import org.hibernate.ScrollableResults;
import org.hibernate.Session;
import org.hibernate.query.Query;
import org.skyve.domain.MapBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.persistence.AbstractQuery;
import org.skyve.impl.util.UtilImpl;
import org.skyve.persistence.AutoClosingIterable;

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
	
	<T> Query<T> createHibernateQuery(AbstractQuery query) {
		// This needs to be be before we set the driving document (below)
		// as it sets the driving document in a BizQL
		String queryString = query.toQueryString();
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(queryString + " executed on thread " + Thread.currentThread());

		drivingModuleName = query.getDrivingModuleName();
		drivingDocumentName = query.getDrivingDocumentName();
		

		Query<T> result = session.createQuery(queryString);
		if (firstResult >= 0) {
			result.setFirstResult(firstResult);
		}
		if (maxResults > 0) {
			result.setMaxResults(maxResults);
		}

		for (String parameterName : query.getParameterNames()) {
			Object value = query.getParameter(parameterName);
			if (value instanceof Collection) {
				result.setParameterList(parameterName, (Collection<?>) value);
			}
			else if ((value != null) && value.getClass().isArray()) {
				result.setParameterList(parameterName, (Object[]) value);
			}
			else if (value instanceof Geometry) {
				result.setParameter(parameterName, value, AbstractHibernatePersistence.getDialect().getGeometryType());
			}
			else {
				result.setParameter(parameterName, value);
			}
		}

		return result;
	}
	
	@SuppressWarnings("unchecked")
	<T> List<T> list(Query<T> query, boolean asIs, boolean assertSingle, boolean assertMultiple) {
		try {
			String[] returnAliases = query.getReturnAliases();
			if ((returnAliases == null) || (returnAliases.length == 0)) {
				throw new DomainException("There should be at least 1 projected value in the query");
			}

			if (asIs) {
				if (assertSingle && (returnAliases.length != 1)) {
					throw new DomainException("There should be only 1 projected value in the query");
				}
				else if (assertMultiple && (returnAliases.length <= 1)) {
					throw new DomainException("There should be more than 1 projected value in the query");
				}
				return query.list();
			}

			// Replace bogus _ property names with the dot
			String[] aliases = returnAliases.clone();
			for (int i = 0, length = aliases.length; i < length; i++) {
				aliases[i] = BindUtil.unsanitiseBinding(aliases[i]);
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

	@SuppressWarnings("resource")
	<T> AutoClosingIterable<T> iterate(Query<T> query, boolean asIs, boolean assertSingle, boolean assertMultiple) {
		try {
			ScrollableResults results = query.scroll(ScrollMode.FORWARD_ONLY);

			String[] returnAliases = query.getReturnAliases();
			if ((returnAliases == null) || (returnAliases.length == 0)) {
				throw new DomainException("There should be at least 1 projected value in the query");
			}

			if (asIs) {
				if (assertSingle && (returnAliases.length != 1)) {
					throw new DomainException("There should be only 1 projected value in the query");
				}
				else if (assertMultiple && (returnAliases.length <= 1)) {
					throw new DomainException("There should be more than 1 projected value in the query");
				}
				return new HibernateAutoClosingIterable<>(results, false, false);
			}

			// Replace bogus _ property names with the dot
			String[] aliases = returnAliases.clone();
			for (int i = 0, length = aliases.length; i < length; i++) {
				aliases[i] = BindUtil.unsanitiseBinding(aliases[i]);
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
		Query<?> hibernateQuery = session.createQuery(query.toQueryString());
		for (String parameterName : query.getParameterNames()) {
			Object value = query.getParameter(parameterName);
			if (value instanceof Collection) {
				hibernateQuery.setParameterList(parameterName, (Collection<?>) value);
			}
			else if ((value != null) && value.getClass().isArray()) {
				hibernateQuery.setParameterList(parameterName, (Object[]) value);
			}
			if (value instanceof Geometry) {
				hibernateQuery.setParameter(parameterName, value, AbstractHibernatePersistence.getDialect().getGeometryType());
			}
			else {
				hibernateQuery.setParameter(parameterName, value);
			}
		}

		return hibernateQuery.executeUpdate();
	}
}
