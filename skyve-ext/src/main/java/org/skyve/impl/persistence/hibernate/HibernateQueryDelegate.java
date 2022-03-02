package org.skyve.impl.persistence.hibernate;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import javax.persistence.QueryTimeoutException;

import org.hibernate.ScrollMode;
import org.hibernate.ScrollableResults;
import org.hibernate.query.Query;
import org.locationtech.jts.geom.Geometry;
import org.skyve.domain.MapBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.SkyveException;
import org.skyve.domain.messages.TimeoutException;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.persistence.AbstractQuery;
import org.skyve.impl.util.UtilImpl;
import org.skyve.persistence.AutoClosingIterable;

class HibernateQueryDelegate {
	private AbstractHibernatePersistence persistence;
	private int firstResult = Integer.MIN_VALUE;
	private int maxResults = Integer.MIN_VALUE;
	private String drivingModuleName;
	private String drivingDocumentName;
	
	HibernateQueryDelegate(AbstractHibernatePersistence persistence) {
		this.persistence = persistence;
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
		

		@SuppressWarnings("resource")
		Query<T> result = persistence.getSession().createQuery(queryString);
		if (firstResult >= 0) {
			result.setFirstResult(firstResult);
		}
		if (maxResults > 0) {
			result.setMaxResults(maxResults);
		}
		
		timeoutQuery(result, query.getTimeoutInSeconds(), persistence.isAsyncThread());
		
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
					throw new DomainException("There should be only 1 projected value in the query when using beanResults() or scalarResults()");
				}
				else if (assertMultiple && (returnAliases.length <= 1)) {
					throw new DomainException("There should be more than 1 projected value in the query when using tupleResults()");
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
		catch (QueryTimeoutException | org.hibernate.QueryTimeoutException e) {
			throw new TimeoutException(e);
		}
		catch (SkyveException e) {
			throw e;
		}
		catch (Exception e) {
			throw new DomainException(e);
		}
	}

	@SuppressWarnings("resource")
	<T> AutoClosingIterable<T> iterate(Query<T> query, boolean asIs, boolean assertSingle, boolean assertMultiple) {
		try {
			String[] returnAliases = query.getReturnAliases();
			if ((returnAliases == null) || (returnAliases.length == 0)) {
				throw new DomainException("There should be at least 1 projected value in the query");
			}

			if (asIs) {
				if (assertSingle && (returnAliases.length != 1)) {
					throw new DomainException("There should be only 1 projected value in the query when using scalarIterable() or beanIterable()");
				}
				else if (assertMultiple && (returnAliases.length <= 1)) {
					throw new DomainException("There should be more than 1 projected value in the query when using tupleIterable()");
				}
				ScrollableResults results = query.scroll(ScrollMode.FORWARD_ONLY);
				return new HibernateAutoClosingIterable<>(results, false, false);
			}

			// Replace bogus _ property names with the dot
			String[] aliases = returnAliases.clone();
			for (int i = 0, length = aliases.length; i < length; i++) {
				aliases[i] = BindUtil.unsanitiseBinding(aliases[i]);
			}

			ScrollableResults results = query.scroll(ScrollMode.FORWARD_ONLY);
			return new HibernateAutoClosingIterable<>(drivingModuleName, 
														drivingDocumentName, 
														results, 
														aliases,
														false,
														false);
		}
		catch (QueryTimeoutException | org.hibernate.QueryTimeoutException e) {
			throw new TimeoutException(e);
		}
		catch (SkyveException e) {
			throw e;
		}
		catch (Exception e) {
			throw new DomainException(e);
		}
	}
	
	int execute(AbstractQuery query) {
		try {
			@SuppressWarnings("resource")
			Query<?> hibernateQuery = persistence.getSession().createQuery(query.toQueryString());
	
			timeoutQuery(hibernateQuery, query.getTimeoutInSeconds(), persistence.isAsyncThread());
			
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
		catch (QueryTimeoutException | org.hibernate.QueryTimeoutException e) {
			throw new TimeoutException(e);
		}
		catch (SkyveException e) {
			throw e;
		}
		catch (Exception e) {
			throw new DomainException(e);
		}
	}
	
	static void timeoutQuery(Query<?> query, int timeoutInSeconds, boolean asyncThread) {
		// negative timeout values means no timeout
		if (timeoutInSeconds == 0) {
			if (asyncThread) {
				int timeout = UtilImpl.DATA_STORE.getAsyncConnectionTimeoutInSeconds();
				if (timeout > 0) {
					query.setTimeout(timeout);
				}
			}
			else {
				int timeout = UtilImpl.DATA_STORE.getOltpConnectionTimeoutInSeconds();
				if (timeout > 0) {
					query.setTimeout(timeout);
				}
			}
		}
		else if (timeoutInSeconds > 0) {
			query.setTimeout(timeoutInSeconds);
		}
	}
}
