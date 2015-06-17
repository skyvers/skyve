package org.skyve.wildcat.persistence.hibernate;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.Map.Entry;

import org.hibernate.Query;
import org.hibernate.ScrollMode;
import org.hibernate.ScrollableResults;
import org.hibernate.Session;
import org.hibernatespatial.GeometryUserType;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.wildcat.domain.MapBean;
import org.skyve.wildcat.persistence.AbstractQuery;
import org.skyve.wildcat.util.UtilImpl;

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
		
		String queryString = query.toQueryString();
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(queryString + " executed on thread " + Thread.currentThread());

		Query result = session.createQuery(queryString);
		if (firstResult >= 0) {
			result.setFirstResult(firstResult);
		}
		if (maxResults > 0) {
			result.setMaxResults(maxResults);
		}

		Map<String, Object> parameters = query.getParameters();
		if (parameters != null) {
			for (Entry<String, Object> entry : parameters.entrySet()) {
				Object value = entry.getValue();
				if (value instanceof Geometry) {
					result.setParameter(entry.getKey(), value, GeometryUserType.TYPE);
				}
				else {
					result.setParameter(entry.getKey(), value);
				}
			}
		}

		return result;
	}
	
	<T> List<T> list(Query query, boolean asIs)
	throws DomainException {
		return list(null, null, query, asIs);
	}
	
	@SuppressWarnings("unchecked")
	<T> List<T> list(String drivingModuleName, String drivingDocumentName, Query query, boolean asIs)
	throws DomainException {
		try {
			if (asIs) {
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
				if (result instanceof Bean) {
					beans.add((T) result);
				}
				else {
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
			}

			return beans;
		}
		catch (Exception e) {
			throw new DomainException(e);
		}
	}

	public <T> AutoClosingIterable<T> iterate(AbstractQuery query, boolean asIs)
	throws DomainException {
		try {
			Query hibernateQuery = createHibernateQuery(query);

			ScrollableResults results = hibernateQuery.scroll(ScrollMode.FORWARD_ONLY);
			if (asIs) {
				return new HibernateAutoClosingIterable<>(null, 
															null, 
															results, 
															null);
			}

			// Replace bogus _ property names with the dot
			String[] aliases = hibernateQuery.getReturnAliases().clone();
			for (int i = 0, length = aliases.length; i < length; i++) {
				aliases[i] = aliases[i].replace('_', '.');
			}

			return new HibernateAutoClosingIterable<>(query.getDrivingModuleName(), 
															query.getDrivingDocumentName(), 
															results, 
															aliases);
		}
		catch (Exception e) {
			throw new DomainException(e);
		}
	}
}
