package org.skyve.impl.web.service.rest;

import java.lang.reflect.InvocationTargetException;
import java.security.Principal;
import java.util.List;
import java.util.Set;

import javax.enterprise.context.RequestScoped;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;

import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.SessionEndedException;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.JSONUtil;
import org.skyve.impl.web.WebUtil;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentQuery;
import org.skyve.util.Util;

@Path("/")
@RequestScoped
public class RestService {
	@Context
	private HttpServletRequest request;
	  
	/**
	 * Return a specific item as at a given date
	 * 
	 * @param inDate
	 *            Date to get list for
	 */
	@GET
	@Path("/json/{module}/{document}/{id}")
	@Produces(MediaType.APPLICATION_JSON)
	public Bean retrieveJSON(@PathParam("module") String module, 
									@PathParam("document") String document,
									@PathParam("id") String id) throws Throwable {
		AbstractPersistence persistence = null;
		Bean result = null;
		
		try {
			try {
				persistence = AbstractPersistence.get();
				persistence.evictAllCached();

		        persistence.begin();
		    	Principal userPrincipal = request.getUserPrincipal();
		    	User user = WebUtil.processUserPrincipalForRequest(request, (userPrincipal == null) ? null : userPrincipal.getName(), true);
				if (user == null) {
					throw new SessionEndedException();
				}
		    	persistence.setUser(user);
		    	
		    	result = persistence.retrieve(module, document,  id, false);
		    	Util.populateFully(result);
			}
			catch (InvocationTargetException e) {
				throw e.getTargetException();
			}
		}
		catch (Throwable t) {
	    	t.printStackTrace();
	    	if (persistence != null) {
	    		persistence.rollback();
	    	}
	    	throw t;
		}
	    finally {
	    	if (persistence != null) {
	    		persistence.commit(true);
	    	}
	    }
		
		return result;
	}
	
	
	/**
	 * Return a specific item as at a given date
	 * 
	 * @param inDate
	 *            Date to get list for
	 */
	@GET
	@Path("/xml/{module}/{document}/{id}")
	@Produces(MediaType.APPLICATION_XML)
	public PersistentBean retrieveXML(@PathParam("module") String module, 
										@PathParam("document") String document,
										@PathParam("id") String id) throws Throwable {
		AbstractPersistence persistence = null;
		PersistentBean result = null;
		
		try {
			try {
				persistence = AbstractPersistence.get();
				persistence.evictAllCached();

		        persistence.begin();
		    	Principal userPrincipal = request.getUserPrincipal();
		    	User user = WebUtil.processUserPrincipalForRequest(request, (userPrincipal == null) ? null : userPrincipal.getName(), true);
				if (user == null) {
					throw new SessionEndedException();
				}
		    	persistence.setUser(user);
		    	
		    	result = persistence.retrieve(module, document, id, false);
		    	Util.populateFully(result);
			}
			catch (InvocationTargetException e) {
				throw e.getTargetException();
			}
		}
		catch (Throwable t) {
	    	t.printStackTrace();
	    	if (persistence != null) {
	    		persistence.rollback();
	    	}
	    	throw t;
		}
	    finally {
	    	if (persistence != null) {
	    		persistence.commit(true);
	    	}
	    }
		
		return result;
	}
	
	@GET
	@Path("/json/{module}/{document}")
	@Produces(MediaType.APPLICATION_JSON)
	public String retrieveJSON(@PathParam("module") String module, 
									@PathParam("document") String document,
									@QueryParam("start") int start,
									@QueryParam("end") int end) throws Throwable {
		AbstractPersistence persistence = null;
		String result = null;
		
		try {
			try {
				persistence = AbstractPersistence.get();
				persistence.evictAllCached();

		        persistence.begin();
		    	Principal userPrincipal = request.getUserPrincipal();
		    	User user = WebUtil.processUserPrincipalForRequest(request, (userPrincipal == null) ? null : userPrincipal.getName(), true);
				if (user == null) {
					throw new SessionEndedException();
				}
		    	persistence.setUser(user);
		    	
		    	DocumentQuery q = persistence.newDocumentQuery(module, document);
		    	q.setFirstResult(start);
		    	q.setMaxResults(end - start - 1);
		    	List<Bean> beans = q.projectedResults();
		    	for (Bean bean : beans) {
		    		Util.populateFully(bean);
		    	}
		    	
//		    	Map<String, Object> props = new TreeMap<>();
//		    	props.put(DocumentQuery.THIS_ALIAS, bean);
//		    	bean = new MapBean(bean.getBizModule(), bean.getBizDocument(), props);
		    	Set<String> projection = null; // new TreeSet<>();
//		    	projection.add("dateOfBirth");
//		    	projection.add("dueBack");
		    	result = JSONUtil.marshall(user.getCustomer(), beans, projection);
			}
			catch (InvocationTargetException e) {
				throw e.getTargetException();
			}
		}
		catch (Throwable t) {
	    	t.printStackTrace();
	    	if (persistence != null) {
	    		persistence.rollback();
	    	}
	    	throw t;
		}
	    finally {
	    	if (persistence != null) {
	    		persistence.commit(true);
	    	}
	    }
		
		return result;
	}
}
