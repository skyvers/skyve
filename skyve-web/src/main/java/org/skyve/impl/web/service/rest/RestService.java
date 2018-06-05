package org.skyve.impl.web.service.rest;

import java.util.List;

import javax.enterprise.context.RequestScoped;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.content.AttachmentContent;
import org.skyve.content.ContentManager;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.NoResultsException;
import org.skyve.impl.domain.messages.SecurityException;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.filter.rest.AbstractRestFilter;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.DocumentQueryDefinition;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.model.list.DocumentQueryListModel;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;
import org.skyve.util.JSON;
import org.skyve.util.Util;

@Path("/")
@RequestScoped
public class RestService {
	@Context
	private HttpServletRequest request;
	@Context
	private HttpServletResponse response;
	
	@GET
	@Path("/json/{module}/{document}/{id}")
	@Produces(MediaType.APPLICATION_JSON)
	public String retrieveJSON(@PathParam("module") String module, 
								@PathParam("document") String document,
								@PathParam("id") String id) {
		String result = null;
		Bean bean = null;
		
		Persistence p = null;
		try {
			response.setContentType(MediaType.APPLICATION_JSON);
			p = CORE.getPersistence();
			User u = p.getUser();
			Customer c = u.getCustomer();
			Module m = c.getModule(module);
			Document d = m.getDocument(c, document);
			
			if (! u.canReadDocument(d)) {
				throw new SecurityException("read this data", u.getName());
			}
	
	    	bean = p.retrieve(d, id, false);
	    	if (bean == null) {
	    		throw new NoResultsException();
	    	}
	    	Util.populateFully(bean);
	    	result = JSON.marshall(CORE.getUser().getCustomer(), bean, null);
		}
		catch (Throwable t) {
			t.printStackTrace();
			AbstractRestFilter.error(p, response, t.getLocalizedMessage());
		}
		
		return result;
	}

	@GET
	@Path("/xml/{module}/{document}/{id}")
	@Produces(MediaType.APPLICATION_XML)
	public Bean retrieveXML(@PathParam("module") String module, 
										@PathParam("document") String document,
										@PathParam("id") String id) {
		Bean result = null;
		
		Persistence p = null;
		try {
			response.setContentType(MediaType.APPLICATION_XML);
			p = CORE.getPersistence();
			User u = p.getUser();
			Customer c = u.getCustomer();
			Module m = c.getModule(module);
			Document d = m.getDocument(c, document);
			
			if (! u.canReadDocument(d)) {
				throw new SecurityException("read this data", u.getName());
			}
	
	    	result = p.retrieve(d, id, false);
	    	if (result == null) {
	    		throw new NoResultsException();
	    	}
	    	Util.populateFully(result);
		}
		catch (Throwable t) {
			t.printStackTrace();
			AbstractRestFilter.error(p, response, t.getLocalizedMessage());
		}
		
		return result;
	}
	
	@GET
	@Path("/json/{module}/{document}")
	@Produces(MediaType.APPLICATION_JSON)
	public String retrieveJSON(@PathParam("module") String module, 
								@PathParam("document") String document,
								@QueryParam("start") int start,
								@QueryParam("end") int end) {
		String result = null;
		
		Persistence p = null;
		try {
			response.setContentType(MediaType.APPLICATION_JSON);

			p = CORE.getPersistence();
			User u = p.getUser();
			Customer c = u.getCustomer();
			Module m = c.getModule(module);
			Document d = m.getDocument(c, document);
			
			if (! u.canReadDocument(d)) {
				throw new SecurityException("read this data", u.getName());
			}
			
	    	DocumentQuery q = p.newDocumentQuery(d);
	    	q.setFirstResult(start);
	    	q.setMaxResults(end - start - 1);
	    	List<Bean> beans = q.projectedResults();
	    	for (Bean bean : beans) {
	    		Util.populateFully(bean);
	    	}
			result = JSON.marshall(CORE.getUser().getCustomer(), beans, null);
		}
		catch (Throwable t) {
			t.printStackTrace();
			AbstractRestFilter.error(p, response, t.getLocalizedMessage());
		}
		
		return result;
	}

	@GET
	@Path("/json/insert/{bean}")
	@Produces(MediaType.APPLICATION_JSON)
	public String insertJSONGet(@PathParam("bean") String json) {
		return insertJSON(json);
	}

	@PUT
	@Path("/json/insert")
	@Produces(MediaType.APPLICATION_JSON)
	@Consumes(MediaType.APPLICATION_JSON)
	public String insertJSONPost(String json) {
		return insertJSON(json);
	}
	
	private String insertJSON(String json) {
		String result = null;
		
		Persistence p = null;
		try {
			response.setContentType(MediaType.APPLICATION_JSON);
			p = CORE.getPersistence();
			User u = p.getUser();
			
			PersistentBean bean = (PersistentBean) JSON.unmarshall(u, json);
			bean = p.save(bean);
			result = JSON.marshall(u.getCustomer(), bean, null);
		}
		catch (Throwable t) {
			t.printStackTrace();
			AbstractRestFilter.error(p, response, t.getLocalizedMessage());
		}
		
		return result;
	}

	@GET
	@Path("/json/update/{bean}")
	@Produces(MediaType.APPLICATION_JSON)
	public String updateJSONGet(@PathParam("bean") String json) {
		return updateJSON(json);
	}

	@POST
	@Path("/json/update")
	@Produces(MediaType.APPLICATION_JSON)
	@Consumes(MediaType.APPLICATION_JSON)
	public String updateJSONPost(String json) {
		return updateJSON(json);
	}
	
	private String updateJSON(String json) {
		String result = null;
		
		Persistence p = null;
		try {
			response.setContentType(MediaType.APPLICATION_JSON);
			p = CORE.getPersistence();
			User u = p.getUser();
			
			PersistentBean bean = (PersistentBean) JSON.unmarshall(u, json);
			PersistentBean beanToUpdate = p.retrieve(bean.getBizModule(), bean.getBizDocument(), bean.getBizId(), true);
			Binder.copy(bean, beanToUpdate);
			beanToUpdate = p.save(beanToUpdate);
			result = JSON.marshall(u.getCustomer(), beanToUpdate, null);
		}
		catch (Throwable t) {
			t.printStackTrace();
			AbstractRestFilter.error(p, response, t.getLocalizedMessage());
		}
		
		return result;
	}

	@GET
	@Path("/json/delete/{bean}")
	@Produces(MediaType.APPLICATION_JSON)
	public String deleteJSONGet(@PathParam("bean") String json) {
		return deleteJSON(json);
	}
	
	@DELETE
	@Path("/json/delete")
	@Produces(MediaType.APPLICATION_JSON)
	@Consumes(MediaType.APPLICATION_JSON)
	public String deleteJSONDelete(String json) {
		return deleteJSON(json);
	}

	private String deleteJSON(String json) {
		String result = null;
		
		Persistence p = null;
		try {
			response.setContentType(MediaType.APPLICATION_JSON);
			p = CORE.getPersistence();
			User u = p.getUser();
			
			PersistentBean bean = (PersistentBean) JSON.unmarshall(u, json);
			PersistentBean beanToDelete = p.retrieve(bean.getBizModule(), bean.getBizDocument(), bean.getBizId(), true);
			p.delete(beanToDelete);
			result = "{}";
		}
		catch (Throwable t) {
			t.printStackTrace();
			AbstractRestFilter.error(p, response, t.getLocalizedMessage());
		}
		
		return result;
	}
	
/* Doesn't work Failed executing GET /xml/admin/Contact: org.jboss.resteasy.core.NoMessageBodyWriterFoundFailure: Could not find MessageBodyWriter for response object of type: java.util.ArrayList of media type: application/xml
	@GET
	@Path("/xml/{module}/{document}")
	@Produces(MediaType.APPLICATION_XML)
	public List<Bean> retrieveXML(@PathParam("module") String module, 
								@PathParam("document") String document,
								@QueryParam("start") int start,
								@QueryParam("end") int end) throws Throwable {
		response.setContentType(MediaType.APPLICATION_XML);
		return retrieve(module, document, start, end);
	}
*/

	@GET
	@Path("/json/query/{module}/{documentOrQuery}")
	@Produces(MediaType.APPLICATION_JSON)
	public String query(@PathParam("module") String module,
							@PathParam("documentOrQuery") String documentOrQuery,
							@QueryParam("start") int start,
							@QueryParam("end") int end) {
		String result = null;
		
		Persistence p = null;
		try {
			response.setContentType(MediaType.APPLICATION_JSON);
			p = CORE.getPersistence();
			User u = p.getUser();
			Customer c = u.getCustomer();
			Module m = c.getModule(module);
	
			DocumentQueryDefinition q = null;
			q = m.getDocumentQuery(documentOrQuery);
			// not a query, could be a document
			if (q == null) {
				q = m.getDocumentDefaultQuery(c, documentOrQuery);
			}
			if (q == null) {
				throw new IllegalArgumentException(documentOrQuery + " is not a valid query or document.");
			}
	 
			DocumentQueryListModel<Bean> qm = new DocumentQueryListModel<>();
	        qm.setQuery(q);
	        qm.setStartRow(start);
	        qm.setEndRow(end);
	
	        Document d = qm.getDrivingDocument();
			if (! u.canReadDocument(d)) {
				throw new SecurityException("read this data", u.getName());
			}
	        
	        List<Bean> beans = qm.fetch().getRows();
	        result = JSON.marshall(c, beans, qm.getProjections());
		}
		catch (Throwable t) {
			t.printStackTrace();
			AbstractRestFilter.error(p, response, t.getLocalizedMessage());
		}
		
		return result;
	}
	
	@GET
	@Path("/content/{contentId}")
	@Produces(MediaType.APPLICATION_OCTET_STREAM)
	public byte[] queryContent(@PathParam("contentId") String contentId) {
		byte[] result = null;
		
		try {
			try (ContentManager cm = EXT.newContentManager()) {
				AttachmentContent content = cm.get(contentId);
				
				if (content == null) {
					UtilImpl.LOGGER.info(request.getRequestURI() + " not found");
					response.setStatus(HttpServletResponse.SC_NOT_FOUND);
					return result;
				}
				
				User u = CORE.getUser();
				if (! u.canAccessContent(content.getBizId(),
											content.getBizModule(),
											content.getBizDocument(),
											content.getBizCustomer(),
											content.getBizDataGroupId(),
											content.getBizUserId(),
											content.getAttributeName())) {
					throw new SecurityException(content.getBizModule() + '.' + content.getBizDocument() + '.' + content.getAttributeName(), u.getName());
				}

				result = content.getContentBytes();
				
				// Set headers
				response.setContentType(content.getMimeType().toString());
				response.setCharacterEncoding(Util.UTF8);
				response.setHeader("Content-Disposition", 
									String.format("attachment; filename=\"%s\"", content.getFileName()));
				UtilImpl.LOGGER.info(request.getRequestURI() + " served as binary");
			}				
		}
		catch (Throwable t) {
			t.printStackTrace();
			AbstractRestFilter.error(null, response, t.getLocalizedMessage());
		}
			
		return result;
	}
}
