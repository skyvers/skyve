package org.skyve.impl.web.service;

import java.io.IOException;
import java.io.PrintWriter;
import java.lang.reflect.InvocationTargetException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.skyve.CORE;
import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.SessionEndedException;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.WebUtil;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.DocumentQueryDefinition;
import org.skyve.metadata.repository.Repository;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.model.map.DefaultCollectionMapModel;
import org.skyve.metadata.view.model.map.DefaultQueryMapModel;
import org.skyve.metadata.view.model.map.MapModel;
import org.skyve.util.JSON;
import org.skyve.util.Util;

import com.vividsolutions.jts.geom.Envelope;

/**
 * Map Servlet - supplies map data to google map widget.
 * 
 * there are 3 usage modes:-
 * 
 * 1) This mode executes the query and then gets each geometry object using qeometryBinding.
 * 		parameters
 * 			query
 * 			geometryBinding
 * 2) This mode reaches into the current bean's collection via collectionBinding 
 * 		and gets each geometry object using collectionGeometryBinding
 * 		parameters
 * 			webContext
 * 			collectionBinding
 * 			geometryBinding
 * 3) This mode uses the given model to generate its own geometry list
 * 		parameters
 * 			webContext
 * 			modelName
 */
public class MapServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;
	
	private static final String GEOMETRY_BINDING_NAME = "_geo";
	
	@Override
	protected void doGet(HttpServletRequest request, HttpServletResponse response)
	throws ServletException, IOException {
		response.setContentType(MimeType.json.toString());
		response.setCharacterEncoding(Util.UTF8);
		response.addHeader("Cache-control", "private,no-cache,no-store"); // never
		response.addDateHeader("Expires", 0); // never

		try (PrintWriter pw = response.getWriter()) {
			AbstractPersistence persistence = AbstractPersistence.get();
			try {
				try {
					persistence.begin();
					User user = WebUtil.processUserPrincipalForRequest(request, request.getUserPrincipal().getName(), true);
					if (user == null) {
						throw new SessionEndedException();
					}
					persistence.setUser(user);

					String result = null;
					String queryName = request.getParameter(AbstractWebContext.QUERY_NAME);
					if (queryName != null) {
						result = processQuery(request);
					}
					else {
						String modelName = request.getParameter(AbstractWebContext.MODEL_NAME);
						if (modelName != null) {
							result = processModel(request, response);
						}
						else {
							result = processCollection(request, response);
						}
					}

					if (result != null) {
						pw.print(result);
					}
					else {
						pw.print("{}");
					}
				}
				catch (InvocationTargetException e) {
					throw e.getTargetException();
				}
			}
			catch (Throwable t) {
				t.printStackTrace();
				persistence.rollback();
				pw.print("{}");
			}
			finally {
				if (persistence != null) {
					persistence.commit(true);
				}
			}
		}
	}
	
	private static String processQuery(HttpServletRequest request)
	throws Exception {
		String moduleName = request.getParameter(AbstractWebContext.MODULE_NAME);
		String queryName = request.getParameter(AbstractWebContext.QUERY_NAME);
		String geometryBinding = request.getParameter(GEOMETRY_BINDING_NAME);
		
		AbstractPersistence persistence = AbstractPersistence.get();
		User user = persistence.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(moduleName);
		DocumentQueryDefinition query = module.getDocumentQuery(queryName);
		if (query == null) {
			query = module.getDocumentDefaultQuery(customer, queryName);
		}
		DefaultQueryMapModel<Bean> model = new DefaultQueryMapModel<>(query);
		model.setGeometryBinding(geometryBinding);
		// TODO get the envelope from the map
		return JSON.marshall(customer, model.getResult(new Envelope(-180, 180, -90, 90)), null);
	}
	
	private static String processCollection(HttpServletRequest request, HttpServletResponse response)
	throws Exception {
		Customer customer = CORE.getUser().getCustomer();
		Bean bean = WebUtil.getConversationBeanFromRequest(request, response);
		
		String collectionBinding = request.getParameter(AbstractWebContext.GRID_BINDING_NAME);
		String geometryBinding = request.getParameter(GEOMETRY_BINDING_NAME);
		DefaultCollectionMapModel<Bean> model = new DefaultCollectionMapModel<>(collectionBinding);
		model.setGeometryBinding(geometryBinding);
		model.setBean(bean);
		// TODO get the envelope from the map
		return JSON.marshall(customer, model.getResult(new Envelope(-180, 180, -90, 90)), null);
	}

	private static String processModel(HttpServletRequest request, HttpServletResponse response)
	throws Exception {
		Customer customer = CORE.getUser().getCustomer();
		Bean bean = WebUtil.getConversationBeanFromRequest(request, response);
		Module module = customer.getModule(bean.getBizModule());
		Document document = module.getDocument(customer, bean.getBizDocument());

		Repository repository = CORE.getRepository();
		MapModel<Bean> model = repository.getMapModel(customer, document, request.getParameter(AbstractWebContext.MODEL_NAME));
		model.setBean(bean);
		// TODO get the envelope from the map
		return JSON.marshall(customer, model.getResult(new Envelope(-180, 180, -90, 90)), null);
	}
}
