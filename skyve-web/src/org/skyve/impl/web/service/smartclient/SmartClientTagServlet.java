package org.skyve.impl.web.service.smartclient;

import java.io.IOException;
import java.io.PrintWriter;
import java.lang.reflect.InvocationTargetException;
import java.security.Principal;
import java.util.List;
import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.MessageException;
import org.skyve.domain.messages.SessionEndedException;
import org.skyve.impl.generate.SmartClientGenerateUtils;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.JSONUtil;
import org.skyve.impl.util.TagUtil;
import org.skyve.impl.web.WebUtil;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.DocumentQueryDefinition;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.model.list.DocumentQueryListModel;
import org.skyve.metadata.view.model.list.Filter;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.persistence.AutoClosingIterable;

public class SmartClientTagServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;

	@Override
	protected void doGet(HttpServletRequest request, HttpServletResponse response)
	throws ServletException, IOException {
		processRequest(request, response);
	}

	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response)
	throws ServletException, IOException {
		processRequest(request, response);
	}

	// NB - Never throw ServletException as this will halt the SmartClient Relogin flow.
	private static void processRequest(HttpServletRequest request, 
										HttpServletResponse response)
	throws IOException {
		StringBuilder sb = new StringBuilder(256);

		try (PrintWriter pw = response.getWriter()) {
			AbstractPersistence persistence = AbstractPersistence.get();
			try {
				try {
					persistence.begin();
					Principal userPrincipal = request.getUserPrincipal();
					User user = WebUtil.processUserPrincipalForRequest(request, 
																		(userPrincipal == null) ? null : userPrincipal.getName(),
																		true);
					if (user == null) {
						throw new SessionEndedException();
					}
					Customer customer = user.getCustomer();
	
					String menuButtonId = request.getParameter("ID");
					String action = request.getParameter("a");
					String tagId = request.getParameter("t");
					String tagName = request.getParameter("n");
					String criteria = request.getParameter("c");
					String dataSourceName = request.getParameter("d");
		
					if ("L".equals(action)) {
						list(tagId, menuButtonId, sb);
					}
					else if ("T".equals(action)) {
						// Note - if there is no form in the view then there is no web context
						Bean bean = WebUtil.getConversationBeanFromRequest(request, response);

						try (AutoClosingIterable<Bean> iterable = iterate(tagId, 
																			dataSourceName,
																			criteria,
																			bean,
																			user,
																			customer)) {
							TagUtil.tag(tagId, iterable);
						}
					}
					else if ("U".equals(action)) {
						// Note - if there is no form in the view then there is no web context
						Bean bean = WebUtil.getConversationBeanFromRequest(request, response);

						try (AutoClosingIterable<Bean> iterable = iterate(tagId, 
																			dataSourceName,
																			criteria,
																			bean,
																			user,
																			customer)) {
							TagUtil.untag(tagId, iterable);
						}
					}
					else if ("C".equals(action)) {
						TagUtil.clear(tagId);
					}
					else if ("N".equals(action)) {
						tagId = TagUtil.create(tagName, true);
						sb.append("{bizId:'");
						sb.append(tagId);
						sb.append("'}");
					}
					else if ("D".equals(action)) {
						TagUtil.delete(tagId);
					}

					pw.append(sb);
					pw.flush();
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
	
		    	pw.append("isc.warn('");
		    	if (t instanceof MessageException) {
		    		SmartClientEditServlet.appendErrorText("The tag operation was unsuccessful",
		    												((MessageException) t).getMessages(),
		    												pw);
		    	}
		    	else {
			    	pw.append("The tag operation was unsuccessful: ");
			    	pw.append(SmartClientGenerateUtils.processString(t.getMessage()));
		    	}
		    	pw.append("');");
		    	pw.flush();
			}
			finally {
				if (persistence != null) {
					persistence.commit(true);
				}
			}
		}
	}

	private static void list(String tagId, String menuButtonId, StringBuilder sb)
	throws Exception {
	    sb.append("[{title:'New Tag',icon:'icons/tag_add.png',click:'");
	    sb.append(menuButtonId).append(".newTag()'},");
	    sb.append("{isSeparator:true},");
	    sb.append("{title:'No Tag',click:\"").append(menuButtonId).append(".setTag(null,'No Tag')\"},");
	    sb.append("{isSeparator:true}");

        for (DomainValue value : TagUtil.getTags()) {
        	String escapedCode = SmartClientGenerateUtils.processString(value.getCode());
        	String escapedDescription = SmartClientGenerateUtils.processString(value.getDescription());
        	
        	// tag select menu
            sb.append(",{title:'").append(escapedDescription).append("',icon:'icons/tag.png',click:function(){");
            sb.append(menuButtonId).append(".setTag('").append(escapedCode).append("','");
            sb.append(escapedDescription).append("')},submenu:[");

            boolean disabled = (tagId == null) || (! tagId.equals(escapedCode));

            // Note - javascript escapes (like \') don't work in string methods
            // tag all menu
            sb.append("{title:'Tag all in list");
            if (disabled) {
            	sb.append(" (Select tag first)',enabled:false,");
            } else {
            	sb.append("',");
            }
            sb.append("icon:'icons/tag_all.png',click:function(){").append(menuButtonId).append(".tagOp('");
            sb.append(escapedCode).append("','T')}},");
	            
            // untag all menu
            sb.append("{title:'Untag all in list");
            if (disabled) {
            	sb.append(" (Select tag first)',enabled:false,");
            } else {
            	sb.append("',");
            }
            sb.append("icon:'icons/tag_none.png',click:function(){").append(menuButtonId).append(".tagOp('");
            sb.append(escapedCode).append("','U')}},");
            
            // clear all tagged menu
            sb.append("{title:'Clear all tagged',icon:'icons/tag_clear.png',click:function(){").append(menuButtonId).append(".tagOp('");
            sb.append(escapedCode).append("','C')}},");

            // delete tag menu
            sb.append("{title:'Delete Tag',icon:'icons/tag_delete.png',click:function(){").append(menuButtonId).append(".tagOp('");
            sb.append(escapedCode).append("','D')}}]}");
        }
        sb.append("]");
	}

	private static AutoClosingIterable<Bean> iterate(String tagId,
														String dataSourceName,
														String criteriaJSON,
														Bean bean,
														User user,
														Customer customer)
	throws Exception {
		// Determine
		int _Index = dataSourceName.indexOf('_');
		Module module = customer.getModule(dataSourceName.substring(0, _Index));
		String documentOrQueryOrModelName = dataSourceName.substring(_Index + 1);
		Document drivingDocument = null;
		ListModel<Bean> model = null;
		int __Index = documentOrQueryOrModelName.indexOf("__");
		if (__Index >= 0) {
			String documentName = documentOrQueryOrModelName.substring(0, __Index);
			Document document = module.getDocument(customer, documentName);
			String modelName = documentOrQueryOrModelName.substring(__Index + 2);
			model = CORE.getRepository().getListModel(customer, document, modelName);
			model.setBean(bean);
			drivingDocument = model.getDrivingDocument();
		}
		else {
			DocumentQueryDefinition query = module.getDocumentQuery(documentOrQueryOrModelName);
			if (query == null) {
				query = module.getDocumentDefaultQuery(customer, documentOrQueryOrModelName);
			}
			if (query == null) {
				throw new ServletException("DataSource does not reference a valid query " + documentOrQueryOrModelName);
			}
			drivingDocument = module.getDocument(customer, query.getDocumentName());
			DocumentQueryListModel queryModel = new DocumentQueryListModel();
			queryModel.setQuery(query);
			model = queryModel;
		}
		
		// add filter criteria
		@SuppressWarnings("unchecked")
		Map<String, Object> criteria = (Map<String, Object>) JSONUtil.unmarshall(user, criteriaJSON);
		if (criteria != null) {
			String operator = (String) criteria.get("operator");
			if (operator != null) { // advanced criteria
				@SuppressWarnings("unchecked")
				List<Map<String, Object>> advancedCriteria = (List<Map<String, Object>>) criteria.get("criteria");
				SmartClientListServlet.addAdvancedFilterCriteriaToQuery(module,
																			drivingDocument,
																			user,
																			CompoundFilterOperator.valueOf(operator),
																			advancedCriteria,
																			tagId,
																			model);
			}
			else { // simple criteria
				Filter filter = model.getFilter();
				SmartClientListServlet.addSimpleFilterCriteriaToQuery(module,
																		drivingDocument,
																		customer,
																		SmartClientFilterOperator.substring,
																		criteria,
																		tagId,
																		filter);
			}
		}

		return model.iterate();
	}
}
