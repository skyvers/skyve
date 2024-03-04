package org.skyve.impl.web.service.smartclient;

import java.io.IOException;
import java.io.PrintWriter;
import java.lang.reflect.InvocationTargetException;
import java.security.Principal;
import java.util.List;
import java.util.Map;

import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.MessageException;
import org.skyve.domain.messages.SessionEndedException;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.UserAgent;
import org.skyve.impl.web.WebUtil;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.user.User;
import org.skyve.metadata.user.UserAccess;
import org.skyve.metadata.view.TextOutput.Sanitisation;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.tag.TagManager;
import org.skyve.util.JSON;
import org.skyve.util.OWASP;
import org.skyve.util.Util;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;

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

    	// Send CSRF Token as a response header (must be done before getting the writer)
		String currentCsrfTokenString = UtilImpl.processStringValue(request.getParameter(AbstractWebContext.CSRF_TOKEN_NAME));
		Integer currentCsrfToken = (currentCsrfTokenString == null) ? null : Integer.valueOf(currentCsrfTokenString);
		Integer newCsrfToken = currentCsrfToken;
		String action = OWASP.sanitise(Sanitisation.text, Util.processStringValue(request.getParameter("a")));
		// If this is a mutating request, we'll definitely need a new CSRF Token
		if ("L".equals(action)) {
			if (newCsrfToken == null) {
				newCsrfToken = StateUtil.createToken();
			}
		}
		else {
			newCsrfToken = StateUtil.createToken();
		}
    	response.setIntHeader("X-CSRF-TOKEN", newCsrfToken.intValue());

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
						throw new SessionEndedException(request.getLocale());
					}
					Customer customer = user.getCustomer();
	
					String menuButtonId = OWASP.sanitise(Sanitisation.text, Util.processStringValue(request.getParameter("ID")));
					String tagId = OWASP.sanitise(Sanitisation.text, Util.processStringValue(request.getParameter("t")));
					String tagName = OWASP.sanitise(Sanitisation.text, Util.processStringValue(request.getParameter("n")));
					// Dont sanitise this one as it is JSON - TODO should use a JSON sanitiser on it.
					String criteria = Util.processStringValue(request.getParameter("c"));
					String dataSourceName = OWASP.sanitise(Sanitisation.text, Util.processStringValue(request.getParameter("d")));
		
					HttpSession session = request.getSession();
					TagManager tm = EXT.getTagManager();

					if ("L".equals(action)) {
						list(tagId, menuButtonId, sb);
					}
					else if ("T".equals(action)) {
						SmartClientListServlet.checkCsrfToken(session, request, response, currentCsrfToken);
						
						// Note - if there is no form in the view then there is no web context
						String contextKey = OWASP.sanitise(Sanitisation.text, Util.processStringValue(request.getParameter(AbstractWebContext.CONTEXT_NAME)));
			        	AbstractWebContext webContext = StateUtil.getCachedConversation(contextKey, request, response);
						Bean bean = WebUtil.getConversationBeanFromRequest(webContext, request);

						UxUi uxui = UserAgent.getUxUi(request);
						try (AutoClosingIterable<Bean> iterable = iterate(tagId, 
																			dataSourceName,
																			criteria,
																			bean,
																			user,
																			customer,
																			uxui)) {
							tm.tag(tagId, iterable);
						}
					}
					else if ("U".equals(action)) {
						SmartClientListServlet.checkCsrfToken(session, request, response, currentCsrfToken);
						
						// Note - if there is no form in the view then there is no web context
						String contextKey = OWASP.sanitise(Sanitisation.text, Util.processStringValue(request.getParameter(AbstractWebContext.CONTEXT_NAME)));
			        	AbstractWebContext webContext = StateUtil.getCachedConversation(contextKey, request, response);
						Bean bean = WebUtil.getConversationBeanFromRequest(webContext, request);

						UxUi uxui = UserAgent.getUxUi(request);
						try (AutoClosingIterable<Bean> iterable = iterate(tagId, 
																			dataSourceName,
																			criteria,
																			bean,
																			user,
																			customer,
																			uxui)) {
							tm.untag(tagId, iterable);
						}
					}
					else if ("C".equals(action)) {
						SmartClientListServlet.checkCsrfToken(session, request, response, currentCsrfToken);
						
						tm.clear(tagId);
					}
					else if ("N".equals(action)) {
						SmartClientListServlet.checkCsrfToken(session, request, response, currentCsrfToken);
						
						tagId = tm.create(tagName, true);
						sb.append("{bizId:'");
						sb.append(tagId);
						sb.append("'}");
					}
					else if ("D".equals(action)) {
						SmartClientListServlet.checkCsrfToken(session, request, response, currentCsrfToken);
						
						tm.delete(tagId);
					}

					pw.append(sb);
					pw.flush();
					
					// Replace CSRF token
					StateUtil.replaceToken(session, currentCsrfToken, newCsrfToken);
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
			    	pw.append(OWASP.escapeJsString(t.getMessage()));
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

        for (DomainValue value : EXT.getTagManager().getTags()) {
        	String escapedCode = OWASP.escapeJsString(value.getCode());
        	String escapedDescription = OWASP.escapeJsString(value.getLocalisedDescription());
        	
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
														Customer customer,
														UxUi uxui)
	throws Exception {
		// Determine
		int _Index = dataSourceName.indexOf('_');
		String moduleName = dataSourceName.substring(0, _Index);
		Module module = customer.getModule(moduleName);
		String documentOrQueryOrModelName = dataSourceName.substring(_Index + 1);
		Document drivingDocument = null;
		ListModel<Bean> model = null;
		int __Index = documentOrQueryOrModelName.indexOf("__");
		// model type of request
		if (__Index >= 0) {
			String documentName = documentOrQueryOrModelName.substring(0, __Index);
			Document document = module.getDocument(customer, documentName);
			String modelName = documentOrQueryOrModelName.substring(__Index + 2);
			user.checkAccess(UserAccess.modelAggregate(moduleName, documentName, modelName), uxui.getName());

			model = document.getListModel(customer, modelName, true);
			model.setBean(bean);
			drivingDocument = model.getDrivingDocument();
		}
		// query type of request
		else {
			MetaDataQueryDefinition query = module.getMetaDataQuery(documentOrQueryOrModelName);
			// not a query, must be a document
			if (query == null) {
				user.checkAccess(UserAccess.documentAggregate(moduleName, documentOrQueryOrModelName), uxui.getName());
				query = module.getDocumentDefaultQuery(customer, documentOrQueryOrModelName);
			}
			else {
				user.checkAccess(UserAccess.queryAggregate(moduleName, documentOrQueryOrModelName), uxui.getName());
			}
			if (query == null) {
				throw new ServletException("DataSource does not reference a valid query " + documentOrQueryOrModelName);
			}
			drivingDocument = module.getDocument(customer, query.getDocumentName());
			model = EXT.newListModel(query);
		}

		// Note:- No need to check read on the driving document here as we are tagging, not reading the target document
		
		// add filter criteria
		@SuppressWarnings("unchecked")
		Map<String, Object> criteria = (Map<String, Object>) JSON.unmarshall(user, criteriaJSON);
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
				SmartClientListServlet.addSimpleFilterCriteriaToQuery(module,
																		drivingDocument,
																		customer,
																		SmartClientFilterOperator.substring,
																		criteria,
																		tagId,
																		model);
			}
		}

		return model.iterate();
	}
}
