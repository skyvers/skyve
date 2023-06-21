package org.skyve.impl.web.service.smartclient;

import java.io.IOException;
import java.io.PrintWriter;
import java.lang.reflect.InvocationTargetException;
import java.security.Principal;
import java.util.Collections;
import java.util.List;
import java.util.logging.Level;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.ConversationEndedException;
import org.skyve.domain.messages.SessionEndedException;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.domain.messages.AccessException;
import org.skyve.impl.domain.messages.SecurityException;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.view.widget.bound.input.CompleteType;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.UserAgent;
import org.skyve.impl.web.WebUtil;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.user.User;
import org.skyve.metadata.user.UserAccess;
import org.skyve.persistence.DocumentQuery;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.util.Util;

/**
 * Service for complete mechanism.
 */
public class SmartClientCompleteServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;

	@Override
	protected void doGet(HttpServletRequest request, HttpServletResponse response)
	throws ServletException, IOException {
		UtilImpl.LOGGER.info("SmartClientComplete - get....");
		processRequest(request, response);
	}

	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response)
	throws ServletException, IOException {
		UtilImpl.LOGGER.info("SmartClientComplete - post....");
		processRequest(request, response);
	}

	// NB - Never throw ServletException as this will halt the SmartClient Relogin flow.
	private static void processRequest(HttpServletRequest request, HttpServletResponse response)
	throws IOException {
		response.setContentType(MimeType.json.toString());
		response.setCharacterEncoding(Util.UTF8);
		response.addHeader("Cache-control", "private,no-cache,no-store"); // never
		response.addDateHeader("Expires", 0); // never
		
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
					persistence.setUser(user);

					CompleteType complete = null;
					Customer customer = user.getCustomer();
					Document document = null;
					Attribute attribute = null;
					Bean bean = null;
					
					// NB This can be a compound binding but will be reduced to the last component attribute name below
					String attributeName = UtilImpl.processStringValue(request.getParameter("_attr"));
					if (attributeName == null) {
						throw new ServletException("Mal-formed URL");
					}
					attributeName = BindUtil.unsanitiseBinding(attributeName);
					final String binding = attributeName;
					
					String webId = UtilImpl.processStringValue(request.getParameter(AbstractWebContext.CONTEXT_NAME));
			        if (webId == null) {
			        	throw new ConversationEndedException(request.getLocale());
			        }

			        String formModuleName = null;
			        String formDocumentName = null;
			        try {
						complete = CompleteType.valueOf(request.getParameter(AbstractWebContext.ACTION_NAME));

			        	AbstractWebContext webContext = StateUtil.getCachedConversation(webId, request, response);
			        	UtilImpl.LOGGER.info("USE OLD CONVERSATION!!!!");
			        	bean = webContext.getCurrentBean();

				    	String formBinding = request.getParameter(AbstractWebContext.BINDING_NAME);
				    	if (formBinding != null) {
				    		formBinding = BindUtil.unsanitiseBinding(formBinding);
				    		bean = (Bean) BindUtil.get(bean, formBinding);
				    	}
				    	formModuleName = bean.getBizModule();
				    	formDocumentName = bean.getBizDocument();
				    	
				    	// if attributeName is compound, get the parent bean and adjust the attributeName
				    	// prefer the module and document name determination polymorphically from the bean, otherwise use the metadata
				    	int lastDotIndex = attributeName.lastIndexOf('.');
						if (lastDotIndex >= 0) {
							Bean formBean = bean;
							bean = (Bean) BindUtil.get(bean, attributeName.substring(0, lastDotIndex));
							if (bean == null) { // no bean so use metadata
								Module module = customer.getModule(formBean.getBizModule());
								document = module.getDocument(customer, formBean.getBizDocument());
								TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, document, attributeName);
								document = target.getDocument();
								attribute = target.getAttribute(); // could be null for an implicit attribute
								attributeName = attributeName.substring(lastDotIndex + 1);
							}
							else {
								attributeName = attributeName.substring(lastDotIndex + 1);
								Module module = customer.getModule(bean.getBizModule());
								document = module.getDocument(customer, bean.getBizDocument());
								TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, document, attributeName);
								document = target.getDocument();
								attribute = target.getAttribute(); // could be null for an implicit attribute
							}
						}
						else {
							Module module = customer.getModule(bean.getBizModule());
							document = module.getDocument(customer, bean.getBizDocument());
							TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, document, attributeName);
							document = target.getDocument();
							attribute = target.getAttribute(); // could be null for an implicit attribute
						}
					}
			        catch (ConversationEndedException | SessionEndedException e) {
			        	throw e;
			        }
					catch (Exception e) {
						throw new ServletException("Mal-formed URL", e);
					}
					if ((attribute == null) && (! BindUtil.isImplicit(attributeName))) {
						throw new ServletException("Mal-formed URL");
					}

					String value = UtilImpl.processStringValue(request.getParameter("value"));

					String _startRow = request.getParameter("_startRow");
					int startRow = (_startRow == null) ? 0 : Integer.parseInt(_startRow);
					String _endRow = request.getParameter("_endRow");
					int endRow = (_endRow == null) ? Integer.MAX_VALUE : Integer.parseInt(_endRow);

					if (complete == CompleteType.previous) {
						final String userName = user.getName();
						final UxUi uxui = UserAgent.getUxUi(request);
						if (! user.canAccess(UserAccess.previousComplete(formModuleName, formDocumentName, binding), uxui.getName())) {
							UtilImpl.LOGGER.warning("User " + userName + " cannot access document view previous complete " + formModuleName + '.' + formDocumentName + " for " + binding);
							UtilImpl.LOGGER.info("If this user already has a document privilege, check if they were navigated to this page/resource programatically or by means other than the menu or views and need to be granted access via an <accesses> stanza in the module or view XML.");
							throw new AccessException("the previous data", userName);
						}

						if (! user.canReadDocument(document)) {
							throw new SecurityException("read this data", userName);
						}
						
						long totalRows = 0L;
						List<Object> values = Collections.emptyList();

						if (document.isPersistable()) { // persistent document
							if ((attribute == null) || // implicit attribute or
									attribute.isPersistent()) { // explicit and persistent attribute
								final String moduleName = document.getOwningModuleName();
								final String documentName = document.getName();
								DocumentQuery q = persistence.newDocumentQuery(moduleName, documentName);
								StringBuilder sb = new StringBuilder(128);
								q.addExpressionProjection(sb.append("count(distinct bean.").append(attributeName).append(')').toString(), "totalRows");
								if (value != null) {
									sb.setLength(0);
									q.getFilter().addLike(attributeName, sb.append('%').append(value).append('%').toString());
								}
								Number count = q.scalarResult(Number.class);
								if (count != null) {
									totalRows = count.longValue();
								}
								
								if (totalRows > 0) {
									q = persistence.newDocumentQuery(moduleName, documentName);
									q.addBoundProjection(attributeName, "value");
									q.setDistinct(true);
									if (value != null) {
										sb.setLength(0);
										q.getFilter().addLike(attributeName, sb.append('%').append(value).append('%').toString());
									}
									// NB return Object as the type could be anything
									values = q.setFirstResult(startRow)
												.setMaxResults(endRow - startRow)
												.scalarResults(Object.class);
								}
							}
						}
						
						StringBuilder message = new StringBuilder(1024);
						message.append("{\"response\":{");
						message.append("\"status\":0,");
						message.append("\"startRow\":").append(startRow);
						message.append(",\"endRow\":");
						message.append(Math.min(totalRows, endRow));
						message.append(",\"totalRows\":");
						message.append(totalRows);
						message.append(",\"data\":[");
						if (! values.isEmpty()) {
							for (Object result : values) {
								message.append("{value:\"").append(result).append("\"},");
							}
							message.setLength(message.length() - 1); // remove last comma
						}
						message.append("]}}");
						pw.append(message);
					}
					else {
			        	List<String> result = null;
			        	CustomerImpl internalCustomer = (CustomerImpl) customer;
						boolean vetoed = internalCustomer.interceptBeforeComplete(attributeName, value, bean);
						if (! vetoed) {
							Bizlet<Bean> bizlet = ((DocumentImpl) document).getBizlet(customer);
							if (bizlet != null) {
								if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "complete", "Entering " + bizlet.getClass().getName() + ".complete: " + attributeName + ", " + value + ", " + bean);
								result = bizlet.complete(attributeName, value, bean);
								if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "complete", "Exiting " + bizlet.getClass().getName() + ".complete: " + attributeName + ", " + value + ", " + bean);
							}
							internalCustomer.interceptAfterComplete(attributeName, value, bean, result);
						}
			        	
						if (result == null) {
			        		result = Collections.emptyList();
			        	}

			        	int finish = Math.min(result.size(), endRow);
						StringBuilder message = new StringBuilder(1024);
						message.append("{\"response\":{");
						message.append("\"status\":0,");
						message.append("\"startRow\":").append(startRow);
						message.append(",\"endRow\":").append(finish);
						message.append(",\"totalRows\":").append(result.size());
						message.append(",\"data\":[");
						if (finish > 0) {
							for (int i = startRow; i < finish; i++) {
								message.append("{\"value\":\"").append(result.get(i)).append("\"},");
							}
							message.setLength(message.length() - 1); // remove last comma
						}
						message.append("]}}");
						pw.append(message);
					}
				}
				catch (InvocationTargetException e) {
					throw e.getTargetException();
				}
			}
			catch (Throwable t) {
				t.printStackTrace();
				persistence.rollback();

				SmartClientEditServlet.produceErrorResponse(t, Operation.fetch, false, pw);
			}
			finally {
				if (persistence != null) {
					persistence.commit(true);
				}
			}
		}
	}
}
