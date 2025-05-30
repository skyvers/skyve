package org.skyve.impl.web.service.smartclient;

import java.io.IOException;
import java.io.PrintWriter;
import java.security.Principal;

import org.skyve.EXT;
import org.skyve.content.MimeType;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.MessageException;
import org.skyve.domain.messages.SessionEndedException;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.UserAgent;
import org.skyve.impl.web.WebUtil;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.user.User;
import org.skyve.metadata.user.UserAccess;
import org.skyve.metadata.view.TextOutput.Sanitisation;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.util.OWASP;
import org.skyve.util.Util;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.servlet.ServletConfig;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * Generates views based on bizhub's XML view spec.
 */
public class SmartClientGeneratorServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;

    private static final Logger LOGGER = LoggerFactory.getLogger(SmartClientGeneratorServlet.class);

	private static Class<? extends SmartClientViewRenderer> RENDERER_CLASS = null;
	
	@Override
	@SuppressWarnings("unchecked")
	public void init(ServletConfig config) throws ServletException {
		super.init(config);
		
		String rendererParam = Util.processStringValue(config.getInitParameter("renderer"));
		if (rendererParam != null) {
			try {
				RENDERER_CLASS = (Class<? extends SmartClientViewRenderer>) Thread.currentThread().getContextClassLoader().loadClass(rendererParam);
			}
			catch (Exception e) {
				throw new ServletException("Cannot load SmartClient renderer " + rendererParam, e);
			}
		}
	}
	
	public static SmartClientViewRenderer newRenderer(User user, Module module, Document document, View view, String uxui, boolean noCreateView) {
		if (RENDERER_CLASS == null) {
			return new SmartClientViewRenderer(user, module, document, view, uxui, noCreateView);
		}
		
		try {
			return (SmartClientViewRenderer) RENDERER_CLASS.getDeclaredConstructors()[0].newInstance(user, module, document, view, uxui, Boolean.valueOf(noCreateView));
		}
		catch (Exception e) {
			throw new DomainException("Cannot instantiate SmartClient renderer " + RENDERER_CLASS, e);
		}
	}

	@Override
	protected void doGet(HttpServletRequest request,
							HttpServletResponse response)
	throws ServletException, IOException {
		LOGGER.info("SmartClient Generate - get....");
		processRequest(request, response);
	}

	// NB - Never throw ServletException as this will halt the SmartClient Relogin flow.
	private static void processRequest(HttpServletRequest request,
										HttpServletResponse response)
	throws IOException {
		String moduleName = OWASP.sanitise(Sanitisation.text, Util.processStringValue(request.getParameter(AbstractWebContext.MODULE_NAME)));
		String documentName = OWASP.sanitise(Sanitisation.text, Util.processStringValue(request.getParameter(AbstractWebContext.DOCUMENT_NAME)));

		response.setContentType(MimeType.javascript.toString());
		response.setCharacterEncoding(Util.UTF8);
		response.addHeader("Cache-control", "private,no-cache,no-store"); // never
		response.addDateHeader("Expires", 0); // never
		try (PrintWriter pw = response.getWriter()) {
			AbstractPersistence persistence = AbstractPersistence.get();
			try {
				persistence.begin();
				Principal userPrincipal = request.getUserPrincipal();
				User user = WebUtil.processUserPrincipalForRequest(request, 
																	(userPrincipal == null) ? null : userPrincipal.getName());
				if (user == null) {
					throw new SessionEndedException(request.getLocale());
				}
				Customer customer = user.getCustomer();
	
				if (moduleName == null) {
					throw new ServletException("No module name in the request.");
				}
				if (documentName == null) {
					throw new ServletException("No document name in the request.");
				}

				UxUi uxui = UserAgent.getUxUi(request);
				String uxuiName = uxui.getName();
				LOGGER.info("UX/UI = " + uxuiName);

				EXT.checkAccess(user, UserAccess.singular(moduleName, documentName), uxuiName);

				Module module = customer.getModule(moduleName);
				Document document = module.getDocument(customer, documentName);
				View editView = document.getView(uxuiName, customer, ViewType.edit.toString());
				View createView = document.getView(uxuiName, customer, ViewType.create.toString());
	
				StringBuilder edit = null;
				StringBuilder create = null;
	
				// create and edit view are the same - use edit view
				if (ViewType.edit.toString().equals(createView.getName())) {
					SmartClientViewRenderer renderer = newRenderer(user, module, document, editView, uxuiName, true);
					renderer.visit();
					edit = renderer.getCode();
				}
				else {
					SmartClientViewRenderer renderer = newRenderer(user, module, document, editView, uxuiName, false);
					renderer.visit();
					edit = renderer.getCode();
	
					renderer = newRenderer(user, module, document, createView, uxuiName, false);
					renderer.visit();
					create = renderer.getCode();
				}
	
				pw.append(module.getName()).append('.').append(document.getName()).append(SmartClientWebContext.EDIT_ID_COUNTER).append("=0;");
				pw.append(module.getName()).append('.').append(document.getName()).append(SmartClientWebContext.CREATE_ID_COUNTER).append("=0;");
				pw.append(module.getName()).append(".create").append(document.getName()).append("=function(){");
				pw.append("var view=isc.EditView.create({width:'100%',height:'100%',title:'");
				pw.append("',_mod:'").append(module.getName()).append("',_doc:'").append(document.getName());

				String iconStyleClass = editView.getIconStyleClass();
				if (iconStyleClass == null) {
					iconStyleClass = document.getIconStyleClass();
					if (iconStyleClass != null) {
						pw.append("',_editFontIcon:'").append(OWASP.escapeJsString(iconStyleClass));
					}
					else {
						String icon32 = editView.getIcon32x32RelativeFileName();
						if (icon32 == null) {
							icon32 = document.getIcon32x32RelativeFileName();
							if (icon32 != null) {
								pw.append("',_editIcon:'").append(OWASP.escapeJsString(icon32));
							}
						}
						else { 
							pw.append("',_editIcon:'").append(OWASP.escapeJsString(icon32));
						}
					}
				}
				else {
					pw.append("',_editFontIcon:'").append(OWASP.escapeJsString(iconStyleClass));
				}

				String help = editView.getHelpRelativeFileName();
				if (help != null) {
					pw.append("',_editHelpFile:'").append(OWASP.escapeJsString(help));
				}
				else {
					help = editView.getHelpURL();
					if (help != null) {
						pw.append("',_editHelpURL:'").append(OWASP.escapeJsString(help));
					}
				}

				// create and edit view are not the same - add the create view icons and help stuff
				iconStyleClass = createView.getIconStyleClass();
				if (iconStyleClass == null) {
					iconStyleClass = document.getIconStyleClass();
					if (iconStyleClass != null) {
						pw.append("',_createFontIcon:'").append(OWASP.escapeJsString(iconStyleClass));
					}
					else {
						String icon32 = createView.getIcon32x32RelativeFileName();
						if (icon32 == null) {
							icon32 = document.getIcon32x32RelativeFileName();
							if (icon32 != null) {
								pw.append("',_createIcon:'").append(OWASP.escapeJsString(icon32));
							}
						}
						else { 
							pw.append("',_createIcon:'").append(OWASP.escapeJsString(icon32));
						}
					}
				}
				else {
					pw.append("',_createFontIcon:'").append(OWASP.escapeJsString(iconStyleClass));
				}

				help = createView.getHelpRelativeFileName();
				if (help != null) {
					pw.append("',_createHelpFile:'").append(OWASP.escapeJsString(help));
				}
				else {
					help = createView.getHelpURL();
					if (help != null) {
						pw.append("',_createHelpURL:'").append(OWASP.escapeJsString(help));
					}
				}
				
				pw.append("',_singular:'").append(OWASP.escapeJsString(document.getLocalisedSingularAlias()));
				pw.append("',_ecnt:").append(module.getName()).append('.').append(document.getName()).append("_ecnt");
				pw.append(",_ccnt:").append(module.getName()).append('.').append(document.getName()).append("_ccnt});");

				Util.chunkCharsToWriter(edit, pw);
				if (create != null) {
					Util.chunkCharsToWriter(create, pw);
				}
	
				pw.append("return view;};");
			}
			catch (Throwable t) {
				t.printStackTrace();
				persistence.rollback();
	
				pw.append("isc.warn('");
				if (t instanceof MessageException) {
					SmartClientEditServlet.appendErrorText("Could not generate view.",
															((MessageException) t).getMessages(),
															pw);
					pw.append("');");
				}
				else {
					pw.append("isc.warn('Could not generate views.  Please contact your system administrator.');");
				}
			}
			finally {
				persistence.commit(true);
			}
		}
	}
}