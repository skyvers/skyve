package org.skyve.impl.web.service.smartclient;

import java.io.IOException;
import java.io.PrintWriter;
import java.lang.reflect.InvocationTargetException;
import java.security.Principal;
import java.security.SecureRandom;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.MessageException;
import org.skyve.domain.messages.SessionEndedException;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.WebUtil;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.TextOutput.Sanitisation;
import org.skyve.persistence.DocumentFilter;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.OWASP;
import org.skyve.util.Util;

public class SmartClientSnapServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;

	private static final String SNAPSHOT_MODULE_NAME = "admin";
	private static final String SNAPSHOT_DOCUMENT_NAME = "Snapshot";
	private static final String SNAPSHOT_MODULE_NAME_PROPERTY_NAME = "moduleName";
	private static final String SNAPSHOT_QUERY_NAME_PROPERTY_NAME = "queryName";
	private static final String SNAPSHOT_NAME_PROPERTY_NAME = "name";
	private static final String SNAPSHOT_SNAPSHOT_PROPERTY_NAME = "snapshot";
    
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
				newCsrfToken = Integer.valueOf(new SecureRandom().nextInt());
			}
		}
		else {
			newCsrfToken = Integer.valueOf(new SecureRandom().nextInt());
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
	
					String menuButtonId = OWASP.sanitise(Sanitisation.text, Util.processStringValue(request.getParameter("ID")));
					String snapId = OWASP.sanitise(Sanitisation.text, Util.processStringValue(request.getParameter("i")));
					String snapName = OWASP.sanitise(Sanitisation.text, Util.processStringValue(request.getParameter("n")));
					// Dont sanitise this one as it is JSON - TODO should use a JSON sanitiser on it.
					String snapshot = Util.processStringValue(request.getParameter("s"));
					String dataSource = OWASP.sanitise(Sanitisation.text, Util.processStringValue(request.getParameter("d")));

					String moduleName = null;
					String queryName = null;
					if (dataSource != null) {
			        	// use first 2 tokens of '_' split - could be a pick list which means extra '_' in it
			        	String[] tokens = dataSource.split("_");
						moduleName = tokens[0];
						queryName = tokens[1];
					}

					HttpSession session = request.getSession();

					if ("L".equals(action)) {
						list(snapId, menuButtonId, moduleName, queryName, sb);
					}
					else if ("U".equals(action)) {
						SmartClientListServlet.checkCsrfToken(session, request, response, currentCsrfToken);
						
						update(snapId, snapshot);
					}
					else if ("N".equals(action)) {
						SmartClientListServlet.checkCsrfToken(session, request, response, currentCsrfToken);

						snapId = create(moduleName, queryName, snapName, snapshot);
						sb.append("{bizId:'");
						sb.append(snapId);
						sb.append("'}");
					}
					else if ("D".equals(action)) {
						SmartClientListServlet.checkCsrfToken(session, request, response, currentCsrfToken);

						delete(snapId);
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
		    		SmartClientEditServlet.appendErrorText("The snapshot operation was unsuccessful", 
		    												((MessageException) t).getMessages(),
		    												pw);
		    	}
		    	else {
			    	pw.append("The snapshot operation was unsuccessful: ");
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

	private static void list(String snapId, 
								String menuButtonId,
								String moduleName,
								String queryName,
								StringBuilder sb)
	throws Exception {
	    sb.append("[{title:'New Snapshot',icon:'icons/snap_add.png',click:'");
	    sb.append(menuButtonId).append(".newSnap()'},");
	    sb.append("{isSeparator:true},");
	    sb.append("{title:'No Snapshot',click:\"").append(menuButtonId).append(".setSnap(null,'No Snapshot',null)\"},");
	    sb.append("{isSeparator:true}");

	    Persistence p = CORE.getPersistence();
	    DocumentQuery q = p.newDocumentQuery(SNAPSHOT_MODULE_NAME, SNAPSHOT_DOCUMENT_NAME);
	    q.addBoundProjection(Bean.DOCUMENT_ID);
	    q.addBoundProjection(SNAPSHOT_NAME_PROPERTY_NAME);
	    q.addBoundProjection(SNAPSHOT_SNAPSHOT_PROPERTY_NAME);
	    DocumentFilter f = q.getFilter();
	    f.addEquals(SNAPSHOT_MODULE_NAME_PROPERTY_NAME, moduleName);
	    f.addEquals(SNAPSHOT_QUERY_NAME_PROPERTY_NAME, queryName);
	    q.addBoundOrdering(SNAPSHOT_NAME_PROPERTY_NAME);

	    for (Bean bean : q.projectedResults()) {
        	String escapedCode = OWASP.escapeJsString((String) BindUtil.get(bean, Bean.DOCUMENT_ID));
        	String escapedDescription = OWASP.escapeJsString((String) BindUtil.get(bean, SNAPSHOT_NAME_PROPERTY_NAME));
        	String snapshot = (String) BindUtil.get(bean, SNAPSHOT_SNAPSHOT_PROPERTY_NAME);

        	// snap select menu
            sb.append(",{title:'").append(escapedDescription).append("',icon:'icons/snap.png',click:function(){");
            sb.append(menuButtonId).append(".setSnap('").append(escapedCode).append("','");
            sb.append(escapedDescription).append("',");
            sb.append(snapshot).append(")},submenu:[");

            boolean disabled = (snapId == null) || (! snapId.equals(escapedCode));

            // Note - javascript escapes (like \') don't work in string methods
            // update snap
            sb.append("{title:'Update Snapshot");
            if (disabled) {
            	sb.append(" (Select snapshot first)',enabled:false,");
            } else {
            	sb.append("',");
            }
            sb.append("icon:'icons/snap_edit.png',click:function(){").append(menuButtonId).append(".updateSnap('");
            sb.append(escapedCode).append("')}},");

            // delete snap
            sb.append("{title:'Delete SnapShot',icon:'icons/snap_delete.png',click:function(){").append(menuButtonId).append(".deleteSnap('");
            sb.append(escapedCode).append("')}}]}");
	    }
        sb.append("]");
	}

	private static String create(String snapModuleName,
									String snapQueryName,
									String snapName,
									String snapshot)
	throws Exception {
	    Persistence p = CORE.getPersistence();
	    User user = p.getUser();
	    Customer customer = user.getCustomer();
	    Module module = customer.getModule(SNAPSHOT_MODULE_NAME);
	    Document document = module.getDocument(customer, SNAPSHOT_DOCUMENT_NAME);
	    
	    PersistentBean snap = document.newInstance(user);
	    BindUtil.set(snap, SNAPSHOT_MODULE_NAME_PROPERTY_NAME, snapModuleName);
	    BindUtil.set(snap, SNAPSHOT_QUERY_NAME_PROPERTY_NAME, snapQueryName);
	    BindUtil.set(snap, SNAPSHOT_NAME_PROPERTY_NAME, snapName);
	    BindUtil.set(snap, SNAPSHOT_SNAPSHOT_PROPERTY_NAME, snapshot);
	    
	    snap = p.save(document, snap);
	    
	    return snap.getBizId();
	}

	private static void update(String snapId, String snapshot) 
	throws Exception {
		Persistence p = CORE.getPersistence();
		PersistentBean snap = p.retrieveAndLock(SNAPSHOT_MODULE_NAME, SNAPSHOT_DOCUMENT_NAME, snapId);
		BindUtil.set(snap, SNAPSHOT_SNAPSHOT_PROPERTY_NAME, snapshot);
		p.save(snap);
	}

	private static void delete(String snapId) 
	throws Exception {
		Persistence p = CORE.getPersistence();
		PersistentBean snap = p.retrieveAndLock(SNAPSHOT_MODULE_NAME, SNAPSHOT_DOCUMENT_NAME, snapId);
		p.delete(snap);
	}
}
