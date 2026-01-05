package org.skyve.impl.web.service.smartclient;

import java.io.IOException;
import java.io.PrintWriter;
import java.lang.reflect.InvocationTargetException;
import java.security.Principal;
import java.util.List;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.app.AppConstants;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.MessageException;
import org.skyve.domain.messages.SecurityException;
import org.skyve.domain.messages.SessionEndedException;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.snapshot.SnapshotAdapter;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.UserAgent;
import org.skyve.impl.web.WebUtil;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.user.User;
import org.skyve.metadata.user.UserAccess;
import org.skyve.metadata.view.TextOutput.Sanitisation;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.OWASP;
import org.skyve.util.Util;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;

public class SmartClientSnapServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;
	
	@Override
	@SuppressWarnings("java:S1989") // there exists JavaEE error pages
	protected void doGet(HttpServletRequest request, HttpServletResponse response)
	throws ServletException, IOException {
		processRequest(request, response);
	}

	@Override
	@SuppressWarnings("java:S1989") // there exists JavaEE error pages
	protected void doPost(HttpServletRequest request, HttpServletResponse response)
	throws ServletException, IOException {
		processRequest(request, response);
	}

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
																		(userPrincipal == null) ? null : userPrincipal.getName());
					if (user == null) {
						throw new SessionEndedException(request.getLocale());
					}

					String snapId = OWASP.sanitise(Sanitisation.text, Util.processStringValue(request.getParameter("i")));
					String snapName = OWASP.sanitise(Sanitisation.text, Util.processStringValue(request.getParameter("n")));
					// Don't sanitise this one as it is JSON - TODO should use a JSON sanitiser on it.
					String snapshot = Util.processStringValue(request.getParameter("s"));
					String dataSource = OWASP.sanitise(Sanitisation.text, Util.processStringValue(request.getParameter("d")));
					boolean smartClientRequest = "sc".equals(Util.processStringValue(request.getParameter("t")));

					String moduleName = null;
					String documentOrQueryOrModelName = null;
					if (dataSource != null) {
						int _Index = dataSource.indexOf('_');
						moduleName = dataSource.substring(0, _Index);
						Customer customer = user.getCustomer();
						Module module = customer.getModule(moduleName);
						documentOrQueryOrModelName = dataSource.substring(_Index + 1);

						UxUi uxui = UserAgent.getUxUi(request);

						int __Index = documentOrQueryOrModelName.indexOf("__");
						// model type of request
						if (__Index >= 0) {
							String documentName = documentOrQueryOrModelName.substring(0, __Index);
							String modelName = documentOrQueryOrModelName.substring(__Index + 2);

							EXT.checkAccess(user, UserAccess.modelAggregate(moduleName, documentName, modelName), uxui.getName());
						}
						// query type of request
						else {
							MetaDataQueryDefinition query = module.getMetaDataQuery(documentOrQueryOrModelName);
							// not a query, must be a document
							if (query == null) {
								EXT.checkAccess(user, UserAccess.documentAggregate(moduleName, documentOrQueryOrModelName), uxui.getName());
								query = module.getDocumentDefaultQuery(customer, documentOrQueryOrModelName);
							}
							else {
								EXT.checkAccess(user, UserAccess.queryAggregate(moduleName, documentOrQueryOrModelName), uxui.getName());
							}
						}
					}

					HttpSession session = request.getSession();

					if ("L".equals(action)) {
						StringBuilder result = list(moduleName, documentOrQueryOrModelName, smartClientRequest);
						sb.append(result);
					}
					else if ("U".equals(action)) {
						SmartClientListServlet.checkCsrfToken(session, request, response, currentCsrfToken);
						update(snapId, snapshot, smartClientRequest);
					}
					else if ("N".equals(action)) {
						SmartClientListServlet.checkCsrfToken(session, request, response, currentCsrfToken);
						snapId = create(moduleName, documentOrQueryOrModelName, snapName, snapshot, smartClientRequest);
						sb.append("{\"bizId\":\"");
						sb.append(snapId);
						sb.append("\"}");
					}
					else if ("D".equals(action)) {
						SmartClientListServlet.checkCsrfToken(session, request, response, currentCsrfToken);
						delete(snapId);
					}

					Util.chunkCharsToWriter(sb, pw);
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
				persistence.rollback();

				pw.append("isc.warn('");
				if (t instanceof MessageException me) {
					SmartClientEditServlet.appendErrorText("The Snapshot operation was unsuccessful",
															me.getMessages(),
															pw);
				}
				else {
					pw.append("The Snapshot operation was unsuccessful: ");
					pw.append(OWASP.escapeJsString(t.getMessage()));
				}
				pw.append("');");
				pw.flush();
			}
			finally {
				persistence.commit(true);
			}
		}
	}

	private static StringBuilder list(String moduleName,
										String queryName,
										boolean smartClientRequest)
	throws Exception {
		Persistence p = CORE.getPersistence();
		DocumentQuery q = p.newDocumentQuery(AppConstants.ADMIN_MODULE_NAME, AppConstants.SNAPSHOT_DOCUMENT_NAME)
							.addBoundProjection(Bean.DOCUMENT_ID)
							.addBoundProjection(AppConstants.NAME_ATTRIBUTE_NAME)
							.addBoundProjection(AppConstants.SNAPSHOT_ATTRIBUTE_NAME)
							.addBoundOrdering(AppConstants.ORDINAL_ATTRIBUTE_NAME)
							.addBoundOrdering(AppConstants.NAME_ATTRIBUTE_NAME);
		q.getFilter()
			.addEquals(AppConstants.MODULE_NAME_ATTRIBUTE_NAME, moduleName)
			.addEquals(AppConstants.QUERY_NAME_ATTRIBUTE_NAME, queryName);
		
		StringBuilder sb = new StringBuilder();

		// Snapshots array
		sb.append('[');
		List<Bean> results = q.projectedResults();
		for (Bean bean : results) {
			String escapedCode = OWASP.escapeJsString((String) BindUtil.get(bean, Bean.DOCUMENT_ID));
			String escapedDescription = OWASP.escapeJsString((String) BindUtil.get(bean, AppConstants.NAME_ATTRIBUTE_NAME));
			String snapshot = (String) BindUtil.get(bean, AppConstants.SNAPSHOT_ATTRIBUTE_NAME);

			snapshot = smartClientRequest ? SnapshotAdapter.toSmartClient(snapshot) : SnapshotAdapter.toVue(snapshot);
			if (snapshot == null) { // not a valid snapshot
				continue;
			}
			
			sb.append("{\"")
					.append(Bean.DOCUMENT_ID)
					.append("\":\"")
					.append(escapedCode);
			sb.append("\",\"")
					.append(AppConstants.NAME_ATTRIBUTE_NAME)
					.append("\":\"")
					.append(escapedDescription);
			sb.append("\",\"")
					.append(AppConstants.SNAPSHOT_ATTRIBUTE_NAME)
					.append("\":")
					.append(snapshot)
					.append("},");
		}
		if (! results.isEmpty()) {
			sb.setLength(sb.length() - 1); // remove last comma
		}
		sb.append(']');

		return sb;
	}

	private static String create(String snapModuleName,
									String snapQueryName,
									String snapName,
									String snapshot,
									boolean smartClientRequest)
	throws Exception {
		// Validate snapshot definition
		SnapshotAdapter snapshotAdapater = smartClientRequest ? SnapshotAdapter.SMART_CLIENT : SnapshotAdapter.VUE;
		if (snapshotAdapater.fromClientPayload(snapshot) == null) { // not valid
			throw new DomainException("Snapshot definition is not valid");
		}

		Persistence p = CORE.getPersistence();
		User user = p.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(AppConstants.ADMIN_MODULE_NAME);
		Document document = module.getDocument(customer, AppConstants.SNAPSHOT_DOCUMENT_NAME);

		PersistentBean snap = document.newInstance(user);
		BindUtil.set(snap, AppConstants.MODULE_NAME_ATTRIBUTE_NAME, snapModuleName);
		BindUtil.set(snap, AppConstants.QUERY_NAME_ATTRIBUTE_NAME, snapQueryName);
		BindUtil.set(snap, AppConstants.NAME_ATTRIBUTE_NAME, snapName);
		BindUtil.set(snap, AppConstants.SNAPSHOT_ATTRIBUTE_NAME, snapshot);
		// NB SnapshotBizlet puts the new snapshot at the bottom of the list with max(ordinal) + 1
		snap = p.save(document, snap);

		return snap.getBizId();
	}

	private static void update(String snapId, String snapshot, boolean smartClientRequest)
	throws Exception {
		// Validate snapshot definition
		SnapshotAdapter snapshotAdapater = smartClientRequest ? SnapshotAdapter.SMART_CLIENT : SnapshotAdapter.VUE;
		if (snapshotAdapater.fromClientPayload(snapshot) == null) { // not valid
			throw new DomainException("Snapshot definition is not valid");
		}

		Persistence p = CORE.getPersistence();
		PersistentBean snap = p.retrieveAndLock(AppConstants.ADMIN_MODULE_NAME,
													AppConstants.SNAPSHOT_DOCUMENT_NAME,
													snapId);
		// Check the snapshot is for the current user
		User u = p.getUser();
		if (! u.getId().equals(snap.getBizUserId())) {
			throw new SecurityException("update this Snapshot", u.getName());
		}
		BindUtil.set(snap, AppConstants.SNAPSHOT_ATTRIBUTE_NAME, snapshot);
		p.save(snap);
	}

	private static void delete(String snapId)
	throws Exception {
		Persistence p = CORE.getPersistence();
		PersistentBean snap = p.retrieveAndLock(AppConstants.ADMIN_MODULE_NAME,
													AppConstants.SNAPSHOT_DOCUMENT_NAME,
													snapId);
		// Check the snapshot is for the current user
		User u = p.getUser();
		if (! u.getId().equals(snap.getBizUserId())) {
			throw new SecurityException("delete this Snapshot", u.getName());
		}
		p.delete(snap);
	}
}
