package org.skyve.wildcat.web.faces;

import java.security.Principal;
import java.util.Map;

import javax.faces.FacesException;
import javax.faces.component.UIViewRoot;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.faces.event.PhaseEvent;
import javax.faces.event.PhaseId;
import javax.faces.event.PhaseListener;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.skyve.wildcat.persistence.AbstractPersistence;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.web.AbstractWebContext;
import org.skyve.wildcat.web.WebUtil;
import org.skyve.wildcat.web.faces.beans.FacesView;

public class WildcatPhaseListener implements PhaseListener {
	private static final long serialVersionUID = 3757264858610371158L;

	@Override
	public void beforePhase(PhaseEvent event) {
		// nothing to see here
	}

	@Override
	public void afterPhase(PhaseEvent event) {
		PhaseId phaseId = event.getPhaseId();
		if (UtilImpl.FACES_TRACE) UtilImpl.LOGGER.info("WildcatPhaseListener - AFTER " + phaseId + " : responseComplete=" + event.getFacesContext().getResponseComplete());
		try {
			if (PhaseId.RENDER_RESPONSE.equals(phaseId)) {
				afterResponseRendered(event);
			}
			else if (PhaseId.RESTORE_VIEW.equals(phaseId)) {
				afterRestoreView(event);
			}
			// The bean issued a HTTP redirect response
			else if (event.getFacesContext().getResponseComplete()) {
				afterResponseRendered(event);
			}
		}
		catch (Exception e) {
			throw new FacesException(e);
		}
	}

	@Override
	public PhaseId getPhaseId() {
		return PhaseId.ANY_PHASE;
	}
	
	private static void afterRestoreView(PhaseEvent event)
	throws Exception {
		FacesContext fc = event.getFacesContext();
		ExternalContext ec = fc.getExternalContext();
		Map<String, Object> s = ec.getSessionMap();
		UIViewRoot vr = fc.getViewRoot();

		// restore from the session - used when http redirect is used to navigate to a new view
		// restore from the view root - used when within the same view-scoped bean
		if (s.containsKey(FacesUtil.MANAGED_BEAN_NAME_KEY)) {
			if (UtilImpl.FACES_TRACE) UtilImpl.LOGGER.info("WildcatPhaseListener - SET PERSISTENCE FROM SESSION");
			FacesView<?> view = (FacesView<?>) s.get(FacesUtil.MANAGED_BEAN_NAME_KEY);
			restore(view);
		}
		else if (vr != null) {
			String managedBeanName = (String) vr.getAttributes().get(FacesUtil.MANAGED_BEAN_NAME_KEY);
			if (managedBeanName != null) {
				if (UtilImpl.FACES_TRACE) UtilImpl.LOGGER.info("WildcatPhaseListener - SET PERSISTENCE FROM VIEW");
				FacesView<?> view = FacesUtil.getManagedBean(managedBeanName);
				restore(view);
			}
		}

		// initialise the conversation
		AbstractPersistence persistence = AbstractPersistence.get();
		persistence.begin();
		HttpServletRequest request = (HttpServletRequest) ec.getRequest();
    	Principal userPrincipal = request.getUserPrincipal();
    	WebUtil.processUserPrincipalForRequest(request, (userPrincipal == null) ? null : userPrincipal.getName(), true);
	}

	private static void restore(FacesView<?> view)
	throws Exception {
		// restore the context
		ExternalContext ec = FacesContext.getCurrentInstance().getExternalContext();
		AbstractWebContext webContext = WebUtil.getCachedConversation(view.getWebId(),
																	(HttpServletRequest) ec.getRequest(),
																	(HttpServletResponse) ec.getResponse());
		if (webContext != null) { // should always be the case
			view.hydrate(webContext);
	
			// place the conversation into the thread
			AbstractPersistence persistence = webContext.getConversation();
			persistence.setForThread();
		}
	}
	
	private static void afterResponseRendered(PhaseEvent event)
	throws Exception {
		UIViewRoot vr = event.getFacesContext().getViewRoot();
		if (vr != null) {
			String managedBeanName = (String) vr.getAttributes().get(FacesUtil.MANAGED_BEAN_NAME_KEY);
			if (managedBeanName != null) {
				FacesView<?> view = FacesUtil.getManagedBean(managedBeanName);
				AbstractWebContext webContext = view.getWebContext();
				WebUtil.putConversationInCache(webContext);
				view.dehydrate();
			}
		}
	}
}
