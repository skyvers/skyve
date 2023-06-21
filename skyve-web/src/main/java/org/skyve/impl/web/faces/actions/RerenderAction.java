package org.skyve.impl.web.faces.actions;

import java.util.logging.Level;

import org.skyve.domain.Bean;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.impl.web.faces.beans.FacesView;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.web.WebContext;

public class RerenderAction<T extends Bean> extends FacesAction<Void> {
	private FacesView<T> facesView;
	private String source;
	private boolean validate;
	
	public RerenderAction(FacesView<T> facesView, String source, boolean validate) {
		this.facesView = facesView;
		this.source = source;
		this.validate = validate;
	}

	@Override
	public Void callback() throws Exception {
		if (UtilImpl.FACES_TRACE) UtilImpl.LOGGER.info("RerenderAction - EXECUTE RERENDER with source " + source + (validate ? " with" : " without") + " validation ");

		AbstractPersistence persistence = AbstractPersistence.get();
		Bean targetBean = ActionUtil.getTargetBeanForView(facesView);
		// rerender can be called asynchronously with push notifications 
		// and maybe the user is not on the same view, maybe not even the same type
		// Maybe they were on an edit, now on a list view.
		// So target bean can be null
		// If this is the case, bug out...
		if (targetBean != null) {
			User user = persistence.getUser();
	    	CustomerImpl customer = (CustomerImpl) user.getCustomer();
	    	Module targetModule = customer.getModule(targetBean.getBizModule());
			Document targetDocument = targetModule.getDocument(customer, targetBean.getBizDocument());
			Bizlet<Bean> targetBizlet = ((DocumentImpl) targetDocument).getBizlet(customer);
			WebContext webContext = facesView.getWebContext();
			
		    if ((! validate) || FacesAction.validateRequiredFields()) {
				boolean vetoed = customer.interceptBeforePreRerender(source, targetBean, webContext);
				if (! vetoed) {
					if (targetBizlet != null) {
						if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, targetBizlet.getClass().getName(), "preRerender", "Entering " + targetBizlet.getClass().getName() + ".preRerender: " + source + ", " + targetBean + ", " + webContext);
		    			targetBizlet.preRerender(source, targetBean, webContext);
		    			if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, targetBizlet.getClass().getName(), "preRerender", "Exiting " + targetBizlet.getClass().getName() + ".preRerender: " + targetBean);
					}
					customer.interceptAfterPreRerender(source, targetBean, webContext);
					
					// We want to call post render
					facesView.setPostRender(targetBizlet, targetBean);
				}
		    }
		}
		else {
        	UtilImpl.LOGGER.warning("RerenderAction: Target Bean is null");
		}
		
	    return null;
	}
}
