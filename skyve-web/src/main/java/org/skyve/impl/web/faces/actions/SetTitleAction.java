package org.skyve.impl.web.faces.actions;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.UserAgent;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.util.Binder;
import org.skyve.util.logging.Category;
import org.slf4j.Logger;
import org.skyve.util.logging.SkyveLoggerFactory;

import jakarta.faces.context.FacesContext;
import jakarta.servlet.http.HttpServletRequest;

/**
 * Executes a Faces callback action within the current Skyve web context.
 */
public class SetTitleAction extends FacesAction<Void> {

    private static final Logger LOGGER = SkyveLoggerFactory.getLogger(SetTitleAction.class);
    private static final Logger FACES_LOGGER = Category.FACES.logger();

	private FacesView facesView;

	/**
	 * Creates an action that updates the supplied view model's title.
	 *
	 * @param facesView Faces view receiving the raw resolved title; must not be {@code null}
	 */
	public SetTitleAction(FacesView facesView) {
		this.facesView = facesView;
	}

	/**
	 * Resolves the active view title and stores the raw text on the current
	 * {@link FacesView}.
	 *
	 * <p>Side effects: reads the current Skyve user/repository metadata and mutates
	 * {@code facesView}. HTML escaping is deliberately left to the JSF/PrimeFaces
	 * page-title output boundary.
	 */
	@Override
	public Void callback() throws Exception {
		if (UtilImpl.FACES_TRACE) FACES_LOGGER.info("SetTitleAction");

		if (facesView.getBean() != null) {
			User user = CORE.getUser();
			Customer customer = user.getCustomer();
			Bean targetBean = ActionUtil.getTargetBeanForView(facesView);
	    	Module targetModule = customer.getModule(targetBean.getBizModule());
			Document targetDocument = targetModule.getDocument(customer, targetBean.getBizDocument());
			HttpServletRequest request = (HttpServletRequest) FacesContext.getCurrentInstance().getExternalContext().getRequest();
			View view = targetDocument.getView(UserAgent.getSelection(request).getUxUi().getName(),
												customer, 
												targetBean.isCreated() ? 
													ViewType.edit.toString() : 
													ViewType.create.toString());
			facesView.setTitle(Binder.formatMessage(view.getLocalisedTitle(), targetBean));
	    }
		else {
        	LOGGER.warn("SetTitleAction: FacesView.getBean() yields null");
		}
		
		return null;
	}
}
