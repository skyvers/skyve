package org.skyve.impl.web.faces.actions;

import java.util.logging.Level;

import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.domain.messages.SecurityException;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.impl.web.faces.beans.FacesView;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

public class SaveAction<T extends Bean> extends FacesAction<Void> {
	private FacesView<T> facesView; 
	private boolean ok;
	public SaveAction(FacesView<T> facesView, boolean ok) {
		this.facesView = facesView;
		this.ok = ok;
	}

	@Override
	@SuppressWarnings("unchecked")
	public Void callback() throws Exception {
		if (UtilImpl.FACES_TRACE) Util.LOGGER.info("SaveAction - ok=" + ok);

		AbstractPersistence persistence = AbstractPersistence.get();
		PersistentBean targetBean = (PersistentBean) ActionUtil.getTargetBeanForView(facesView);
    	User user = persistence.getUser();
    	Customer customer = user.getCustomer();
    	Module targetModule = customer.getModule(targetBean.getBizModule());
		Document targetDocument = targetModule.getDocument(customer, targetBean.getBizDocument());
		View view = targetDocument.getView(facesView.getUxUi().getName(), 
											customer, 
											targetBean.isCreated() ? ViewType.edit.toString() : ViewType.create.toString());
		ImplicitActionName ian = ok ? ImplicitActionName.OK : ImplicitActionName.Save;
		Boolean clientValidation = null;
		Action action = view.getAction(ian.toString());
    	if (action != null) { // could be Defaults action
    		clientValidation = action.getClientValidation();
    	}
		if (UtilImpl.FACES_TRACE) UtilImpl.LOGGER.info("SaveAction - client validation = " + (! Boolean.FALSE.equals(clientValidation)));

		if (Boolean.FALSE.equals(clientValidation) || FacesAction.validateRequiredFields()) {
			// Run the bizlet
			WebContext webContext = facesView.getWebContext();
			CustomerImpl internalCustomer = (CustomerImpl) customer;
			boolean vetoed = internalCustomer.interceptBeforePreExecute(ian, targetBean, null, webContext);
			if (! vetoed) {
				Bizlet<PersistentBean> bizlet = ((DocumentImpl) targetDocument).getBizlet(customer);
				if (bizlet != null) {
					if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "preExecute", "Entering " + bizlet.getClass().getName() + ".preExecute: " + ian + ", " + targetBean + ", null, " + ", " + webContext);
					targetBean = bizlet.preExecute(ian, targetBean, null, webContext);
					if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "preExecute", "Exiting " + bizlet.getClass().getName() + ".preExecute: " + targetBean);
				}
				internalCustomer.interceptAfterPreExecute(ian, targetBean, null, webContext);

				if (targetBean.isNotPersisted() && (! user.canCreateDocument(targetDocument))) {
					throw new SecurityException("create this data", user.getName());
				}
				else if (targetBean.isPersisted() && (! user.canUpdateDocument(targetDocument))) {
					throw new SecurityException("update this data", user.getName());
				}
		
				targetBean = persistence.save(targetDocument, targetBean);
				ActionUtil.setTargetBeanForViewAndCollectionBinding(facesView, null, (T) targetBean);
			}
		}
		
		return null;
	}
}
