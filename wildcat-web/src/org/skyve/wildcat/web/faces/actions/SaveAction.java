package org.skyve.wildcat.web.faces.actions;

import java.util.logging.Level;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.util.Util;
import org.skyve.wildcat.domain.messages.SecurityException;
import org.skyve.wildcat.metadata.model.document.DocumentImpl;
import org.skyve.wildcat.persistence.AbstractPersistence;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.web.faces.FacesAction;
import org.skyve.wildcat.web.faces.beans.FacesView;

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

		if (FacesAction.validateRequiredFields()) {
			AbstractPersistence persistence = AbstractPersistence.get();
			PersistentBean targetBean = (PersistentBean) ActionUtil.getTargetBeanForViewAndCollectionBinding(facesView, null, null);
	
			// Run the bizlet
			User user = CORE.getUser();
			Customer customer = user.getCustomer();
			Module module = customer.getModule(targetBean.getBizModule());
			Document document = module.getDocument(customer, targetBean.getBizDocument());
			Bizlet<PersistentBean> bizlet = ((DocumentImpl) document).getBizlet(customer);
			if (bizlet != null) {
				ImplicitActionName ian = ok ? ImplicitActionName.OK : ImplicitActionName.Save;
				if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "preExecute", "Entering " + bizlet.getClass().getName() + ".preExecute: " + ian + ", " + targetBean + ", null, " + ", " + facesView.getWebContext());
				targetBean = bizlet.preExecute(ian, targetBean, null, facesView.getWebContext());
				if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "preExecute", "Exiting " + bizlet.getClass().getName() + ".preExecute: " + targetBean);
			}
	
			if (targetBean.isNotPersisted() && (! user.canCreateDocument(document))) {
				throw new SecurityException("create this data", user.getName());
			}
			else if (targetBean.isPersisted() && (! user.canUpdateDocument(document))) {
				throw new SecurityException("update this data", user.getName());
			}
	
			targetBean = persistence.save(targetBean);
			ActionUtil.setTargetBeanForViewAndCollectionBinding(facesView, null, (T) targetBean);
		}
		
		return null;
	}
}
