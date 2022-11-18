package org.skyve.impl.web.faces.actions;

import java.util.logging.Level;

import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.OptimisticLockException;
import org.skyve.domain.messages.OptimisticLockException.OperationType;
import org.skyve.domain.messages.ValidationException;
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
import org.skyve.util.Util;
import org.skyve.web.WebContext;

public class DeleteAction extends FacesAction<Void> {
	private FacesView<? extends Bean> facesView;
	public DeleteAction(FacesView<? extends Bean> facesView) {
		this.facesView = facesView;
	}

	@Override
	public Void callback() throws Exception {
		if (UtilImpl.FACES_TRACE) Util.LOGGER.info("DeleteAction");

		AbstractPersistence persistence = AbstractPersistence.get();
		PersistentBean beanToDelete = (PersistentBean) ActionUtil.getTargetBeanForView(facesView);
		User user = persistence.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(beanToDelete.getBizModule());
		Document document = module.getDocument(customer, beanToDelete.getBizDocument());
		
		if (! user.canDeleteDocument(document)) {
			throw new SecurityException("delete this data", user.getName());
		}
		
		// Ensure that we are working on the latest of everything and no related entities are pointing to old data
		persistence.evictAllCached();
		PersistentBean persistentBeanToDelete = persistence.retrieve(document, 
																		beanToDelete.getBizId());

		if (persistentBeanToDelete == null) { // deleted by another user
			throw new ValidationException(new Message("Failed to delete this information as it was already deleted by someone else after you looked at it."));
		}

		if (! user.canReadBean(persistentBeanToDelete.getBizId(), 
								persistentBeanToDelete.getBizModule(), 
								persistentBeanToDelete.getBizDocument(), 
								persistentBeanToDelete.getBizCustomer(), 
								persistentBeanToDelete.getBizDataGroupId(), 
								persistentBeanToDelete.getBizUserId())) {
			throw new SecurityException("read this data", user.getName());
		}
		
		if (! persistentBeanToDelete.getBizLock().equals(beanToDelete.getBizLock())) {
			throw new OptimisticLockException(user, 
												OperationType.delete, 
												persistentBeanToDelete.getBizLock());
		}

		// Run preExecute after the copy is taken, in case we rollback
		WebContext webContext = facesView.getWebContext();
		CustomerImpl internalCustomer = (CustomerImpl) customer;
		boolean vetoed = internalCustomer.interceptBeforePreExecute(ImplicitActionName.Delete, persistentBeanToDelete, null, webContext);
		if (! vetoed) {
			Bizlet<PersistentBean> bizlet = ((DocumentImpl) document).getBizlet(customer);
			if (bizlet != null) {
				if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "preExecute", "Entering " + bizlet.getClass().getName() + ".preExecute: " + ImplicitActionName.Delete + ", " + persistentBeanToDelete + ", null, " + ", " + webContext);
				persistentBeanToDelete = bizlet.preExecute(ImplicitActionName.Delete, persistentBeanToDelete, null, webContext);
				if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "preExecute", "Exiting " + bizlet.getClass().getName() + ".preExecute: " + persistentBeanToDelete);
			}
			internalCustomer.interceptAfterPreExecute(ImplicitActionName.Delete, persistentBeanToDelete, null, webContext);

			persistence.delete(document, persistentBeanToDelete);
			
			// We want to call post render
			facesView.setPostRender(bizlet, persistentBeanToDelete);
		}
		
		return null;
	}
}
