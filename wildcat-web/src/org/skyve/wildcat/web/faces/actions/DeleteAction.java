package org.skyve.wildcat.web.faces.actions;

import java.util.logging.Level;

import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.OptimisticLockException;
import org.skyve.domain.messages.OptimisticLockException.OperationType;
import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.messages.Message;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.wildcat.domain.messages.SecurityException;
import org.skyve.wildcat.metadata.model.document.DocumentImpl;
import org.skyve.wildcat.persistence.AbstractPersistence;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.web.faces.FacesAction;
import org.skyve.wildcat.web.faces.beans.FacesView;

public class DeleteAction extends FacesAction<Void> {
	private FacesView<? extends Bean> facesView;
	public DeleteAction(FacesView<? extends Bean> facesView) {
		this.facesView = facesView;
	}

	@Override
	public Void callback() throws Exception {
    	AbstractPersistence persistence = AbstractPersistence.get();
		PersistentBean beanToDelete = (PersistentBean) ActionUtil.getTargetBeanForViewAndCollectionBinding(facesView, null, null);
		User user = persistence.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(beanToDelete.getBizModule());
		Document document = module.getDocument(customer, beanToDelete.getBizDocument());
		
		if (! user.canDeleteDocument(document)) {
			throw new SecurityException("delete this data", user.getName());
		}
		
		persistence.evictCached(beanToDelete);
		PersistentBean persistentBeanToDelete = persistence.retrieve(document, 
																		beanToDelete.getBizId(), 
																		false);

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
			throw new OptimisticLockException(customer, 
												OperationType.delete, 
												persistentBeanToDelete.getBizLock());
		}

		// Run preExecute after the copy is taken, in case we rollback
		Bizlet<PersistentBean> bizlet = ((DocumentImpl) document).getBizlet(customer);
		if (bizlet != null) {
			UtilImpl.LOGGER.info("PRE-EXECUTE on " + ImplicitActionName.Delete);
			if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "preExecute", "Entering " + bizlet.getClass().getName() + ".preExecute: " + ImplicitActionName.Delete + ", " + persistentBeanToDelete + ", null, " + ", " + facesView.getWebContext());
			persistentBeanToDelete = bizlet.preExecute(ImplicitActionName.Delete, 
														persistentBeanToDelete, 
														null,
														facesView.getWebContext());
			if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "preExecute", "Exiting " + bizlet.getClass().getName() + ".preExecute: " + persistentBeanToDelete);
		}

		persistence.delete(document, persistentBeanToDelete);
		
		return null;
	}
}
