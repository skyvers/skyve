package org.skyve.impl.web.faces.actions;

import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.OptimisticLockException;
import org.skyve.domain.messages.OptimisticLockException.OperationType;
import org.skyve.domain.messages.SecurityException;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.util.logging.Category;
import org.skyve.util.monitoring.Monitoring;
import org.skyve.util.monitoring.RequestKey;
import org.skyve.web.WebContext;
import org.slf4j.Logger;

/**
 * Executes a Faces callback action within the current Skyve web context.
 */
public class DeleteAction extends FacesAction<Void> {
    private static final Logger FACES_LOGGER = Category.FACES.logger();
    private static final Logger BIZLET_LOGGER = Category.BIZLET.logger();

	private FacesView facesView;
	
	/**
	 * Creates a delete action for the current bean in the supplied view context.
	 *
	 * @param facesView the active Faces view state
	 */
	public DeleteAction(FacesView facesView) {
		this.facesView = facesView;
	}

	/**
	 * Deletes the current bean after permission checks, optimistic-lock validation, and pre-execute interception.
	 * As in Save, a bean that is not persisted is only taken through pre-execute and post-render.
	 *
	 * @return {@code null}; this action mutates persistence and view state through side effects
	 * @throws Exception when deletion cannot be completed
	 */
	@Override
	public Void callback() throws Exception {
		if (UtilImpl.FACES_TRACE) FACES_LOGGER.info("DeleteAction");

		AbstractPersistence persistence = AbstractPersistence.get();
		Bean targetBean = ActionUtil.getTargetBeanForView(facesView);
		User user = persistence.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(targetBean.getBizModule());
		Document document = module.getDocument(customer, targetBean.getBizDocument());

		// Only a persisted bean is security checked and deleted
		boolean persisted = (targetBean instanceof PersistentBean) && targetBean.isPersisted();
		if (persisted) {
			targetBean = retrieveForDelete(persistence, user, document, (PersistentBean) targetBean);
		}

		// Run preExecute after the copy is taken, in case we rollback
		WebContext webContext = facesView.getWebContext();
		CustomerImpl internalCustomer = (CustomerImpl) customer;
		boolean vetoed = internalCustomer.interceptBeforePreExecute(ImplicitActionName.Delete, targetBean, null, webContext);
		if (! vetoed) {
			Bizlet<Bean> bizlet = ((DocumentImpl) document).getBizlet(customer);
			if (bizlet != null) {
				if (UtilImpl.BIZLET_TRACE) BIZLET_LOGGER.info("Entering {}.preExecute: {}, {}, null, , {}", bizlet.getClass().getName(), ImplicitActionName.Delete, targetBean, webContext);
				targetBean = bizlet.preExecute(ImplicitActionName.Delete, targetBean, null, webContext);
				if (UtilImpl.BIZLET_TRACE) BIZLET_LOGGER.info("Exiting {}.preExecute: {}", bizlet.getClass().getName(), targetBean);
			}
			internalCustomer.interceptAfterPreExecute(ImplicitActionName.Delete, targetBean, null, webContext);

			if (persisted) {
				persistence.delete(document, (PersistentBean) targetBean);
			}
			
			// We want to call post render
			facesView.setPostRender(bizlet, targetBean);
		}
		
		Monitoring.measure(RequestKey.delete(document));

		return null;
	}

	/**
	 * Checks that the user can delete a persisted bean and retrieves the latest copy of it to delete.
	 *
	 * @param persistence the current persistence
	 * @param user the current user
	 * @param document the bean's document
	 * @param beanToDelete the view's bean
	 * @return the latest persisted copy of the bean
	 * @throws SecurityException when the user cannot delete or read the bean
	 * @throws ValidationException when the bean was deleted by someone else
	 * @throws OptimisticLockException when the bean was changed by someone else
	 */
	private static PersistentBean retrieveForDelete(AbstractPersistence persistence,
														User user,
														Document document,
														PersistentBean beanToDelete) {
		if (! user.canDeleteDocument(document)) {
			throw new SecurityException("delete this data", user.getName());
		}
		
		// Ensure that we are working on the latest of everything and no related entities are pointing to old data
		persistence.evictAllCached();
		PersistentBean result = persistence.retrieve(document, beanToDelete.getBizId());

		if (result == null) { // deleted by another user
			throw new ValidationException(new Message("Failed to delete this information as it was already deleted by someone else after you looked at it."));
		}

		if (! user.canReadBean(result.getBizId(), 
								result.getBizModule(), 
								result.getBizDocument(), 
								result.getBizCustomer(), 
								result.getBizDataGroupId(), 
								result.getBizUserId())) {
			throw new SecurityException("read this data", user.getName());
		}
		
		if (! result.getBizLock().equals(beanToDelete.getBizLock())) {
			throw new OptimisticLockException(user, 
												OperationType.delete, 
												result.getBizLock());
		}
		
		return result;
	}
}
