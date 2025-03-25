package org.skyve.impl.web.faces.actions;

import java.util.List;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.logging.Category;
import org.skyve.web.WebContext;
import org.slf4j.Logger;

/**
 * Remove an element from an array.
 * The onRemovedHandlers event actions are processed here also.
 */
public class RemoveAction extends FacesAction<Void> {

    private static final Logger FACES_LOGGER = Category.FACES.logger();
    private static final Logger BIZLET_LOGGER = Category.BIZLET.logger();

	private FacesView facesView;
	private String collectionName;
	private String elementBizId;
	private List<String> removedHandlerActionNames; // "true/false" means rerender with/without client validation
	
	public RemoveAction(FacesView facesView,
							String collectionName,
							String elementBizId,
							List<String> removedHandlerActionNames) {
		this.facesView = facesView;
		this.collectionName = collectionName;
		this.elementBizId = elementBizId;
		this.removedHandlerActionNames = removedHandlerActionNames;
	}

	@Override
	public Void callback() throws Exception {
		if (UtilImpl.FACES_TRACE) FACES_LOGGER.info("RemoveAction - collectionName=" + collectionName + " : elementBizId=" + elementBizId);

		Bean bean = facesView.getBean();
		String viewBinding = facesView.getViewBinding();
		if ((collectionName != null) && (elementBizId != null)) { // inline remove
			Bean beanToRemove = ActionUtil.getTargetBeanForViewAndReferenceBinding(facesView, collectionName, elementBizId);

			StringBuilder collectionBinding = new StringBuilder(32);
			if (viewBinding != null) {
				collectionBinding.append(viewBinding).append('.');
			}
			collectionBinding.append(collectionName);
			BindUtil.removeElementFromCollection(bean, collectionBinding.toString(), beanToRemove);
			
			// fire onRemovedHandlers after removal
			if (removedHandlerActionNames != null) {
				for (String removedHandlerActionName : removedHandlerActionNames) {
					if (Boolean.TRUE.toString().equals(removedHandlerActionName)) {
						if (UtilImpl.FACES_TRACE) FACES_LOGGER.info("RemoveAction - execute removed handler rerender with client validation");
						new RerenderAction(facesView, collectionName, true).execute();
					}
					else if (Boolean.FALSE.toString().equals(removedHandlerActionName)) {
						if (UtilImpl.FACES_TRACE) FACES_LOGGER.info("RemoveAction - execute removed handler rerender with no client validation");
						new RerenderAction(facesView, collectionName, false).execute();
					}
					else {
						if (UtilImpl.FACES_TRACE) FACES_LOGGER.info(String.format("RemoveAction - execute removed handler [%s] action", removedHandlerActionName));
						new ExecuteActionAction(facesView, removedHandlerActionName, null, null).execute();
					}
				}
			}
			// Update the edit view title just in case
			// NB It isn't actually updated as deleting a row only updates the data table.
			new SetTitleAction(facesView).execute();
		}
		else { // Remove on zoomed view
			int lastCollectionindex = viewBinding.lastIndexOf("ElementById(");
			Bean beanToRemove = (Bean) BindUtil.get(bean, viewBinding);
			if (beanToRemove == null) { // should never happen
				throw new IllegalStateException("beanToRemove is null");
			}
			
			// Run preExecute after the copy is taken, in case we rollback
			WebContext webContext = facesView.getWebContext();
			CustomerImpl internalCustomer = (CustomerImpl) CORE.getPersistence().getUser().getCustomer();
			Module module = internalCustomer.getModule(beanToRemove.getBizModule());
			Document document = module.getDocument(internalCustomer, beanToRemove.getBizDocument());
			boolean vetoed = internalCustomer.interceptBeforePreExecute(ImplicitActionName.Remove, beanToRemove, null, webContext);
			if (! vetoed) {
				Bizlet<Bean> bizlet = ((DocumentImpl) document).getBizlet(internalCustomer);
				if (bizlet != null) {
					if (UtilImpl.BIZLET_TRACE) BIZLET_LOGGER.info("Entering " + bizlet.getClass().getName() + ".preExecute: " + ImplicitActionName.Remove + ", " + beanToRemove + ", null, " + ", " + webContext);
					beanToRemove = bizlet.preExecute(ImplicitActionName.Remove, beanToRemove, null, webContext);
					if (UtilImpl.BIZLET_TRACE) BIZLET_LOGGER.info("Exiting " + bizlet.getClass().getName() + ".preExecute: " + beanToRemove);
				}
				internalCustomer.interceptAfterPreExecute(ImplicitActionName.Remove, beanToRemove, null, webContext);

				if (lastCollectionindex > -1) { // remove from a collection or inverseMany
					BindUtil.removeElementFromCollection(bean, viewBinding.substring(0, lastCollectionindex), beanToRemove);
				}
				else { // remove from an association or inverseOne
					BindUtil.set(bean, viewBinding, null);
				}
				ZoomOutAction.zoomOut(facesView, internalCustomer);
			}
		}

	    return null;
	}
}
