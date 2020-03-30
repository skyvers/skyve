package org.skyve.impl.web.faces.actions;

import java.util.List;
import java.util.logging.Level;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.domain.messages.SecurityException;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.impl.web.faces.beans.FacesView;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.util.Binder;
import org.skyve.util.Util;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.web.WebContext;

/**
 * Adds an element to an array.
 * The onAddedHandlers event actions is not implemented since the grid cannot be inlined.
 */
public class AddAction extends FacesAction<Void> {
	private FacesView<? extends Bean> facesView;
	private String dataWidgetBinding;
	private boolean inline;
	public AddAction(FacesView<? extends Bean> facesView, String dataWidgetBinding, boolean inline) {
		this.facesView = facesView;
		this.dataWidgetBinding = dataWidgetBinding;
		this.inline = inline;
	}

	@Override
	public Void callback() throws Exception {
		String viewBinding = facesView.getViewBinding();
		if (UtilImpl.FACES_TRACE) Util.LOGGER.info("AddAction - dataWidgetBinding=" + dataWidgetBinding + 
													" : facesView.viewBinding=" + viewBinding + 
													" : facesView.inline=" + inline);
		if ((! inline) && (! FacesAction.validateRequiredFields())) {
			return null;
		}
		
		StringBuilder newViewBinding = new StringBuilder(32);
		StringBuilder zoomInBinding = new StringBuilder(32);
		if (viewBinding != null) {
			newViewBinding.append(viewBinding).append('.');
		}
		newViewBinding.append(dataWidgetBinding);
		zoomInBinding.append(dataWidgetBinding);

		Bean bean = facesView.getBean();
		String bizModule = bean.getBizModule();
    	String bizDocument = bean.getBizDocument();

		User user = CORE.getUser();
    	Customer customer = user.getCustomer();
    	Module module = customer.getModule(bizModule);
    	Document document = module.getDocument(customer, bizDocument);
    	Bean parentBean = facesView.getCurrentBean().getBean();
    	
    	// Create a new element
    	TargetMetaData target = Binder.getMetaDataForBinding(customer, module, document, newViewBinding.toString());
		Relation targetRelation = (Relation) target.getAttribute();
		Document relationDocument = module.getDocument(customer, targetRelation.getDocumentName());
    	Persistent persistent = relationDocument.getPersistent();

    	// check for create privilege if the collection is persistent and the collection document is persistent
    	if (targetRelation.isPersistent() && // collection is persistent
    			(persistent != null) && (persistent.getName() != null) && // collection document is persistent
    			(! user.canCreateDocument(relationDocument))) {
			throw new SecurityException("create this data", user.getName());
		}

    	// Create the new bean
    	Bean newBean = relationDocument.newInstance(user);

    	// set bizOrdinal if this is an ordered child collection
		if ((targetRelation instanceof Collection) &&
				Boolean.TRUE.equals(((Collection) targetRelation).getOrdered())) {
			@SuppressWarnings("unchecked")
			List<Bean> beans = (List<Bean>) Binder.get(bean, newViewBinding.toString());
			Binder.set(newBean, Bean.ORDINAL_NAME, Integer.valueOf(beans.size() + 1));
		}

		// Call the bizlet and interceptors
		WebContext webContext = facesView.getWebContext();
		CustomerImpl internalCustomer = (CustomerImpl) customer;
		boolean vetoed = internalCustomer.interceptBeforePreExecute(ImplicitActionName.Add, newBean, parentBean, webContext);
		if (! vetoed) {
			Bizlet<Bean> bizlet = ((DocumentImpl) relationDocument).getBizlet(customer);
			if (bizlet != null) {
				if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "preExecute", "Entering " + bizlet.getClass().getName() + ".preExecute: " + ImplicitActionName.Add + ", " + newBean + ", " + facesView.getBean() + ", " + webContext);
				newBean = bizlet.preExecute(ImplicitActionName.Add, newBean, parentBean, webContext);
				if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "preExecute", "Exiting " + bizlet.getClass().getName() + ".preExecute: " + newBean);
			}
			internalCustomer.interceptAfterPreExecute(ImplicitActionName.Add, newBean, parentBean, webContext);

			// Add the new element to the collection after preExecute
			Binder.addElement(bean, newViewBinding.toString(), newBean);

			if (! inline) {
				newViewBinding.append("ElementById(").append(newBean.getBizId()).append(')');
				zoomInBinding.append("ElementById(").append(newBean.getBizId()).append(')');
				
		    	facesView.setViewBinding(newViewBinding.toString());
		    	facesView.getZoomInBindings().push(zoomInBinding.toString());
				if (UtilImpl.FACES_TRACE) { 
					Util.LOGGER.info("Push ZoomInBinding " + zoomInBinding.toString());
					Util.LOGGER.info("Set ViewBinding " + newViewBinding.toString());
				}

		    	ActionUtil.redirectViewScopedConversation(facesView, newBean);
			}
		}
		
    	return null;
	}
}
