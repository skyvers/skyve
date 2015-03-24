package org.skyve.wildcat.web.faces.actions;

import java.util.List;
import java.util.logging.Level;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.util.Binder;
import org.skyve.util.Util;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.web.WebContext;
import org.skyve.wildcat.metadata.customer.CustomerImpl;
import org.skyve.wildcat.metadata.model.document.DocumentImpl;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.web.faces.FacesAction;
import org.skyve.wildcat.web.faces.beans.FacesView;

/**
 * Adds an element to an array
 */
public class AddAction extends FacesAction<Void> {
	private FacesView<? extends Bean> facesView;
	private String listBinding;
	private boolean inline;
	public AddAction(FacesView<? extends Bean> facesView, String listBinding, boolean inline) {
		this.facesView = facesView;
		this.listBinding = listBinding;
		this.inline = inline;
	}

	@Override
	public Void callback() throws Exception {
		String viewBinding = facesView.getViewBinding();
		if (UtilImpl.FACES_TRACE) Util.LOGGER.info("AddAction - listBinding=" + listBinding + 
													" : facesView.viewBinding=" + viewBinding + 
													" : facesView.inline=" + inline);
		if ((! inline) && (! FacesAction.validateRequiredFields())) {
			return null;
		}
		
		StringBuilder collectionBinding = new StringBuilder(32);
		if (viewBinding != null) {
			collectionBinding.append(viewBinding).append('.');
		}
		collectionBinding.append(listBinding);

		Bean bean = facesView.getBean();
		String bizModule = bean.getBizModule();
    	String bizDocument = bean.getBizDocument();

		User user = CORE.getUser();
    	Customer customer = user.getCustomer();
    	Module module = customer.getModule(bizModule);
    	Document document = module.getDocument(customer, bizDocument);
    	Bean parentBean = facesView.getCurrentBean().getBean();
    	
    	// Create a new element
    	TargetMetaData target = Binder.getMetaDataForBinding(customer, module, document, collectionBinding.toString());
		Collection targetCollection = (Collection) target.getAttribute();
		Document collectionDocument = module.getDocument(customer, targetCollection.getDocumentName());
		Bean newBean = collectionDocument.newInstance(user);

		// Set the parent of a child bean, if applicable
		if (newBean instanceof ChildBean<?>) {
			Document parentDocument = collectionDocument.getParentDocument(customer);
			String parentModuleName = parentDocument.getOwningModuleName();
			String parentDocumentName = parentDocument.getName();

			// Check if processBean.setParent() can be called or not.
			// The processBean may be a child of some other bean and just being added to another collection here.
			// Or it could be a derived document, so need to check inheritance as well.
			CustomerImpl internalCustomer = (CustomerImpl) customer;
			Document parentBeanDocument = customer.getModule(parentBean.getBizModule()).getDocument(customer, parentBean.getBizDocument());
			while (parentBeanDocument != null) {
				if (parentModuleName.equals(parentBeanDocument.getOwningModuleName()) &&
						parentDocumentName.equals(parentBeanDocument.getName())) {
					@SuppressWarnings("unchecked")
					ChildBean<Bean> uncheckedNewBean = (ChildBean<Bean>) newBean;
					uncheckedNewBean.setParent(parentBean);
					parentBeanDocument = null;
				}
				else {
					String baseDocumentName = internalCustomer.getBaseDocument(parentBeanDocument);
    				if (baseDocumentName == null) {
    					parentBeanDocument = null;
    				}
    				else {
        				int dotIndex = baseDocumentName.indexOf('.');
        				Module baseModule = customer.getModule(baseDocumentName.substring(0, dotIndex));
        				parentBeanDocument = baseModule.getDocument(customer, baseDocumentName.substring(dotIndex + 1));
    				}
				}
			}
		}

		// Call the bizlet
		Bizlet<Bean> bizlet = ((DocumentImpl) collectionDocument).getBizlet(customer);
		if (bizlet != null) {
			WebContext webContext = facesView.getWebContext();
			CustomerImpl internalCustomer = (CustomerImpl) customer;
			boolean vetoed = internalCustomer.interceptBeforePreExecute(ImplicitActionName.Add, newBean, parentBean, webContext);
			if (! vetoed) {
				if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "preExecute", "Entering " + bizlet.getClass().getName() + ".preExecute: " + ImplicitActionName.Add + ", " + newBean + ", " + facesView.getBean() + ", " + webContext);
				newBean = bizlet.preExecute(ImplicitActionName.Add, newBean, parentBean, webContext);
				if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "preExecute", "Exiting " + bizlet.getClass().getName() + ".preExecute: " + newBean);
				internalCustomer.interceptAfterPreExecute(ImplicitActionName.Add, newBean, parentBean, webContext);
			}
		}

		// Add the new element to the collection
		@SuppressWarnings("unchecked")
		List<Bean> beans = (List<Bean>) Binder.get(bean, collectionBinding.toString());
		beans.add(newBean);

		if (! inline) {
			collectionBinding.append("ElementById(").append(newBean.getBizId()).append(')');
			
	    	facesView.setViewBinding(collectionBinding.toString());
	    	ActionUtil.redirect(facesView, newBean);
		}
		
    	return null;
	}
}
