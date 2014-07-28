package org.skyve.wildcat.web.faces.actions;

import java.util.List;
import java.util.logging.Level;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.util.Binder;
import org.skyve.util.Binder.TargetMetaData;
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
	public AddAction(FacesView<? extends Bean> facesView, String listBinding) {
		this.facesView = facesView;
		this.listBinding = listBinding;
	}

	@Override
	public Void callback() throws Exception {
    	StringBuilder newViewBinding = new StringBuilder(32);
    	String viewBinding = facesView.getViewBinding();
		if (viewBinding != null) {
			newViewBinding.append(viewBinding).append('.');
		}
		newViewBinding.append(listBinding);

		String bizModule = facesView.getBizModuleParameter();
    	String bizDocument = facesView.getBizDocumentParameter();

		User user = CORE.getUser();
    	Customer customer = user.getCustomer();
    	Module module = customer.getModule(bizModule);
    	Document document = module.getDocument(customer, bizDocument);
		
    	// Create a new element
    	TargetMetaData target = Binder.getMetaDataForBinding(customer, module, document, newViewBinding.toString());
		Collection targetCollection = (Collection) target.getAttribute();
		Document collectionDocument = module.getDocument(customer, targetCollection.getDocumentName());
		Bean newBean = collectionDocument.newInstance(user);
		
		// Call the bizlet
		Bizlet<Bean> bizlet = ((DocumentImpl) collectionDocument).getBizlet(customer);
		if (bizlet != null) {
			UtilImpl.LOGGER.info("PRE-EXECUTE on " + ImplicitActionName.Add);
			if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "preExecute", "Entering " + bizlet.getClass().getName() + ".preExecute: " + ImplicitActionName.Add + ", " + newBean + ", " + facesView.getBean() + ", " + facesView.getWebContext());
			newBean = bizlet.preExecute(ImplicitActionName.Add, newBean, facesView.getBean(), facesView.getWebContext());
			if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "preExecute", "Exiting " + bizlet.getClass().getName() + ".preExecute: " + newBean);
		}

		// Add the new element to the collection
		@SuppressWarnings("unchecked")
		List<Bean> beans = (List<Bean>) Binder.get(facesView.getBean(), newViewBinding.toString());
		beans.add(newBean);

		newViewBinding.append("ElementById(").append(newBean.getBizId()).append(')');
		
    	facesView.setViewBinding(newViewBinding.toString());
    	ActionUtil.redirect(facesView, newBean);

    	return null;
	}
}
