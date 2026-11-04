package org.skyve.impl.web.faces.actions;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.WebUtil;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.user.User;
import org.skyve.util.logging.Category;
import org.slf4j.Logger;

/**
 * Adds an element to an array.
 * The onAddedHandlers event actions is not implemented since the grid cannot be inlined.
 */
public class AddAction extends FacesAction<Void> {
    private static Logger FACES_LOGGER = Category.FACES.logger();
    
	private FacesView facesView;
	private String dataWidgetBinding;
	private boolean inline;

	/**
	 * Creates an add action for the given collection binding.
	 */
	public AddAction(FacesView facesView, String dataWidgetBinding, boolean inline) {
		this.facesView = facesView;
		this.dataWidgetBinding = dataWidgetBinding;
		this.inline = inline;
	}

	/**
	 * Creates and inserts a new collection element (see {@link WebUtil#addNewElement}), then, unless inline,
	 * zooms into the new element's view.
	 */
	@Override
	public Void callback() throws Exception {
		String viewBinding = facesView.getViewBinding();
		if (UtilImpl.FACES_TRACE) FACES_LOGGER.info("AddAction - dataWidgetBinding={} : facesView.viewBinding={} : facesView.inline={}", 
													dataWidgetBinding, viewBinding, Boolean.valueOf(inline));
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
		User user = CORE.getUser();
		Bean newBean = WebUtil.addNewElement(user,
												bean,
												newViewBinding.toString(),
												facesView.getCurrentBean().getBean(),
												facesView.getWebContext());
		if ((newBean != null) && (! inline)) {
			newViewBinding.append("ElementById(").append(newBean.getBizId()).append(')');
			zoomInBinding.append("ElementById(").append(newBean.getBizId()).append(')');
			
			// If not inline, then we want to call post render on the newly added bean
			Customer customer = user.getCustomer();
			Document newDocument = customer.getModule(newBean.getBizModule()).getDocument(customer, newBean.getBizDocument());
			facesView.setPostRender(((DocumentImpl) newDocument).getBizlet(customer), bean);
			
			facesView.setViewBinding(newViewBinding.toString());
	    	facesView.getZoomInBindings().push(zoomInBinding.toString());
			if (UtilImpl.FACES_TRACE) { 
				FACES_LOGGER.info("Push ZoomInBinding {}", zoomInBinding);
				FACES_LOGGER.info("Set ViewBinding {}", newViewBinding);
			}

	    	ActionUtil.redirectViewScopedConversation(facesView, true);
		}
		
    	return null;
	}
}
