package org.skyve.impl.web.faces.actions;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.util.Util;

/**
 * Just set the state of the list bean up - but do not load any data
 */
public class PopulateAction extends FacesAction<Void> {
	private FacesView facesView;
	public PopulateAction(FacesView facesView) {
		this.facesView = facesView;
	}
	
	@Override
	public Void callback() throws Exception {
		if (UtilImpl.FACES_TRACE) Util.LOGGER.info("PopulateAction");

		String bizModule = facesView.getBizModuleParameter();
		String bizDocument = facesView.getBizDocumentParameter();
		String queryName = facesView.getQueryNameParameter();
		if ((bizDocument != null) && queryName != null) {
			Customer c = CORE.getCustomer();
			Module m = c.getModule(bizModule);
			Document d = m.getDocument(c, bizDocument);
			ListModel<Bean> lm = d.getListModel(c, queryName, true);
			facesView.setTitle(lm.getLocalisedDescription());
			facesView.setModelName(queryName);
			facesView.setQueryNameParameter(null);
		}
		else {
			MetaDataQueryDefinition query = ActionUtil.getMetaDataQuery(bizModule, queryName);
			facesView.setBizDocumentParameter(query.getDocumentName());
			facesView.setTitle(query.getLocalisedDescription());
		}
		
		return null;
	}
}
