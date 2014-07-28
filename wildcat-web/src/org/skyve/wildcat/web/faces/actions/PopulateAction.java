package org.skyve.wildcat.web.faces.actions;

import org.skyve.domain.Bean;
import org.skyve.metadata.module.query.Query;
import org.skyve.wildcat.web.faces.FacesAction;
import org.skyve.wildcat.web.faces.beans.FacesView;

/**
 * Just set the state of the list bean up - but do not load any data
 */
public class PopulateAction extends FacesAction<Void> {
	private FacesView<? extends Bean> facesView;
	public PopulateAction(FacesView<? extends Bean> facesView) {
		this.facesView = facesView;
	}
	
	@Override
	public Void callback() throws Exception {
		Query query = ActionUtil.getQuery(facesView.getBizModuleParameter(), facesView.getQueryNameParameter());
		facesView.setBizDocumentParameter(query.getDocumentName());
		facesView.setTitle(query.getDisplayName());
		
		return null;
	}
}
