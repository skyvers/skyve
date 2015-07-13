package org.skyve.wildcat.web.faces.actions;

import org.skyve.domain.Bean;
import org.skyve.metadata.module.query.DocumentQueryDefinition;
import org.skyve.util.Util;
import org.skyve.wildcat.util.UtilImpl;
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
		if (UtilImpl.FACES_TRACE) Util.LOGGER.info("PopulateAction");

		DocumentQueryDefinition query = ActionUtil.getDocumentQuery(facesView.getBizModuleParameter(),
																		facesView.getQueryNameParameter());
		facesView.setBizDocumentParameter(query.getDocumentName());
		facesView.setTitle(query.getDisplayName());
		
		return null;
	}
}
