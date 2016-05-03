package org.skyve.impl.web.faces.actions;

import org.skyve.domain.Bean;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.impl.web.faces.beans.FacesView;
import org.skyve.metadata.module.query.DocumentQueryDefinition;
import org.skyve.util.Util;

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
		facesView.setTitle(query.getDescription());
		
		return null;
	}
}
