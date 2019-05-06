package org.skyve.impl.web.faces.actions;

import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.impl.web.faces.beans.FacesView;
import org.skyve.impl.web.faces.models.BeanMapAdapter;
import org.skyve.impl.web.faces.models.SkyveLazyDataModel;
import org.skyve.metadata.view.widget.bound.FilterParameter;
import org.skyve.util.Util;

public class GetBeansAction extends FacesAction<List<BeanMapAdapter<Bean>>> {
	private FacesView<? extends Bean> facesView;
	private String bizModule;
	private String bizDocument;
	private String queryName;
	private String modelName;
	private List<FilterParameter> parameters;

	public GetBeansAction(FacesView<? extends Bean> facesView,
							String bizModule,
							String bizDocument,
							String queryName,
							String modelName,
							List<FilterParameter> parameters) {
		this.facesView = facesView;
		this.bizModule = bizModule;
		this.bizDocument = bizDocument;
		this.queryName = queryName;
		this.modelName = modelName;
		this.parameters = parameters;
	}
	
	@Override
	public List<BeanMapAdapter<Bean>> callback() throws Exception {
		if (UtilImpl.FACES_TRACE) Util.LOGGER.info("GetBeansAction - bizModule=" + bizModule + 
														" : bizDocument=" + bizDocument + 
														" : queryName=" + queryName + 
														" : modelName=" + modelName);

		SkyveLazyDataModel model = new SkyveLazyDataModel(facesView, bizModule, bizDocument, queryName, modelName, parameters);
		return model.load(0, 250, null, null);
	}
}
