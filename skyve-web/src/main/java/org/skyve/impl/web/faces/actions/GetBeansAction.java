package org.skyve.impl.web.faces.actions;

import java.util.List;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.impl.web.faces.models.BeanMapAdapter;
import org.skyve.impl.web.faces.models.SkyveLazyDataModel;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.metadata.view.widget.FilterParameter;
import org.skyve.metadata.view.widget.bound.Parameter;
import org.skyve.util.Util;

public class GetBeansAction extends FacesAction<List<BeanMapAdapter>> {
	private FacesView facesView;
	private String bizModule;
	private String bizDocument;
	private String queryName;
	private String modelName;
	private List<FilterParameter> filterParameters;
	private List<Parameter> parameters;
	private boolean escape;

	public GetBeansAction(FacesView facesView,
							String bizModule,
							String bizDocument,
							String queryName,
							String modelName,
							List<FilterParameter> filterParameters,
							List<Parameter> parameters,
							boolean escape) {
		this.facesView = facesView;
		this.bizModule = bizModule;
		this.bizDocument = bizDocument;
		this.queryName = queryName;
		this.modelName = modelName;
		this.filterParameters = filterParameters;
		this.parameters = parameters;
		this.escape = escape;
	}
	
	@Override
	public List<BeanMapAdapter> callback() throws Exception {
		if (UtilImpl.FACES_TRACE) Util.LOGGER.info("GetBeansAction - bizModule=" + bizModule + 
														" : bizDocument=" + bizDocument + 
														" : queryName=" + queryName + 
														" : modelName=" + modelName);

		SkyveLazyDataModel model = new SkyveLazyDataModel(facesView, bizModule, bizDocument, queryName, modelName, filterParameters, parameters, escape);
		List<BeanMapAdapter> result = model.load(0, 250, null, null);
		return result;
	}
}
