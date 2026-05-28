package org.skyve.impl.web.faces.actions;

import java.util.List;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.impl.web.faces.models.BeanMapAdapter;
import org.skyve.impl.web.faces.models.SkyveLazyDataModel;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.metadata.view.widget.FilterParameter;
import org.skyve.metadata.view.widget.bound.Parameter;
import org.skyve.util.logging.Category;
import org.slf4j.Logger;

/**
 * Executes a Faces callback action within the current Skyve web context.
 */
public class GetBeansAction extends FacesAction<List<BeanMapAdapter>> {

    private static final Logger FACES_LOGGER = Category.FACES.logger();

	private FacesView facesView;
	private String bizModule;
	private String bizDocument;
	private String queryName;
	private String modelName;
	private List<FilterParameter> filterParameters;
	private List<Parameter> parameters;
	private boolean escape;

	/**
	 * Creates a list-bean retrieval action for a specific query/model context.
	 *
	 * @param facesView the active faces view
	 * @param bizModule the module name
	 * @param bizDocument the document name
	 * @param queryName the query name
	 * @param modelName the model name
	 * @param filterParameters filter parameter definitions
	 * @param parameters query parameter definitions
	 * @param escape whether output values should be escaped
	 */
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
	
	/**
	 * Loads the first page of mapped bean rows for the configured list model context.
	 *
	 * @return the first page of mapped bean rows
	 * @throws Exception if list model creation or loading fails
	 */
	@Override
	public List<BeanMapAdapter> callback() throws Exception {
		if (UtilImpl.FACES_TRACE) FACES_LOGGER.info("GetBeansAction - bizModule={} : bizDocument={} : queryName={} : modelName={}", bizModule, bizDocument, queryName, modelName);

		SkyveLazyDataModel model = new SkyveLazyDataModel(facesView, bizModule, bizDocument, queryName, modelName, filterParameters, parameters, escape);
		return model.load(0, 250, null, null);
	}
}