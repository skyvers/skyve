package org.skyve.impl.web.service;

import java.io.IOException;
import java.io.PrintWriter;
import java.lang.reflect.InvocationTargetException;
import java.security.Principal;
import java.util.List;
import java.util.Map;

import org.skyve.CORE;
import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.SecurityException;
import org.skyve.domain.messages.SessionEndedException;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.metadata.view.model.chart.ChartBuilderMetaData;
import org.skyve.impl.metadata.view.model.chart.TextLengthBucketMetaData;
import org.skyve.impl.metadata.view.widget.Chart.ChartType;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.UserAgent;
import org.skyve.impl.web.WebUtil;
import org.skyve.impl.web.faces.actions.ChartAction;
import org.skyve.impl.web.faces.charts.config.ChartConfigRenderer;
import org.skyve.impl.web.service.smartclient.CompoundFilterOperator;
import org.skyve.impl.web.service.smartclient.SmartClientFilterOperator;
import org.skyve.impl.web.service.smartclient.SmartClientListServlet;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.user.User;
import org.skyve.metadata.user.UserAccess;
import org.skyve.metadata.view.TextOutput.Sanitisation;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.metadata.view.model.chart.Bucket;
import org.skyve.metadata.view.model.chart.ChartBuilder;
import org.skyve.metadata.view.model.chart.ChartData;
import org.skyve.metadata.view.model.chart.ChartModel;
import org.skyve.metadata.view.model.chart.ListGridChartListModel;
import org.skyve.metadata.view.model.chart.MetaDataChartModel;
import org.skyve.metadata.view.model.chart.NumericMultipleBucket;
import org.skyve.metadata.view.model.chart.OrderBy;
import org.skyve.metadata.view.model.chart.TemporalBucket;
import org.skyve.metadata.view.model.chart.TemporalBucket.TemporalBucketType;
import org.skyve.metadata.view.model.chart.TextLengthBucket;
import org.skyve.metadata.view.model.chart.TextStartsWithBucket;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.util.JSON;
import org.skyve.util.OWASP;
import org.skyve.util.Util;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * Chart Servlet - supplies chart config to a chart.
 * 
 * there are 3 usage modes:-
 * 
 * 1) This mode uses the given model to generate its own ChartData object.
 * 		parameters
 * 			webContext
 * 			modelName
 * 2) This mode uses an inline model child element of chart to yield a ChartData object
 * 		parameters
 * 			webContext
 * 			modelId
 * 3) This mode receives a model definition from the client and generates the chart from that
 *		parameters
 *			type - chart type name
 *			builder - ChartBuilderMetaData JSON
 */
public class ChartServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;
	
	private static final String CHART_TYPE_NAME = "t";
	private static final String DATA_SOURCE_NAME = "ds";
	private static final String BUILDER_NAME = "b";
	
	@Override
	protected void doGet(HttpServletRequest request, HttpServletResponse response)
	throws ServletException, IOException {
		response.setContentType(MimeType.json.toString());
		response.setCharacterEncoding(Util.UTF8);
		response.addHeader("Cache-control", "private,no-cache,no-store"); // never
		response.addDateHeader("Expires", 0); // never

		try (PrintWriter pw = response.getWriter()) {
			AbstractPersistence persistence = AbstractPersistence.get();
			try {
				try {
					persistence.begin();
			    	Principal userPrincipal = request.getUserPrincipal();
			    	User user = WebUtil.processUserPrincipalForRequest(request, (userPrincipal == null) ? null : userPrincipal.getName(), true);
					if (user == null) {
						throw new SessionEndedException(request.getLocale());
					}
					persistence.setUser(user);

					String dataSourceName = request.getParameter(DATA_SOURCE_NAME);
					String result = (dataSourceName == null) ? 
										processChartModel(request, response) :
										processListModel(request);
					if (result != null) {
						pw.print(result);
					}
					else {
						pw.print(emptyResponse());
					}
				}
				catch (InvocationTargetException e) {
					throw e.getTargetException();
				}
			}
			catch (Throwable t) {
				t.printStackTrace();
				persistence.rollback();
				pw.print(emptyResponse());
			}
			finally {
				if (persistence != null) {
					persistence.commit(true);
				}
			}
		}
	}
	
	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response)
	throws ServletException, IOException {
		doGet(request, response);
	}
	
	private static String processChartModel(HttpServletRequest request, HttpServletResponse response)
	throws Exception {
		String contextKey = OWASP.sanitise(Sanitisation.text, Util.processStringValue(request.getParameter(AbstractWebContext.CONTEXT_NAME)));
		AbstractWebContext webContext = StateUtil.getCachedConversation(contextKey, request, response);
		Bean bean = WebUtil.getConversationBeanFromRequest(webContext, request);

		User user = CORE.getUser();
		String moduleName = bean.getBizModule();
		String documentName = bean.getBizDocument();
		String modelName = OWASP.sanitise(Sanitisation.text, Util.processStringValue(request.getParameter(AbstractWebContext.MODEL_NAME)));
		UxUi uxui = UserAgent.getUxUi(request);
		String uxuiName = uxui.getName();
		user.checkAccess(UserAccess.modelAggregate(moduleName, documentName, modelName), uxuiName);

		Customer customer = CORE.getCustomer();
		Module module = customer.getModule(bean.getBizModule());
		Document document = module.getDocument(customer, bean.getBizDocument());
		UtilImpl.LOGGER.info("UX/UI = " + uxuiName);

		View view = document.getView(uxuiName,
										customer,
										bean.isCreated() ? 
											ViewType.edit.toString() :
											ViewType.create.toString());

		ChartData data = null;
		// Check for an inline model builder
		ChartBuilderMetaData builder = (ChartBuilderMetaData) view.getInlineModel(modelName);
		if (builder == null) {
			ChartModel<Bean> model = document.getChartModel(customer, modelName, true);
			model.setBean(bean);
			data = model.getChartData();
		}
		else {
			MetaDataChartModel model = new MetaDataChartModel(builder);
			model.setBean(bean);
			data = model.getChartData();
		}
		
		ChartType type = ChartType.valueOf(request.getParameter(CHART_TYPE_NAME));
		org.primefaces.model.charts.ChartModel model = ChartAction.pfChartModel(type, data);
		
		return ChartConfigRenderer.config(type, model);
	}

	private static String processListModel(HttpServletRequest request)
	throws Exception {
		User user = CORE.getUser();
		Customer customer = user.getCustomer();

		String module_QueryOrModel = OWASP.sanitise(Sanitisation.text, request.getParameter(DATA_SOURCE_NAME));
		int _Index = module_QueryOrModel.indexOf('_');
		String moduleName = module_QueryOrModel.substring(0, _Index);
		Module module = customer.getModule(moduleName);
		String documentOrQueryOrModelName = module_QueryOrModel.substring(_Index + 1);
		int __Index = documentOrQueryOrModelName.indexOf("__");
		if (__Index >= 0) { // this is a model
			return emptyResponse();
		}

		UxUi uxui = UserAgent.getUxUi(request);

		MetaDataQueryDefinition query = module.getMetaDataQuery(documentOrQueryOrModelName);
		// not a query, must be a document
		if (query == null) {
			user.checkAccess(UserAccess.documentAggregate(moduleName, documentOrQueryOrModelName), uxui.getName());
			query = module.getDocumentDefaultQuery(customer, documentOrQueryOrModelName);
		}
		// a query
		else {
			user.checkAccess(UserAccess.queryAggregate(moduleName, documentOrQueryOrModelName), uxui.getName());
		}
		if (query == null) {
			throw new ServletException("DataSource does not reference a valid query " + documentOrQueryOrModelName);
		}

		// Check read permission
		Document drivingDocument = module.getDocument(customer, query.getDocumentName());
		if (! user.canReadDocument(drivingDocument)) {
			throw new SecurityException("read this data", user.getName());
		}

		ListGridChartListModel model = new ListGridChartListModel(query);
		model.postConstruct(customer, true);
		
		String tagId = request.getParameter("tagId");
		model.setSelectedTagId(tagId);

		// add filter criteria
		String criteriaString = request.getParameter("criteria");
		@SuppressWarnings("unchecked")
		Map<String, Object> criteria = (Map<String, Object>) JSON.unmarshall(null, criteriaString);
		if (criteria != null) {
			String operator = (String) criteria.get("operator");
			if (operator != null) { // advanced criteria
				@SuppressWarnings("unchecked")
				List<Map<String, Object>> advancedCriteria = (List<Map<String, Object>>) criteria.get("criteria");
				SmartClientListServlet.addAdvancedFilterCriteriaToQuery(module,
																			model.getDrivingDocument(),
																			user,
																			CompoundFilterOperator.valueOf(operator),
																			advancedCriteria,
																			tagId,
																			model);
			}
			else { // simple criteria
				SmartClientListServlet.addSimpleFilterCriteriaToQuery(module,
																		model.getDrivingDocument(),
																		customer,
																		SmartClientFilterOperator.substring,
																		criteria,
																		tagId,
																		model);
			}
		}

		@SuppressWarnings("unchecked")
		Map<String, Object> json = (Map<String, Object>) JSON.unmarshall(null, request.getParameter(BUILDER_NAME));

		String categoryBinding = (String) json.get("categoryBinding");
		String categoryBucketSimpleName = (String) json.get("categoryBucket");
		Bucket categoryBucket = null;
		if (categoryBucketSimpleName != null) {
			if (NumericMultipleBucket.class.getSimpleName().equals(categoryBucketSimpleName)) {
				int multiple = ((Number) json.get("numericMultiple")).intValue();
				categoryBucket = new NumericMultipleBucket(multiple);
			}
			else if (TextLengthBucket.class.getSimpleName().equals(categoryBucketSimpleName)) {
				categoryBucket = new TextLengthBucketMetaData();
			}
			else if (TextStartsWithBucket.class.getSimpleName().equals(categoryBucketSimpleName)) {
				int length = ((Number) json.get("startsWithLength")).intValue();
				boolean caseSensitive = Boolean.TRUE.equals(json.get("startsWithCaseSensitive"));
				categoryBucket = new TextStartsWithBucket(length, caseSensitive);
			}
			else if (TemporalBucket.class.getSimpleName().equals(categoryBucketSimpleName)) {
				TemporalBucketType type = TemporalBucketType.valueOf((String) json.get("temporalBucketType"));
				categoryBucket = new TemporalBucket(type);
			}
		}
		
		String valueBinding = (String) json.get("valueBinding");
		String valueFunctionString = (String) json.get("valueFunction");
		AggregateFunction valueFunction = (valueFunctionString == null) ? null : AggregateFunction.valueOf(valueFunctionString);

		ChartBuilder builder = new ChartBuilder();
		builder.with(model.getDocumentQuery());
		builder.category(categoryBinding.replace('_', '.'), categoryBucket);
		builder.value(valueBinding.replace('_', '.'), valueFunction);

		boolean topOn = Boolean.TRUE.equals(json.get("topOn"));
		if (topOn) {
			int top = ((Number) json.get("top")).intValue();
			OrderBy orderBy = OrderBy.valueOf((String) json.get("topBy"));
			SortDirection sort = SortDirection.valueOf((String) json.get("topSort"));
			boolean includeOthers = Boolean.TRUE.equals(json.get("includeOthers"));
			builder.top(top, orderBy, sort, includeOthers);
		}
		
		boolean orderOn = Boolean.TRUE.equals(json.get("orderOn"));
		if (orderOn) {
			OrderBy orderBy = OrderBy.valueOf((String) json.get("orderBy"));
			SortDirection sort = SortDirection.valueOf((String) json.get("orderSort"));
			builder.orderBy(orderBy, sort);
		}

		String title = (String) json.get("title");
		String label = (String) json.get("label");
		ChartData data = builder.build(title, label);
		
		ChartType type = ChartType.valueOf(request.getParameter(CHART_TYPE_NAME));
		org.primefaces.model.charts.ChartModel pfModel = ChartAction.pfChartModel(type, data);
		
		return ChartConfigRenderer.config(type, pfModel);
	}

	private static String emptyResponse() {
		return "{}";
	}
}
