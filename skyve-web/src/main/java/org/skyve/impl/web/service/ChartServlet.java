package org.skyve.impl.web.service;

import java.io.IOException;
import java.io.PrintWriter;
import java.lang.reflect.InvocationTargetException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.skyve.CORE;
import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.SessionEndedException;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.metadata.view.model.chart.ChartBuilderMetaData;
import org.skyve.impl.metadata.view.widget.Chart.ChartType;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.ConversationUtil;
import org.skyve.impl.web.UserAgent;
import org.skyve.impl.web.UserAgentType;
import org.skyve.impl.web.WebUtil;
import org.skyve.impl.web.faces.actions.ChartAction;
import org.skyve.impl.web.faces.charts.config.ChartConfigRenderer;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.Repository;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.router.UxUiSelector;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.metadata.view.model.chart.ChartData;
import org.skyve.metadata.view.model.chart.ChartModel;
import org.skyve.metadata.view.model.chart.MetaDataChartModel;
import org.skyve.util.Util;

/**
 * Chart Servlet - supplies map data to a map display.
 * 
 * there are 2 usage modes:-
 * 
 * 1) This mode uses the given model to generate its own ChartData object.
 * 		parameters
 * 			webContext
 * 			modelName
 * 2) This mode uses an inline model child element of chart to yield a ChartData object
 * 		parameters
 * 			webContext
 * 			modelId
 */
public class ChartServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;
	
	private static final String CHART_TYPE_NAME = "t";
	
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
					User user = WebUtil.processUserPrincipalForRequest(request, request.getUserPrincipal().getName(), true);
					if (user == null) {
						throw new SessionEndedException();
					}
					persistence.setUser(user);

					String result = processModel(request, response);
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
	
	private static String processModel(HttpServletRequest request, HttpServletResponse response)
	throws Exception {
		Customer customer = CORE.getCustomer();
		String contextKey = request.getParameter(AbstractWebContext.CONTEXT_NAME);
		AbstractWebContext webContext = ConversationUtil.getCachedConversation(contextKey, request, response);
		Bean bean = WebUtil.getConversationBeanFromRequest(webContext, request);
		Module module = customer.getModule(bean.getBizModule());
		Document document = module.getDocument(customer, bean.getBizDocument());

		UserAgentType userAgentType = UserAgent.getType(request);
		Router router = CORE.getRepository().getRouter();
		UxUi uxui = ((UxUiSelector) router.getUxuiSelector()).select(userAgentType, request);
		String uxuiName = uxui.getName();
		UtilImpl.LOGGER.info("UX/UI = " + uxuiName);

		View view = document.getView(uxuiName,
										customer,
										bean.isCreated() ? 
											ViewType.edit.toString() :
											ViewType.create.toString());

		String modelName = request.getParameter(AbstractWebContext.MODEL_NAME);
		ChartData data = null;
		// Check for builder if the modelName could be an index
		ChartBuilderMetaData builder = modelName.matches("^[0-9]*$") ?
										(ChartBuilderMetaData) view.getInlineModel(Integer.parseInt(modelName)) :
										null;
		if (builder == null) {
			Repository repository = CORE.getRepository();
			ChartModel<Bean> model = repository.getChartModel(customer, document, modelName, true);
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
	
	private static String emptyResponse() {
		return "{}";
	}
}
