package org.skyve.impl.web.faces.beans;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Stack;
import java.util.TreeMap;

import javax.annotation.PostConstruct;
import javax.faces.application.FacesMessage;
import javax.faces.bean.ManagedBean;
import javax.faces.bean.ViewScoped;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.primefaces.component.datatable.DataTable;
import org.primefaces.event.SelectEvent;
import org.primefaces.model.DualListModel;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.view.widget.bound.FilterParameterImpl;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.DynamicImageServlet;
import org.skyve.impl.web.faces.FacesUtil;
import org.skyve.impl.web.faces.actions.ActionUtil;
import org.skyve.impl.web.faces.actions.AddAction;
import org.skyve.impl.web.faces.actions.DeleteAction;
import org.skyve.impl.web.faces.actions.ExecuteActionAction;
import org.skyve.impl.web.faces.actions.GetBeansAction;
import org.skyve.impl.web.faces.actions.GetContentURLAction;
import org.skyve.impl.web.faces.actions.PreRenderAction;
import org.skyve.impl.web.faces.actions.RemoveAction;
import org.skyve.impl.web.faces.actions.RerenderAction;
import org.skyve.impl.web.faces.actions.SaveAction;
import org.skyve.impl.web.faces.actions.SetTitleAction;
import org.skyve.impl.web.faces.actions.ZoomInAction;
import org.skyve.impl.web.faces.actions.ZoomOutAction;
import org.skyve.impl.web.faces.models.BeanMapAdapter;
import org.skyve.impl.web.faces.models.SkyveLazyDataModel;
import org.skyve.impl.web.faces.pipeline.ResponsiveFormGrid;
import org.skyve.metadata.FilterOperator;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.view.widget.bound.FilterParameter;

@ViewScoped
@ManagedBean(name = "skyve")
public class FacesView<T extends Bean> extends Harness {
	private static final long serialVersionUID = 3331890232012703780L;

	// NB whatever state is added here needs to be handled by hydrate/dehydrate

	// This is set from a request attribute (the attribute is set in home.jsp)
	// NB This should be set once on post construct of the bean and it persists during all ajax requests.
	// NNB hydrate/dehydrate does not clear/set this property
	private UxUi uxui;
	private String viewBinding;
	// The page title
	private String title;
	private AbstractWebContext webContext;
	// The bean currently under edit (for the view binding)
	private BeanMapAdapter<T> currentBean = null;
	// A stack of referring urls set as we edit beans from a list grid
	private Stack<String> history = new Stack<>();
	private Map<String, SkyveLazyDataModel> models = new TreeMap<>();
 	private Map<String, DualListModel<DomainValue>> listMembershipModels = new TreeMap<>();
	private Map<String, List<BeanMapAdapter<Bean>>> beans = new TreeMap<>();

	@PostConstruct
	protected void postConstruct() {
		this.uxui = (UxUi) FacesContext.getCurrentInstance().getExternalContext().getRequestMap().get(FacesUtil.UX_UI_KEY);
	}
	
	public void preRender() {
		FacesContext fc = FacesContext.getCurrentInstance();
		if (! fc.isPostback()) {
			new PreRenderAction<>(this).execute();
		}
		else if (UtilImpl.FACES_TRACE) {
			UtilImpl.LOGGER.info("FacesView - POSTPACK a=" + getWebActionParameter() + 
									" : m=" + getBizModuleParameter() + 
									" : d=" + getBizDocumentParameter() + 
									" : q=" + getQueryNameParameter() + 
									" : i=" + getBizIdParameter());
		}
	}

 	public UxUi getUxUi() {
		return uxui;
	}
	public void setUxUi(UxUi uxui) {
		this.uxui = uxui;
		FacesContext.getCurrentInstance().getExternalContext().getRequestMap().put(FacesUtil.UX_UI_KEY, uxui);
	}
	
	public String getViewBinding() {
		return viewBinding;
	}
	public void setViewBinding(String viewBinding) {
		this.viewBinding = viewBinding;
	}
	
	public String getTitle() {
		return title;
	}
	public void setTitle(String title) {
		this.title = title;
	}
	
	public Stack<String> getHistory() {
		return history;
	}
	
	// The edited bean
	@SuppressWarnings("unchecked")
	public T getBean() {
        return (T) ((webContext == null) ? null : webContext.getCurrentBean());
    }
	@SuppressWarnings("unchecked")
	public void setBean(T bean)
	throws Exception {
		if (webContext != null) {
			webContext.setCurrentBean(bean);
		}
		currentBean = new BeanMapAdapter<>((T) ActionUtil.getTargetBeanForViewAndCollectionBinding(this, null, null));
	}

	public BeanMapAdapter<T> getCurrentBean() {
		return currentBean;
	}

	private long id = 0;
	public String nextId() {
		return new StringBuilder(10).append('s').append(id++).toString();
	}

	public AbstractWebContext getWebContext() {
		return webContext;
	}
	public void setWebContext(AbstractWebContext webContext) {
		this.webContext = webContext;
	}

	public void ok() {
		if (UtilImpl.FACES_TRACE) UtilImpl.LOGGER.info("FacesView - ok");
		new SaveAction<>(this, true).execute();
		
		FacesContext c = FacesContext.getCurrentInstance();
		if (c.getMessageList().isEmpty()) {
			try {
				c.getExternalContext().redirect(history.pop());
			}
			catch (IOException e) {
				e.printStackTrace();
			}
		}
	}

	public void save() {
		if (UtilImpl.FACES_TRACE) UtilImpl.LOGGER.info("FacesView - save");
		new SaveAction<>(this, false).execute();
		new SetTitleAction(this).execute();
		
		FacesContext c = FacesContext.getCurrentInstance();
		if (c.getMessageList().isEmpty()) {
			c.addMessage(null, new FacesMessage("Saved", "Any changes have been saved"));
		}
	}
	
	public void cancel() {
		if (UtilImpl.FACES_TRACE) UtilImpl.LOGGER.info("FacesView - cancel");
		try {
			FacesContext.getCurrentInstance().getExternalContext().redirect(history.pop());
		}
		catch (IOException e) {
			e.printStackTrace();
		}
	}

	public void delete() {
		if (UtilImpl.FACES_TRACE) UtilImpl.LOGGER.info("FacesView - delete");
		new DeleteAction(this).execute();

		FacesContext c = FacesContext.getCurrentInstance();
		if (c.getMessageList().isEmpty()) {
			try {
				c.getExternalContext().redirect(history.pop());
			}
			catch (IOException e) {
				e.printStackTrace();
			}
		}
	}

	// This corresponds to the lower case action name used in data grid generation (there is already edit())
	public void navigate(String listBinding, String bizId) {
		if (UtilImpl.FACES_TRACE) UtilImpl.LOGGER.info("FacesView - zoom in to " + listBinding + '.' + bizId);
		new ZoomInAction(this, listBinding, bizId).execute();
		if (UtilImpl.FACES_TRACE) UtilImpl.LOGGER.info("FacesView - view binding now " + viewBinding);
	}
	
	// for navigate-on-select in data grids
	public void navigate(SelectEvent evt) {
		@SuppressWarnings("unchecked")
		String bizId = ((BeanMapAdapter<Bean>) evt.getObject()).getBean().getBizId();
		String listBinding = ((DataTable) evt.getComponent()).getVar();
		navigate(listBinding.replace('_',  '.'), bizId);
	}
	
	public void add(String listBinding, boolean inline) {
		if (UtilImpl.FACES_TRACE) UtilImpl.LOGGER.info("FacesView - add to " + listBinding + (inline ? " inline" : " with zoom"));
		new AddAction(this, listBinding, inline).execute();
		if (inline && UtilImpl.FACES_TRACE) UtilImpl.LOGGER.info("FacesView - view binding now " + viewBinding);
	}
	
	public void zoomout() {
		if (UtilImpl.FACES_TRACE) UtilImpl.LOGGER.info("FacesView - zoomout");
		new ZoomOutAction(this).execute();
	}

	/**
	 * This method only removes elements from collections, it doesn't null out associations.
	 */
	public void remove(String listBinding, String bizId) {
		if (UtilImpl.FACES_TRACE) UtilImpl.LOGGER.info("FacesView - remove " + viewBinding);
		new RemoveAction(this, listBinding, bizId).execute();
	}

	public void action(String actionName, String listBinding, String bizId) {
		if (UtilImpl.FACES_TRACE) UtilImpl.LOGGER.info("FacesView - EXECUTE ACTION " + actionName + " for grid " + listBinding);
		new ExecuteActionAction<>(this, 
									actionName, 
									UtilImpl.processStringValue(listBinding),
									UtilImpl.processStringValue(bizId)).execute();
	}

	public void rerender(String source, boolean validate) {
		if (UtilImpl.FACES_TRACE) UtilImpl.LOGGER.info("FacesView - EXECUTE RERENDER with source " + source);
		new RerenderAction<>(this, source, validate).execute();
	}

	public SkyveLazyDataModel getModel(String moduleName, 
										String documentName, 
										String queryName,
										String modelName,
										List<List<String>> filterCriteria) {
		String key = null;
		if ((moduleName != null) && (queryName != null)) {
			key = String.format("%s.%s", moduleName, queryName);
		}
		else if ((moduleName != null) && (documentName != null)) {
			if (modelName != null) {
				key = String.format("%s.%s.%s", moduleName, documentName, modelName);
			}
			else {
				key = String.format("%s.%s", moduleName, documentName);
			}
		}
		SkyveLazyDataModel result = models.get(key);

		if (result == null) {
			// Collect the filter parameters from the criteria sent
			List<FilterParameter> filterParameters = null;
			if (filterCriteria != null) {
				filterParameters = new ArrayList<>(filterCriteria.size());
				for (List<String> filterCriterium : filterCriteria) {
					FilterParameterImpl param = new FilterParameterImpl();
					param.setName(filterCriterium.get(0));
					param.setOperator(FilterOperator.valueOf(filterCriterium.get(1)));
					param.setValue(filterCriterium.get(2));
					filterParameters.add(param);
				}
			}
			
			result = new SkyveLazyDataModel(this, moduleName, documentName, queryName, modelName, filterParameters);
			models.put(key, result);
		}
 		
		return result;
	}
	
	// Note - this is also called from EL in ListGrid tag
 	public List<BeanMapAdapter<Bean>> getBeans(final String bizModule, 
												final String queryName,
												final Map<String, Object> parameters) {
 		List<BeanMapAdapter<Bean>> result = null;
 		
 		// these are ultimately web parameters that may not be present in the request
 		if ((queryName == null) || queryName.isEmpty()) {
 			result = new ArrayList<>();
 		}
 		else {
	 		StringBuilder key = new StringBuilder(64).append(bizModule).append('.').append(queryName);
	 		if (parameters != null) {
	 			for (String parameterName : parameters.keySet()) {
	 				key.append('.').append(parameterName).append('=').append(parameters.get(parameterName));
	 			}
	 		}
	 		if (UtilImpl.FACES_TRACE) UtilImpl.LOGGER.info("FacesView - LIST KEY = " + key);
			result = beans.get(key.toString());
			if (result == null) {
				List<Bean> interimBeans = new GetBeansAction(bizModule, queryName, parameters).execute();
				result = new ArrayList<>(interimBeans.size());
				for (Bean bean : interimBeans) {
					result.add(new BeanMapAdapter<>(bean));
				}
				beans.put(key.toString(), result);
			}
 		}
 		
		return result;
	}
 	
 	private static List<DomainValue> domainValues = null;
 	private DualListModel<DomainValue> listMembershipModel = new DualListModel<>();
 	public DualListModel<DomainValue> getListMembershipModel() {
 		if (domainValues == null) {
 			domainValues = new ArrayList<>();
 			domainValues.add(new DomainValue("FUCKED"));
 		}
 		return listMembershipModel;
 	}
	public void setListMembershipModel(DualListModel<DomainValue> listMembershipModel) {
		this.listMembershipModel = listMembershipModel;
	}
/*
	public Map<String, DomainValueDualListModel> getListMembershipModels() {
 		DomainValueDualListModel result = listMembershipModels.get(binding);
 		if (result == null) {
 			result = new DomainValueDualListModel(new ArrayList<DomainValue>(), new ArrayList<DomainValue>());
 			listMembershipModels.put(binding, result);
 		}
 		
 		return result;
 	}
*/ 	
 	public String getContentUrl(final String binding) {
		return new GetContentURLAction(getBean(), binding).execute();
 	}
 	
 	public String getDynamicImageUrl(String name, 
 										String moduleName,
 										String documentName,
 										Integer pixelWidth, 
 										Integer pixelHeight, 
 										Integer initialPixelWidth, 
 										Integer initialPixelHeight) {
		StringBuilder result = new StringBuilder(128);
		result.append("/images/dynamic.png?").append(AbstractWebContext.DOCUMENT_NAME).append('=');
		result.append(moduleName).append('.').append(documentName);
		result.append('&').append(DynamicImageServlet.IMAGE_NAME).append('=').append(name);
		if (pixelWidth != null) {
			result.append('&').append(DynamicImageServlet.IMAGE_WIDTH_NAME).append('=').append(pixelWidth);
		}
		else if (initialPixelWidth != null) {
			result.append('&').append(DynamicImageServlet.IMAGE_WIDTH_NAME).append('=').append(initialPixelWidth);
		}
		else {
			result.append('&').append(DynamicImageServlet.IMAGE_WIDTH_NAME).append("=200");
		}
		if (pixelHeight != null) {
			result.append('&').append(DynamicImageServlet.IMAGE_HEIGHT_NAME).append('=').append(pixelHeight);
		}
		else if (initialPixelHeight != null) {
			result.append('&').append(DynamicImageServlet.IMAGE_HEIGHT_NAME).append('=').append(initialPixelHeight);
		}
		else {
			result.append('&').append(DynamicImageServlet.IMAGE_HEIGHT_NAME).append("=200");
		}
		result.append('&').append(DynamicImageServlet.IMAGE_WIDTH_ZOOM_NAME).append("=100");
		result.append('&').append(DynamicImageServlet.IMAGE_HEIGHT_ZOOM_NAME).append("=100");
		result.append('&').append(AbstractWebContext.CONTEXT_NAME).append('=').append(getWebContext().getWebId());
		result.append("&bizId=").append(getCurrentBean().getBean().getBizId());
		result.append("_ts=").append(System.currentTimeMillis());
		
		return result.toString();
 	}

 	public List<BeanMapAdapter<Bean>> complete(String query) {
		UIComponent currentComponent = UIComponent.getCurrentComponent(FacesContext.getCurrentInstance());
		Map<String, Object> attributes = currentComponent.getAttributes();
		String completeModule = (String) attributes.get("module");
		String completeQuery = (String) attributes.get("query");
		String displayBinding = (String) attributes.get("display");
		Map<String, Object> parameters = new TreeMap<>();
		parameters.put(displayBinding, query);
		if (UtilImpl.FACES_TRACE) UtilImpl.LOGGER.info("FacesView - COMPLETE = " + completeModule + "." + completeQuery + " : " + query);
		return getBeans(completeModule, completeQuery, parameters);
	}
	
	// Used to hydrate the state after dehydration in SkyvePhaseListener.afterRestoreView()
	private String webId;
	public String getWebId() {
		return webId;
	}

 	// restore the webContext and current bean etc
	public void hydrate(AbstractWebContext newWebContext)
	throws Exception {
		if (UtilImpl.FACES_TRACE) UtilImpl.LOGGER.info("FacesView - hydrate");
		webContext = newWebContext;
		webId = null;
		setBean(getBean());
	}

	// remove the webContext and current bean etc leaving only the webId
	public void dehydrate() {
		if (UtilImpl.FACES_TRACE) UtilImpl.LOGGER.info("FacesView - dehydrate");
		if (webContext != null) {
			webId = webContext.getWebId();
		}
		webContext = null;
		models.clear();
		listMembershipModels.clear();
		beans.clear();
		currentBean = null;
	}
	
	/**
	 * This method produces a style class that implements 
	 * the form column/row contract in the skyve view metadata.
	 * This method is called within the div styleClass attribute
	 * in form layouts.
	 * @param formIndex	The form to get the style for.
	 * @param colspan	The colspan to style for.
	 * @return	The responsive grid style classes required.
	 */
	@SuppressWarnings({"unchecked", "static-method"})
	public String getResponsiveFormStyle(int formIndex, String alignment, int colspan) {
		List<ResponsiveFormGrid> formStyles = (List<ResponsiveFormGrid>) FacesContext.getCurrentInstance().getViewRoot().getAttributes().get(FacesUtil.FORM_STYLES_KEY);
		String result = formStyles.get(formIndex).getStyle(colspan);
		if (alignment != null) {
			result = String.format("%s %s", result, alignment);
		}
		
		return result;
	}
	
	/**
	 * This method produces a style class for each edit view form row.
	 * The side-effect is that the style is reset for the new row to layout.
	 * This method is called within the div styleClass attribute
	 * in form layouts.
	 * @param formIndex	The form to reset the style for.
	 * @return ui-g-12 ui-g-nopad
	 */
	@SuppressWarnings({"unchecked", "static-method"})
	public String resetResponsiveFormStyle(int formIndex) {
		List<ResponsiveFormGrid> formStyles = (List<ResponsiveFormGrid>) FacesContext.getCurrentInstance().getViewRoot().getAttributes().get(FacesUtil.FORM_STYLES_KEY);
		formStyles.get(formIndex).reset();
		return "ui-g-12 ui-g-nopad";
	}
}
