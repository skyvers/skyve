package org.skyve.wildcat.web.faces.beans;

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
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;

import org.primefaces.component.datatable.DataTable;
import org.primefaces.event.SelectEvent;
import org.skyve.domain.Bean;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.user.User;
import org.skyve.web.WebContext;
import org.skyve.wildcat.metadata.repository.AbstractRepository;
import org.skyve.wildcat.persistence.AbstractPersistence;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.web.AbstractWebContext;
import org.skyve.wildcat.web.faces.BeanMapAdapter;
import org.skyve.wildcat.web.faces.FacesUtil;
import org.skyve.wildcat.web.faces.QueryDataModel;
import org.skyve.wildcat.web.faces.actions.ActionUtil;
import org.skyve.wildcat.web.faces.actions.AddAction;
import org.skyve.wildcat.web.faces.actions.DeleteAction;
import org.skyve.wildcat.web.faces.actions.ExecuteActionAction;
import org.skyve.wildcat.web.faces.actions.GetBeansAction;
import org.skyve.wildcat.web.faces.actions.GetResourceURLAction;
import org.skyve.wildcat.web.faces.actions.PreRenderAction;
import org.skyve.wildcat.web.faces.actions.RemoveAction;
import org.skyve.wildcat.web.faces.actions.SaveAction;
import org.skyve.wildcat.web.faces.actions.SetTitleAction;
import org.skyve.wildcat.web.faces.actions.ZoomInAction;
import org.skyve.wildcat.web.faces.actions.ZoomOutAction;

@ViewScoped
@ManagedBean(name = "wildcat")
public class FacesView<T extends Bean> extends Harness {
	private static final long serialVersionUID = 3331890232012703780L;

	private String uxui;
	public String getUxUi() {
		return uxui;
	}
	public void setUxUi(String uxui) {
		this.uxui = uxui;
		FacesContext.getCurrentInstance().getExternalContext().getRequestMap().put(FacesUtil.UX_UI_KEY, uxui);
	}
	
	@PostConstruct
	private void postConstruct() {
		this.uxui = (String) FacesContext.getCurrentInstance().getExternalContext().getRequestMap().get(FacesUtil.UX_UI_KEY);
	}
	
	private String queryNameParameter;
	public String getQueryNameParameter() {
		return queryNameParameter;
	}
	public void setQueryNameParameter(String queryNameParameter) {
		this.queryNameParameter = queryNameParameter;
	}

	private String viewBinding;
	public String getViewBinding() {
		return viewBinding;
	}
	public void setViewBinding(String viewBinding) {
		this.viewBinding = viewBinding;
	}
	
	// The page title
	private String title;
	public String getTitle() {
		return title;
	}
	public void setTitle(String title) {
		this.title = title;
	}
	
	// A stack of referring urls set as we edit beans from a list grid
	private Stack<String> history = new Stack<>();
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

	// The bean currently under edit (for the view binding)
	private BeanMapAdapter<T> currentBean = null;
	public BeanMapAdapter<T> getCurrentBean() {
		return currentBean;
	}

	private long id = 0;
	public String nextId() {
		return new StringBuilder(10).append('w').append(id++).toString();
	}

	private AbstractWebContext webContext;
	public AbstractWebContext getWebContext() {
		return webContext;
	}
	public void setWebContext(AbstractWebContext webContext) {
		this.webContext = webContext;
	}
	private String webId;
	public String getWebId() {
		return webId;
	}
	public void setWebId(String webId) {
		this.webId = webId;
	}
	
	public void preRender() {
		FacesContext fc = FacesContext.getCurrentInstance();
		if (! fc.isPostback()) {
			new PreRenderAction<>(this).execute();
		}
//		else {
//System.out.println("POSTPACK = " + getWebActionParameter() + " : " + getBizModuleParameter() + " : " + getBizDocumentParameter() + " : " + queryNameParameter);
//		}
	}

	public void ok() {
System.out.println("ok");
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
System.out.println("save");
		new SaveAction<>(this, false).execute();
		new SetTitleAction(this).execute();
		
		FacesContext c = FacesContext.getCurrentInstance();
		if (c.getMessageList().isEmpty()) {
			c.addMessage(null, new FacesMessage("Saved"));
		}
	}
	
	public void cancel() {
		try {
			FacesContext.getCurrentInstance().getExternalContext().redirect(history.pop());
		}
		catch (IOException e) {
			e.printStackTrace();
		}
	}

	public void delete() {
System.out.println("delete");
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
System.out.println("zoom in to " + listBinding + '.' + bizId);
		new ZoomInAction(this, listBinding, bizId).execute();
System.out.println("view binding now " + viewBinding);
	}
	
	// for navigate-on-select in data grids
	public void navigate(SelectEvent evt) {
		@SuppressWarnings("unchecked")
		String bizId = ((BeanMapAdapter<Bean>) evt.getObject()).getBean().getBizId();
		String listBinding = ((DataTable) evt.getComponent()).getVar();
		navigate(listBinding, bizId);
	}
	
	public void add(String listBinding) {
System.out.println("add to " + listBinding);
		new AddAction(this, listBinding).execute();
System.out.println("view binding now " + viewBinding);
	}
	
	public void zoomout() {
System.out.println("zoomout");
		new ZoomOutAction(this).execute();
	}

	/**
	 * This method only removes elements from collections, it doesn't null out associations.
	 */
	public void remove() {
System.out.println("remove " + viewBinding);
		new RemoveAction(this).execute();
	}

	public void action(String actionName, String listBinding, String bizId) {
System.out.println("EXECUTE ACTION " + actionName + " for grid " + listBinding);
		new ExecuteActionAction<>(this, 
									actionName, 
									UtilImpl.processStringValue(listBinding),
									UtilImpl.processStringValue(bizId)).execute();
	}
	
	private Map<String, QueryDataModel> models = new TreeMap<>();
	public QueryDataModel getModel(String bizModule, final String queryName) {
 		String key = new StringBuilder(64).append(bizModule).append('.').append(queryName).toString();
		QueryDataModel result = models.get(key);

		if (result == null) {
			result = new QueryDataModel(bizModule, queryName);
			models.put(key, result);
		}
 		
		return result;
	}
	
	private Map<String, List<BeanMapAdapter<Bean>>> beans = new TreeMap<>();
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
System.out.println("LIST KEY = " + key);
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
 	
 	public String getResourceUrl(final String binding) {
		return new GetResourceURLAction(getBean(), binding).execute();
 	}
 	
	public List<BeanMapAdapter<Bean>> complete(String query) {
		UIComponent currentComponent = UIComponent.getCurrentComponent(FacesContext.getCurrentInstance());
		Map<String, Object> attributes = currentComponent.getAttributes();
		String completeModule = (String) attributes.get("module");
		String completeQuery = (String) attributes.get("query");
		String displayBinding = (String) attributes.get("display");
		Map<String, Object> parameters = new TreeMap<>();
		parameters.put(displayBinding, query);
System.out.println("COMPLETE = " + completeModule + "." + completeQuery + " : " + query);
		return getBeans(completeModule, completeQuery, parameters);
	}
	
	// restore the webContext and current bean etc
	public void hydrate(AbstractWebContext newWebContext)
	throws Exception {
System.out.println("hydrate");
		webContext = newWebContext;
		webId = null;
		setBean(getBean());
	}

	// remove the webContext and current bean etc leaving only the webId
	public void dehydrate() {
System.out.println("dehydrate");
		if (webContext != null) {
			webId = webContext.getWebId();
		}
		webContext = null;
		beans.clear();
		currentBean = null;
	}
	
	@SuppressWarnings("static-method")
	public User getUser() {
		ExternalContext ec = FacesContext.getCurrentInstance().getExternalContext();
		return (User) ec.getSessionMap().get(WebContext.USER_SESSION_ATTRIBUTE_NAME);
	}
	
	@SuppressWarnings("static-method")
	public void setUser(String customerName, String userName)
	throws MetaDataException {
		User user = null;
		AbstractRepository repository = AbstractRepository.get();
		ExternalContext ec = FacesContext.getCurrentInstance().getExternalContext();
		if (ec.getUserPrincipal() == null) { // not logged in
			user = repository.retrieveUser(new StringBuilder(64).append(customerName).append('/').append(userName).toString());
		}
		else {
			user = repository.retrieveUser(ec.getUserPrincipal().toString());
		}
		ec.getSessionMap().put(WebContext.USER_SESSION_ATTRIBUTE_NAME, user);

		AbstractPersistence persistence = AbstractPersistence.get();
		persistence.setUser(user);
	}
}
