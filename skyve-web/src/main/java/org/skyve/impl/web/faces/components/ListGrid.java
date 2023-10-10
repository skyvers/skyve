package org.skyve.impl.web.faces.components;

import java.awt.MenuItem;
import java.io.IOException;
import java.util.Map;

import javax.el.ELContext;
import javax.el.ExpressionFactory;
import javax.faces.application.Application;
import javax.faces.component.FacesComponent;
import javax.faces.component.UIComponent;
import javax.faces.component.html.HtmlPanelGroup;
import javax.faces.context.FacesContext;
import javax.servlet.http.HttpServletRequest;

import org.primefaces.behavior.ajax.AjaxBehavior;
import org.primefaces.component.contextmenu.ContextMenu;
import org.primefaces.component.menuitem.UIMenuItem;
import org.primefaces.model.menu.DefaultMenuModel;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.impl.sail.mock.MockFacesContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.UserAgent;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.impl.web.faces.pipeline.component.ComponentBuilder;
import org.skyve.impl.web.faces.pipeline.component.ComponentRenderer;
import org.skyve.impl.web.faces.pipeline.component.SkyveComponentBuilderChain;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.util.Util;
import org.skyve.web.WebAction;

@FacesComponent(ListGrid.COMPONENT_TYPE)
public class ListGrid extends HtmlPanelGroup {
	@SuppressWarnings("hiding")
	public static final String COMPONENT_TYPE = "org.skyve.impl.web.faces.components.ListGrid";

	@Override
	public void encodeBegin(FacesContext context) throws IOException {
		Map<String, Object> attributes = getAttributes();

		if (Boolean.TRUE.toString().equals(attributes.get("dynamic"))) {
			getChildren().clear();
		}
		if (getChildCount() == 0) {
			final String moduleName = (String) attributes.get("module");
			final String queryName = (String) attributes.get("query");
			final String documentName = (String) attributes.get("document");
			final String modelName = (String) attributes.get("model");
			final String managedBeanName = (String) attributes.get("managedBean");
			Object createRenderedAttribute = attributes.get("createRendered");
			final Boolean createRendered = Boolean.valueOf(
					(createRenderedAttribute == null) || String.valueOf(true).equals(createRenderedAttribute) || // literal
																													// "true"
							Boolean.TRUE.equals(createRenderedAttribute)); // evaluated EL expression
			Object createDisabledAttribute = attributes.get("createDisabled");
			final boolean createDisabled = String.valueOf(true).equals(createDisabledAttribute) || // literal true
					Boolean.TRUE.equals(createDisabledAttribute); // evaluated EL Expression
			Object zoomRenderedAttribute = attributes.get("zoomRendered");
			final Boolean zoomRendered = Boolean
					.valueOf((zoomRenderedAttribute == null) || String.valueOf(true).equals(zoomRenderedAttribute) || // literal
																														// "true"
							Boolean.TRUE.equals(zoomRenderedAttribute)); // evaluated EL expression
			Object zoomDisabledAttribute = attributes.get("zoomDisabled");
			final boolean zoomDisabled = String.valueOf(true).equals(zoomDisabledAttribute) || // literal "true"
					Boolean.TRUE.equals(zoomDisabledAttribute); // evaluated EL expression
			Object filterRenderedAttribute = attributes.get("filterRendered");
			final Boolean filterRendered = Boolean.valueOf(
					(filterRenderedAttribute == null) || String.valueOf(true).equals(filterRenderedAttribute) || // literal
																													// "true"
							Boolean.TRUE.equals(filterRenderedAttribute)); // evaluated EL expression
			String classString = (String) attributes.get("componentBuilderClass");
			ComponentBuilder tempComponentBuilder = null;
			try {
				if (classString == null) {
					tempComponentBuilder = new SkyveComponentBuilderChain();
				} else {
					Class<?> type = Thread.currentThread().getContextClassLoader().loadClass(classString);
					tempComponentBuilder = (ComponentBuilder) type.getDeclaredConstructor().newInstance();
				}
			} catch (Exception e) {
				throw new IOException("Cannot instantiate the component builder " + classString, e);
			}
			final ComponentBuilder componentBuilder = tempComponentBuilder;

			new FacesAction<Void>() {
				@Override
				public Void callback() throws Exception {
					componentBuilder.setManagedBeanName(managedBeanName);
					HttpServletRequest request = (HttpServletRequest) FacesContext.getCurrentInstance()
							.getExternalContext().getRequest();
					componentBuilder.setUserAgentType(UserAgent.getType(request));
					UxUi uxui = UserAgent.getUxUi(request);

					UIComponent listGrid = ListGrid.generate(moduleName, documentName, queryName, modelName,
							uxui.getName(), createRendered, createDisabled, zoomRendered, zoomDisabled, filterRendered,
							componentBuilder);
					ListGrid.this.getChildren().add(listGrid);

					// add a context menu to the list grid
					ContextMenu contextMent = ListGrid.generateBasicContextMenu(listGrid.getId());
					ListGrid.this.getChildren().add(contextMent);
					return null;
				}
			}.execute();
		}

		if ((UtilImpl.FACES_TRACE) && (!context.isPostback()))
			Util.LOGGER.info(new ComponentRenderer(this).toString());

		super.encodeBegin(context);
	}

	public static ContextMenu generateBasicContextMenu(String listGridId) {
		FacesContext fc = (FacesContext.getCurrentInstance() != null) ? FacesContext.getCurrentInstance()
				: new MockFacesContext();
		Application a = fc.getApplication();

		// create context menu
		ContextMenu contextMenu = (ContextMenu) a.createComponent(ContextMenu.COMPONENT_TYPE);
		contextMenu.setFor(listGridId); // Set the target DataTable

		// Create menu items and add them to the context menu
		UIMenuItem zoomMenuItem = (UIMenuItem) a.createComponent(UIMenuItem.COMPONENT_TYPE);
		zoomMenuItem.setValue("Zoom in");		
		zoomMenuItem.setUrl("#");
		StringBuilder zoomInScript = new StringBuilder();
        
		zoomInScript.append("var s=PF('").append(listGridId).append("').selection[0];console.log(s);SKYVE.PF.pushHistory('");
		zoomInScript.append("?a=").append(WebAction.e.toString()).append("&m='+s.substring(s.indexOf('.') + 1)+");
		zoomInScript.append("'&d='+s.substring(s.indexOf('#') + 1, s.indexOf('.'))+");
		zoomInScript.append("'&i='+s.substring(0, s.indexOf('#')));return false;");
		zoomMenuItem.setOnclick(zoomInScript.toString());
		
		
		
		UIMenuItem zoomInNewTabMenuItem = (UIMenuItem) a.createComponent(UIMenuItem.COMPONENT_TYPE);
		zoomInNewTabMenuItem.setValue("Zoom in New Tab");
		zoomMenuItem.setUrl("#");
		StringBuilder zoomInNewTabScript = new StringBuilder();
        
		zoomInNewTabScript.append("var s=PF('").append(listGridId).append("').selection[0];console.log(s);window.open('");
		zoomInNewTabScript.append("?a=").append(WebAction.e.toString()).append("&m='+s.substring(s.indexOf('.') + 1)+");
		zoomInNewTabScript.append("'&d='+s.substring(s.indexOf('#') + 1, s.indexOf('.'))+");
		zoomInNewTabScript.append("'&i='+s.substring(0, s.indexOf('#')),'_blank');return false;");
		zoomInNewTabMenuItem.setOnclick(zoomInNewTabScript.toString());
		
		contextMenu.getChildren().add(zoomMenuItem);
		contextMenu.getChildren().add(zoomInNewTabMenuItem);

		return contextMenu;
	}
	
	public static UIComponent generate(String moduleName, String documentName, String queryName, String modelName,
			String uxui, Boolean createRendered, boolean createDisabled, Boolean zoomRendered, boolean zoomDisabled,
			Boolean filterRendered, ComponentBuilder componentBuilder) {
		ListModel<Bean> model = null;
		org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid listGrid = new org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid();
		String name = null;

		User user = CORE.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(moduleName);
		boolean aggregateQuery = false;
		if (queryName != null) {
			MetaDataQueryDefinition query = module.getMetaDataQuery(queryName);
			if (query == null) {
				query = module.getDocumentDefaultQuery(customer, queryName);
			}
			aggregateQuery = query.isAggregate();
			model = EXT.newListModel(query);
			listGrid.setQueryName(queryName);
			name = queryName;
		} else {
			Document document = module.getDocument(customer, documentName);
			model = document.getListModel(customer, modelName, true);
			listGrid.setModelName(modelName);
			name = modelName;
		}

		listGrid.setTitle(model.getDescription()); // no localisation here as listGrid.getLocalisedTitle() would be
													// called
		listGrid.setShowAdd(createRendered);
		listGrid.setDisabledConditionName(String.valueOf(createDisabled));
		listGrid.setShowZoom(zoomRendered);
		listGrid.setDisableZoomConditionName(String.valueOf(zoomDisabled));
		listGrid.setShowFilter(filterRendered);

		return componentBuilder.listGrid(null, moduleName, documentName, name, uxui, model, null, null, listGrid,
				aggregateQuery);
	}
}
