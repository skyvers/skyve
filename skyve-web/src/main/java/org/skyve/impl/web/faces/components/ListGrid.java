package org.skyve.impl.web.faces.components;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.UserAgent;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.impl.web.faces.pipeline.component.ComponentBuilder;
import org.skyve.impl.web.faces.pipeline.component.ComponentBuilderChain;
import org.skyve.impl.web.faces.pipeline.component.ComponentRenderer;
import org.skyve.impl.web.faces.pipeline.component.DeviceResponsiveComponentBuilder;
import org.skyve.impl.web.faces.pipeline.component.PaginatedListGridBuilder;
import org.skyve.impl.web.faces.pipeline.component.SkyveComponentBuilderChain;
import org.skyve.impl.web.faces.pipeline.component.VueListGridComponentBuilder;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.util.Util;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.faces.component.FacesComponent;
import jakarta.faces.component.UIComponent;
import jakarta.faces.component.html.HtmlPanelGroup;
import jakarta.faces.context.FacesContext;
import jakarta.servlet.http.HttpServletRequest;

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
			final Boolean createRendered = getBooleanObjectAttribute(createRenderedAttribute);
			Object createDisabledAttribute = attributes.get("createDisabled");
			final boolean createDisabled = getBooleanAttribute(createDisabledAttribute);
			Object zoomRenderedAttribute = attributes.get("zoomRendered");
			final Boolean zoomRendered = getBooleanObjectAttribute(zoomRenderedAttribute);
			Object zoomDisabledAttribute = attributes.get("zoomDisabled");
			final boolean zoomDisabled = getBooleanAttribute(zoomDisabledAttribute);
			Object filterRenderedAttribute = attributes.get("filterRendered");
			final Boolean filterRendered = getBooleanObjectAttribute(filterRenderedAttribute);
			final String componentBuilderClassString = (String) attributes.get("componentBuilderClass");
			final ComponentBuilder componentBuilder = newComponentBuilder(componentBuilderClassString);

			new FacesAction<Void>() {
				@Override
				public Void callback() throws Exception {
					componentBuilder.setManagedBeanName(managedBeanName);
					HttpServletRequest request = (HttpServletRequest) FacesContext.getCurrentInstance().getExternalContext().getRequest();
					componentBuilder.setUserAgentType(UserAgent.getType(request));
					UxUi uxui = UserAgent.getUxUi(request);

					List<UIComponent> components = ListGrid.generate(moduleName,
																		documentName,
																		queryName,
																		modelName,
																		uxui.getName(),
																		createRendered,
																		createDisabled,
																		zoomRendered,
																		zoomDisabled,
																		filterRendered,
																		componentBuilder);
					ListGrid.this.getChildren().addAll(components);
					
					return null;
				}
			}.execute();
		}

		if ((UtilImpl.FACES_TRACE) && (! context.isPostback())) Util.LOGGER.info(new ComponentRenderer(this).toString());

		super.encodeBegin(context);
	}

	public static List<UIComponent> generate(@Nonnull String moduleName,
												@Nonnull String documentName,
												@Nullable String queryName,
												@Nullable String modelName,
												@Nonnull String uxui,
												@Nullable Boolean createRendered,
												boolean createDisabled,
												@Nullable Boolean zoomRendered,
												boolean zoomDisabled,
												@Nullable Boolean filterRendered,
												@Nonnull ComponentBuilder componentBuilder) {
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
		}
		else {
			Document document = module.getDocument(customer, documentName);
			model = document.getListModel(customer, modelName, true);
			listGrid.setModelName(modelName);
			name = modelName;
		}

		listGrid.setTitle(model.getDescription()); // no localisation here as listGrid.getLocalisedTitle() would be called
		listGrid.setShowAdd(createRendered);
		listGrid.setDisabledConditionName(String.valueOf(createDisabled));
		listGrid.setShowZoom(zoomRendered);
		listGrid.setDisableZoomConditionName(String.valueOf(zoomDisabled));
		listGrid.setShowFilter(filterRendered);

		List<UIComponent> result = new ArrayList<>(2);
		UIComponent grid = componentBuilder.listGrid(null,
														moduleName,
														documentName,
														name,
														uxui,
														model,
														null,
														null,
														listGrid,
														aggregateQuery);
		result.add(grid);
		if ((! aggregateQuery) && (! Boolean.FALSE.equals(zoomRendered))) {
			result.add(componentBuilder.listGridContextMenu(null, grid.getId(), listGrid));
		}
		return result;
	}
	
	public static ComponentBuilder newComponentBuilder(@Nullable String componentBuilderClassString) {
		try {
			ComponentBuilder result = null;
			if (componentBuilderClassString == null) {
				result = new SkyveComponentBuilderChain();
			}
			else if (componentBuilderClassString.equalsIgnoreCase("faces")) {
				result = new ComponentBuilderChain(new DeviceResponsiveComponentBuilder(),
																	new PaginatedListGridBuilder());
			}
			else if (componentBuilderClassString.equalsIgnoreCase("vue")) {
				result = new VueListGridComponentBuilder();
			}
			else {
				Class<?> type = Thread.currentThread().getContextClassLoader().loadClass(componentBuilderClassString);
				result = (ComponentBuilder) type.getDeclaredConstructor().newInstance();
			}
			return result;
		}
		catch (Exception e) {
			throw new DomainException("Cannot instantiate the component builder " + componentBuilderClassString, e);
		}
	}
	
	public static Boolean getBooleanObjectAttribute(Object renderedAttributeValue) {
		return Boolean.valueOf((renderedAttributeValue == null) || 
														String.valueOf(true).equals(renderedAttributeValue) || // literal "true"
														Boolean.TRUE.equals(renderedAttributeValue)); // evaluated EL expression
	}

	public static boolean getBooleanAttribute(Object disabledAttributeValue) {
		return String.valueOf(true).equals(disabledAttributeValue) || // literal "true"
				Boolean.TRUE.equals(disabledAttributeValue); // evaluated EL Expression
	}
}
