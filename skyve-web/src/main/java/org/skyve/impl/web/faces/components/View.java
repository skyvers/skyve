package org.skyve.impl.web.faces.components;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.skyve.CORE;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.RequestUxUiSelection;
import org.skyve.impl.web.UserAgent;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.impl.web.faces.pipeline.FacesViewRenderer;
import org.skyve.impl.web.faces.pipeline.component.ComponentBuilder;
import org.skyve.impl.web.faces.pipeline.component.ComponentRenderer;
import org.skyve.impl.web.faces.pipeline.component.SkyveComponentBuilderChain;
import org.skyve.impl.web.faces.pipeline.layout.LayoutBuilder;
import org.skyve.impl.web.faces.pipeline.layout.ResponsiveLayoutBuilder;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.util.Binder;
import org.skyve.util.logging.Category;
import org.skyve.web.UserAgentType;
import org.slf4j.Logger;
import org.skyve.util.logging.SkyveLoggerFactory;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.faces.component.FacesComponent;
import jakarta.faces.component.UIComponent;
import jakarta.faces.component.html.HtmlPanelGroup;
import jakarta.faces.context.FacesContext;
import jakarta.servlet.http.HttpServletRequest;

/**
 * Models a view interaction and binds it to the active Skyve web context.
 */
@FacesComponent(View.COMPONENT_TYPE)
public class View extends HtmlPanelGroup {
    private static final Logger LOGGER = SkyveLoggerFactory.getLogger(View.class);
    private static final Logger FACES_LOGGER = Category.FACES.logger();

    @SuppressWarnings("hiding")
    public static final String COMPONENT_TYPE = "org.skyve.impl.web.faces.components.View";

	/**
	 * Populates the view component tree on first render and delegates view generation to the configured builders.
	 *
	 * @param context the current Faces context
	 * @throws IOException if a configured component or layout builder cannot be created or invoked
	 */
	// The method intentionally preserves the existing request-scoped generation flow.
    @Override
    @SuppressWarnings("java:S3776") // Complexity OK
    public void encodeBegin(FacesContext context) throws IOException {
		Map<String, Object> attributes = getAttributes();

		if (Boolean.TRUE.toString().equals(attributes.get("dynamic"))) {
			getChildren().clear();
		}
		if (getChildCount() == 0) {
 			final String moduleName = (String) attributes.get("module");
			final String documentName = (String) attributes.get("document");
	    	final String managedBeanName = (String) attributes.get("managedBean");
	    	final String widgetId = (String) attributes.get("widgetId");
	    	// NB style and styleClass attributes are automatically applied
	    	final String childStyle = (String) attributes.get("childStyle");
	    	final String childStyleClass = (String) attributes.get("childStyleClass");
	    	final String process = (String) attributes.get("process");
	    	final String update = (String) attributes.get("update");
	    	String classString = (String) attributes.get(ComponentBuilder.COMPONENT_BUILDER_CLASS_KEY);
	    	ComponentBuilder tempComponentBuilder = null;
	    	try {
	    		if (classString == null) {
	    			tempComponentBuilder = new SkyveComponentBuilderChain();
	    		}
	    		else {
	    			Class<?> type = Thread.currentThread().getContextClassLoader().loadClass(classString);
	    			tempComponentBuilder = (ComponentBuilder) type.getDeclaredConstructor().newInstance();
	    		}
	    	}
	    	catch (Exception e) {
	    		throw new IOException("Cannot instantiate the component builder " + classString, e);
	    	}
	    	classString = (String) attributes.get(LayoutBuilder.LAYOUT_BUILDER_CLASS_KEY);
	    	LayoutBuilder tempLayoutBuilder = null;
	    	try {
	    		if (classString == null) {
	    			tempLayoutBuilder = new ResponsiveLayoutBuilder();
	    		}
	    		else {
	    			Class<?> type = Thread.currentThread().getContextClassLoader().loadClass(classString);
		    		tempLayoutBuilder = (LayoutBuilder) type.getDeclaredConstructor().newInstance();
	    		}
	    	}
	    	catch (Exception e) {
	    		throw new IOException("Cannot instantiate the layout builder " + classString, e);
	    	}
	    	final ComponentBuilder componentBuilder = tempComponentBuilder;
	    	final LayoutBuilder layoutBuilder = tempLayoutBuilder;

	    	if (UtilImpl.FACES_TRACE) {
	    	    FACES_LOGGER.info("View - GENERATE moduleName={} : documentName={} : managedBeanName={} : widgetId={} : process={} : update={} : managedBeanName={} : componentBuilderClass={} : layoutBuilderClass={}",
													moduleName,
													documentName,
													managedBeanName,
													widgetId,
													process,
													update,
													managedBeanName,
													componentBuilder.getClass().getName(),
													layoutBuilder.getClass().getName());
	   		}

	    	FacesContext fc = FacesContext.getCurrentInstance();
			HttpServletRequest request = (HttpServletRequest) fc.getExternalContext().getRequest();
			RequestUxUiSelection selection = UserAgent.getSelection(request);
			UxUi uxui = selection.getUxUi();
			UserAgentType userAgentType = selection.getUserAgentType();

	    	final String uxuiName = uxui.getName();
	    	final UserAgentType finalUAT = userAgentType;
	    	new FacesAction<Void>() {
				@Override
				public Void callback() throws Exception {
					List<UIComponent> views = View.generate(moduleName,
																documentName,
																widgetId,
																managedBeanName,
																uxuiName,
																finalUAT,
																process,
																update,
																componentBuilder,
																layoutBuilder);
					// Add childStyle and childStyleClass attributes if available
					for (UIComponent view : views) {
						if (childStyle != null) {
							try {
								Binder.set(view, "style", childStyle);
							}
							catch (@SuppressWarnings("unused") Exception e) {
								LOGGER.warn("Can't set the style attribute on this UIComponent - {}", view);
							}
						}
						if (childStyleClass != null) {
							try {
								Binder.set(view, "styleClass", childStyleClass);
							}
							catch (@SuppressWarnings("unused") Exception e) {
								LOGGER.warn("Can't set the styleClass attribute on this UIComponent - {}", view);
							}
						}
					}
					View.this.getChildren().addAll(views);

	                return null;
				}
			}.execute();

			if ((UtilImpl.FACES_TRACE) && (! context.isPostback())) {
				FACES_LOGGER.info("{}", new ComponentRenderer(this));
			}
		}

		super.encodeBegin(context);
    }

	/**
	 * Generates Faces view components for edit/create variants of the requested document view.
	 *
	 * @param moduleName the module name containing the document
	 * @param documentName the document name whose views are generated
	 * @param widgetId the optional widget identifier used to wire the generated faces view
	 * @param managedBeanName the managed bean name that owns the generated view
	 * @param uxui the UX/UI profile name
	 * @param userAgentType the current user-agent type
	 * @param process the PrimeFaces process expression
	 * @param update the PrimeFaces update expression
	 * @param componentBuilder the component builder used to render the view
	 * @param layoutBuilder the layout builder used to render the view
	 * @return the generated faces view components, including an optional sidebar
	 */
	@SuppressWarnings("java:S107") // Long parameter list preserves the existing framework/API contract.
	public static @Nonnull List<UIComponent> generate(@Nonnull String moduleName,
														@Nonnull String documentName,
														@Nullable String widgetId,
														@Nonnull String managedBeanName,
														@Nonnull String uxui,
														@Nonnull UserAgentType userAgentType,
														@Nullable String process,
														@Nullable String update,
														@Nonnull ComponentBuilder componentBuilder,
														@Nonnull LayoutBuilder layoutBuilder) {
		List<UIComponent> result = new ArrayList<>(2);

		User user = CORE.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(moduleName);
		Document document = module.getDocument(customer, documentName);

		componentBuilder.setManagedBeanName(managedBeanName);
		componentBuilder.setProcess(process);
		componentBuilder.setUpdate(update);
		componentBuilder.setUserAgentType(userAgentType);
		layoutBuilder.setManagedBeanName(managedBeanName);
		layoutBuilder.setProcess(process);
		layoutBuilder.setUpdate(update);
		layoutBuilder.setUserAgentType(userAgentType);

		FacesViewRenderer fvr = null;
		org.skyve.metadata.view.View view = document.getView(uxui, customer, ViewType.edit.toString());
		fvr = new FacesViewRenderer(user,
										module,
										document,
										view,
										uxui,
										widgetId,
										componentBuilder,
										layoutBuilder);
		fvr.visit();
		result.add(fvr.getFacesView());
		UIComponent sidebar = fvr.getSidebar();
		if (sidebar != null) {
			result.add(sidebar);
		}
		// Get the create view and add (so long as we didn't get the edit view back)
		view = document.getView(uxui, customer, ViewType.create.toString());
		if (ViewType.create.toString().equals(view.getName())) {
			fvr = new FacesViewRenderer(user,
					module,
					document,
					view,
					uxui,
					widgetId,
					componentBuilder,
					layoutBuilder);
			fvr.visit();
			result.add(fvr.getFacesView());
			sidebar = fvr.getSidebar();
			if (sidebar != null) {
				result.add(sidebar);
			}
		}

		return result;
	}
}
