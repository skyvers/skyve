package org.skyve.impl.web.faces.pipeline.component;

import static java.lang.Boolean.FALSE;
import static java.lang.Boolean.TRUE;

import java.util.List;
import java.util.function.UnaryOperator;

import org.primefaces.component.remotecommand.RemoteCommand;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.view.event.EventAction;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.impl.web.faces.components.VueListGridScript;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.web.WebContext;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.el.MethodExpression;
import jakarta.faces.component.UIComponent;
import jakarta.faces.component.UIOutput;
import jakarta.faces.component.html.HtmlPanelGroup;

public class VueListGridComponentBuilder extends NoOpComponentBuilder {
	@Override
	@SuppressWarnings("java:S3776") // Complexity OK
	public @Nonnull UIComponent listGrid(@Nullable UIComponent component,
									@Nonnull String moduleName,
									@Nullable String documentName,
									@Nullable String modelName,
									@Nonnull String uxui,
									@Nonnull ListModel<Bean> model,
									@Nullable Document owningDocument,
									@Nonnull ListGrid grid,
									@Nullable String stickyHeaderAnchorSelector,
									boolean aggregateQuery) {
		if (component != null) {
			return component;
		}

		Document drivingDocument = model.getDrivingDocument();
		String drivingModuleName = drivingDocument.getOwningModuleName();
		String drivingDocumentName = drivingDocument.getName();

		User user = CORE.getUser();
		boolean canCreateDocument = user.canCreateDocument(drivingDocument);
		
		HtmlPanelGroup result = (HtmlPanelGroup) a.createComponent(HtmlPanelGroup.COMPONENT_TYPE);
		result.setLayout("block");
		setId(result, null);
		String id = result.getId();
		List<UIComponent> children = result.getChildren();

		String queryName = grid.getQueryName();
		String owningDocumentName = (documentName == null) ? drivingDocumentName : documentName;
		// For a model we need the model name defined - finalModelname
		String finalModelName = (queryName == null) ? modelName : null;
		boolean showAdd = canCreateDocument && (! aggregateQuery) && (! FALSE.equals(grid.getShowAdd()));
		boolean showZoom = (! aggregateQuery) && (! FALSE.equals(grid.getShowZoom()));
		boolean showFilter = (! aggregateQuery) && (! FALSE.equals(grid.getShowFilter()));
		boolean showSummary = (! aggregateQuery) && (! FALSE.equals(grid.getShowSummary()));
		boolean showSnap = (! FALSE.equals(grid.getShowSnap()));
		String selectedRemoteCommand = null;

		String selectedIdBinding = grid.getSelectedIdBinding();
		if (selectedIdBinding != null) {
			RemoteCommand selectedCommand = (RemoteCommand) a.createComponent(RemoteCommand.COMPONENT_TYPE);
			selectedRemoteCommand = id + "_selected";
			selectedCommand.setName(selectedRemoteCommand);

			String actionName = null;
			List<EventAction> selectedActions = grid.getSelectedActions();
			if (selectedActions != null) {
				ActionFacesAttributes actionAttributes = determineActionFacesAttributes(selectedActions);
				if (actionAttributes.actionName == null) { // when no selected action defined (collection is empty)
					selectedCommand.setProcess((actionAttributes.process == null) ? "@this" : actionAttributes.process);
					selectedCommand.setUpdate((actionAttributes.update == null) ? "@none" : actionAttributes.update);
				}
				else {
					actionName = actionAttributes.actionName;
					selectedCommand.setProcess((actionAttributes.process == null) ? process : actionAttributes.process);
					selectedCommand.setUpdate((actionAttributes.update == null) ? update : actionAttributes.update);
				}
			}
			else {
				selectedCommand.setProcess("@this");
				selectedCommand.setUpdate("@none");
			}

			MethodExpression expr = createSelectedExpression(selectedIdBinding, actionName, modelName);
			selectedCommand.setActionExpression(expr);

			children.add(selectedCommand);
		}

		// Determine the web Id
		String contextId = null;
		if (managedBean != null) {
			WebContext webContext = managedBean.getWebContext();
			if (webContext != null) {
				contextId = webContext.getWebId();
			}
		}

		// TODO edited
		// TODO deleted

		VueListGridScript script = new VueListGridScript(id,
															moduleName,
															owningDocumentName,
															drivingModuleName,
															drivingDocumentName,
															queryName,
															finalModelName,
															contextId,
															showAdd,
															showZoom,
															showFilter,
															showSummary,
															showSnap,
															selectedRemoteCommand,
															stickyHeaderAnchorSelector);
		children.add(script);

		return result;
	}

	/**
	 * @see FacesView#selectGridRow()
	 * @param selectedIdBinding
	 * @param actionName
	 * @param modelName
	 * @return	The method expression
	 */
	private @Nonnull MethodExpression createSelectedExpression(@Nonnull String selectedIdBinding, @Nullable String actionName, @Nullable String modelName) {
		// Note source should only be defined when this is a rerender
		String source = null;
		if (TRUE.toString().equals(actionName) || FALSE.toString().equals(actionName)) {
			source = modelName;
		}

		UnaryOperator<String> addApostrophes = s -> s == null ? null : "'" + s + "'";

		String exprSting = String.format("${%s.selectGridRow(param.bizId, %s, %s, %s)}",
											managedBeanName,
											addApostrophes.apply(selectedIdBinding),
											addApostrophes.apply(actionName),
											addApostrophes.apply(source));

		return ef.createMethodExpression(elc, exprSting, null,
				new Class[] { String.class, String.class, String.class, String.class });
	}

	@Override
	public @Nonnull UIComponent listGridContextMenu(@Nullable UIComponent component, @Nonnull String listGridId, @Nonnull ListGrid listGrid) {

		UIOutput empty = new UIOutput();
		empty.setValue("");
		empty.setRendered(false);

		return empty;
	}
}
