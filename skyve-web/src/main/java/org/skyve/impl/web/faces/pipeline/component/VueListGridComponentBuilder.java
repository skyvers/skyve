package org.skyve.impl.web.faces.pipeline.component;

import static org.apache.commons.lang3.StringUtils.isNotBlank;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.BiConsumer;
import java.util.function.UnaryOperator;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.primefaces.component.remotecommand.RemoteCommand;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.view.event.EventAction;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.impl.web.faces.components.VueListGridScript;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.web.WebContext;

import jakarta.el.MethodExpression;
import jakarta.faces.component.UIComponent;
import jakarta.faces.component.UIOutput;
import jakarta.faces.component.html.HtmlPanelGroup;

public class VueListGridComponentBuilder extends NoOpComponentBuilder {

    private static final Logger log = LogManager.getLogger(VueListGridComponentBuilder.class);

    @Override
    public UIComponent listGrid(UIComponent component,
						            String moduleName,
						            String modelDocumentName,
						            String modelName,
						            String uxui,
						            ListModel<Bean> model,
						            Document owningDocument,
						            String title,
						            ListGrid grid,
						            boolean aggregateQuery) {

        if (component != null) {
            return component;
        }

        HtmlPanelGroup result = (HtmlPanelGroup) a.createComponent(HtmlPanelGroup.COMPONENT_TYPE);
        result.setLayout("block");
        setId(result, null);
        String id = result.getId();
        List<UIComponent> children = result.getChildren();
        
        VueListGridScript script = (VueListGridScript) a.createComponent(VueListGridScript.COMPONENT_TYPE);
        Map<String, Object> attributes = script.getAttributes();

        BiConsumer<String, Object> put = (key, value) -> {
            if (value != null)
                attributes.put(key, value);
        };

        final Document docToUse;

        // Only set one of "query" or "model", preferring query
        String queryName = grid.getQueryName();
        if (isNotBlank(queryName)) {
            put.accept("query", queryName);
            docToUse = model.getDrivingDocument();
        } else {
            put.accept("model", modelName);
            docToUse = owningDocument;
        }

        put.accept("module", docToUse.getOwningModuleName());
        put.accept("document", docToUse.getName());
        Optional.ofNullable(managedBean)
                .map(FacesView::getWebContext)
                .map(WebContext::getWebId)
                .ifPresent(contextId -> put.accept("contextId", contextId));
        put.accept("containerId", id);
        children.add(script);
        

        String selectedIdBinding = grid.getSelectedIdBinding();
        if (selectedIdBinding != null) {
	        put.accept("selectedIdBinding", grid.getSelectedIdBinding());

	        RemoteCommand selectedCommand = (RemoteCommand) a.createComponent(RemoteCommand.COMPONENT_TYPE);
	        selectedCommand.setName(id + "_selected");

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
					attributes.put("actionName", actionName);
					if (Boolean.TRUE.toString().equals(actionAttributes.actionName) ||
							Boolean.FALSE.toString().equals(actionAttributes.actionName)) {
						attributes.put("source", modelName);
					}
					selectedCommand.setProcess((actionAttributes.process == null) ? process : actionAttributes.process);
					selectedCommand.setUpdate((actionAttributes.update == null) ? update : actionAttributes.update);
				}
			}
	        else {
	        	selectedCommand.setProcess("@this");
				selectedCommand.setUpdate("@none");
	        }

	        MethodExpression expr = createSelectedExpression(grid.getSelectedIdBinding(), actionName, modelName);
	        selectedCommand.setActionExpression(expr);
	        
	        put.accept("selectedRemoteCommand", selectedCommand.getName());
	        
	        children.add(selectedCommand);
        }

        // TODO edited
        // TODO deleted

        log.debug("Created VueListGrid component with attributes: {}", attributes);

        return result;
    }

    /**
     * @see FacesView#altSelectGridRow()
     * @param selectedIdBinding
     * @param actionName
     * @param source
     * @return
     */
    private MethodExpression createSelectedExpression(String selectedIdBinding, String actionName, String source) {

        UnaryOperator<String> addApostrophes = s -> s == null ? null : "'" + s + "'";

        String exprSting = String.format("${%s.altSelectGridRow(param.bizId, %s, %s, %s)}",
                managedBeanName,
                addApostrophes.apply(selectedIdBinding),
                addApostrophes.apply(actionName),
                addApostrophes.apply(source));

        log.debug("Creating selected expression: '{}'", exprSting);

        return ef.createMethodExpression(elc, exprSting, null,
                new Class[] { String.class, String.class, String.class, String.class });
    }

    @Override
    public UIComponent listGridContextMenu(UIComponent component, String listGridId, ListGrid listGrid) {

        UIOutput empty = new UIOutput();
        empty.setValue("");
        empty.setRendered(false);

        return empty;
    }
}
