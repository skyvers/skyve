package org.skyve.impl.web.faces.pipeline.component;

import static org.apache.commons.lang3.StringUtils.isNotBlank;

import java.util.Map;
import java.util.Optional;
import java.util.function.BiConsumer;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.impl.web.faces.components.VueListGrid;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.web.WebContext;

import jakarta.faces.component.UIComponent;
import jakarta.faces.component.UIOutput;

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

        VueListGrid result = (VueListGrid) a.createComponent(VueListGrid.COMPONENT_TYPE);
        Map<String, Object> attributes = result.getAttributes();

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
                .ifPresent(id -> put.accept("contextId", id));

        log.debug("Created VueListGrid component with attributes: {}", attributes);

        return result;
    }

    @Override
    public UIComponent listGridContextMenu(UIComponent component, String listGridId, ListGrid listGrid) {

        UIOutput empty = new UIOutput();
        empty.setValue("");
        empty.setRendered(false);

        return empty;
    }
}
