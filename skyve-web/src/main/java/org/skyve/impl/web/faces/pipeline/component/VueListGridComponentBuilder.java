package org.skyve.impl.web.faces.pipeline.component;

import java.util.Map;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.impl.web.faces.components.VueListGrid;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.view.model.list.ListModel;

import jakarta.faces.component.UIComponent;

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

		Document drivingDocument = model.getDrivingDocument();
		attributes.put("module", drivingDocument.getOwningModuleName());
		attributes.put("document", drivingDocument.getName());
		attributes.put("query", modelName);

		log.trace("Created VueListGrid component with attributes: {}", attributes);

		return result;
	}
}
