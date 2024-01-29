package org.skyve.impl.sail.execution;

import org.skyve.impl.metadata.Container;
import org.skyve.impl.web.faces.pipeline.layout.NoOpLayoutBuilder;

import jakarta.faces.component.UIComponent;

public class ComponentCollectingLayoutBuilder extends NoOpLayoutBuilder {
	private ComponentCollectingComponentBuilder cccb;
	
	public ComponentCollectingLayoutBuilder(ComponentCollectingComponentBuilder cccb) {
		this.cccb = cccb;
	}
	
	@Override
	public UIComponent addToContainer(UIComponent component,
										Container viewContainer,
										UIComponent container,
										UIComponent componentToAdd,
										Integer pixelWidth,
										Integer responsiveWidth,
										Integer percentageWidth,
										Integer sm,
										Integer md,
										Integer lg,
										Integer xl,
										String invisibleConditionName) {
		cccb.addToContainer(componentToAdd);
		return component;
	}
}
