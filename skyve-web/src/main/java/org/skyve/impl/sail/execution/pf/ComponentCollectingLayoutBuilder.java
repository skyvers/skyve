package org.skyve.impl.sail.execution.pf;

import org.skyve.impl.metadata.Container;
import org.skyve.impl.web.faces.pipeline.layout.NoOpLayoutBuilder;

import jakarta.faces.component.UIComponent;

/**
 * Builds Faces components for a specific rendering concern in the pipeline.
 */
public class ComponentCollectingLayoutBuilder extends NoOpLayoutBuilder {
	private final ComponentCollectingComponentBuilder cccb;
	
	/**
	 * Creates a layout builder that forwards container insertions to the component collector.
	 *
	 * @param cccb the collecting component builder receiving deferred component additions
	 */
	public ComponentCollectingLayoutBuilder(ComponentCollectingComponentBuilder cccb) {
		this.cccb = cccb;
	}
	
	/**
	 * Records the supplied component in the collecting builder while preserving the current layout component.
	 *
	 * <p>Side effects: appends {@code componentToAdd} to the collector's ordered component list so that
	 * PrimeFaces SAIL execution can resolve generated widgets by discovery order.
	 *
	 * @param component the current component in the layout-builder chain
	 * @param viewContainer the metadata container currently being rendered
	 * @param container the rendered container component
	 * @param componentToAdd the component to register in the collecting builder
	 * @param pixelWidth optional fixed pixel width
	 * @param responsiveWidth optional responsive width token
	 * @param percentageWidth optional percentage width
	 * @param sm optional small breakpoint width
	 * @param md optional medium breakpoint width
	 * @param lg optional large breakpoint width
	 * @param xl optional extra-large breakpoint width
	 * @param invisibleConditionName optional invisible-condition expression
	 *
	 * @return {@code component}, unchanged, because this no-op layout builder does not alter container structure
	 */
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
