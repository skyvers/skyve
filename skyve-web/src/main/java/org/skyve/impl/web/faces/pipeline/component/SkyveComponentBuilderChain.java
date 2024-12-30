package org.skyve.impl.web.faces.pipeline.component;

public class SkyveComponentBuilderChain extends ComponentBuilderChain {
	public SkyveComponentBuilderChain() {
		// customised
		// Add the VueListGridComponentBuilder to the chain
		super(new VueListGridComponentBuilder(),
				new DeviceResponsiveComponentBuilder(),
				new PaginatedListGridBuilder());
		// ./ customised
	}
}
