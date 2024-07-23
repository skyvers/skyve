package org.skyve.impl.web.faces.pipeline.component;

public class SkyveComponentBuilderChain extends ComponentBuilderChain {
	public SkyveComponentBuilderChain() {
		super(
				// Added the VueListGridComponentBuilder below
				new VueListGridComponentBuilder(),
				new DeviceResponsiveComponentBuilder(),
				new PaginatedListGridBuilder());
	}
}
