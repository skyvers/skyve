package org.skyve.impl.web.faces.pipeline.component;

public class SkyveComponentBuilderChain extends ComponentBuilderChain {
	public SkyveComponentBuilderChain() {
		super(new DeviceResponsiveComponentBuilder(), new PaginatedListGridBuilder());
	}
}
