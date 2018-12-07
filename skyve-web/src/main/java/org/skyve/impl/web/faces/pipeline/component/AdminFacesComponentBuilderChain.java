package org.skyve.impl.web.faces.pipeline.component;

public class AdminFacesComponentBuilderChain extends ComponentBuilderChain {
	public AdminFacesComponentBuilderChain() {
		super(new DeviceResponsiveComponentBuilder(), new PaginatedListGridBuilder(), new AdminFacesComponentBuilder());
	}
}
