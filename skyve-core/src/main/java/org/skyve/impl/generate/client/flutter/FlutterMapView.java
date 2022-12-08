package org.skyve.impl.generate.client.flutter;

import java.io.FileWriter;
import java.io.IOException;

import org.skyve.domain.Bean;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.view.model.list.ListModel;


public class FlutterMapView extends FlutterView {
	private ListModel<Bean> model;
	private MetaDataQueryDefinition query;
	
	public FlutterMapView(FlutterGenerator generator, String moduleName, String viewName) {
		super(generator, moduleName, viewName);
	}

	@Override
	protected void create(FileWriter fw) throws IOException {
		// TODO implement
	}
}
